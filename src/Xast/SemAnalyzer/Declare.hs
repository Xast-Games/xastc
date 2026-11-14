{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Declare where

import Control.Monad.State
import Control.Monad (forM_, unless, when)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Unify (freshConstructorId, freshExternId, freshFunctionId, collectIntrinsics)
import Xast.SemAnalyzer.TypeUsages (payloadTypes)
import Control.Monad.Reader (asks)
import Xast.Intrinsics (intrinsicArities)

-- #### DECLARE STATEMENTS ####

qualify :: Ident -> SemAnalyzer QualifiedName
qualify ident = do
   module_ <- gets (.currentModule)
   return (QualifiedName module_ ident)

enterModule :: ModuleDef -> SemAnalyzer ()
enterModule (ModuleDef _ moduleName _) = do
   symTable <- get

   case M.lookup moduleName symTable.modules of
      Just _ -> pure ()
      Nothing ->
         put symTable
            { modules = M.insert moduleName emptyModuleInfo symTable.modules
            }

declareStmts :: Program Parsed -> SemAnalyzer ()
declareStmts (Program moduleDef@(ModuleDef _ moduleName _) _ stmts _) = do
   enterModule moduleDef
   modify $ \symTable -> symTable { currentModule = moduleName }
   forM_ stmts declareStmt

   inStd <- isStdModule moduleName
   unless inStd $
      forM_ stmts checkNoIntrinsicTypes

declareStmt :: Stmt Parsed -> SemAnalyzer ()
declareStmt = \case
   StmtFunc (FnDef funcDef@(FuncDef _ _ ident _ _)) ->
      declareFn ident funcDef

   StmtTypeDef typeDef@(TypeDef _ _ ident _ _) ->
      declareType ident typeDef

   StmtExtern (ExtFunc externFunc@(ExternFunc _ _ ident _ _)) -> do
      declareExternFn ident externFunc
      checkIntrinsic externFunc

   StmtExtern (ExtType externType@(ExternType _ _ ident _)) ->
      declareExternType ident externType

   StmtSystem (SysDef systemDef@(SystemDef _ _ ident _ _ _)) ->
      declareSystem ident systemDef

   _ -> return ()

-- | Builds the error reported when a name is declared twice.
type MakeRedeclarationError = Ident -> Location -> Location -> RedeclarationError

declareSymbol :: Ident -> SymbolInfo -> MakeRedeclarationError -> SemAnalyzer ()
declareSymbol ident symbol redeclarationError = do
   QualifiedName moduleName _ <- qualify ident
   symTable <- get
   let moduleInfo = M.findWithDefault emptyModuleInfo moduleName symTable.modules

   case M.lookup ident moduleInfo.symbols of
      Just oldSymbol ->
         errSem (redeclarationError ident (symbolLoc oldSymbol) (symbolLoc symbol))

      Nothing ->
         put symTable
            { modules =
                  M.insert moduleName
                     moduleInfo { symbols = M.insert ident symbol moduleInfo.symbols }
                     symTable.modules
            }

declareFn :: Ident -> FuncDef -> SemAnalyzer ()
declareFn ident funcDef@(FuncDef loc _ fnIdent fnArgs _) = do
   -- Report function gayness
   let argsCount = length fnArgs
   when (argsCount > 6) $
      warnSem (SWFunctionGayness loc fnIdent argsCount)
   warnMisplacedPrime loc fnIdent

   functionId <- freshFunctionId
   declareSymbol ident (SymbolFn loc functionId (funcSig funcDef)) SEFnRedeclaration

declareType :: Ident -> TypeDef -> SemAnalyzer ()
declareType ident typeDef@(TypeDef loc modifiers _ generics ctors) = do
   checkTagType typeDef

   QualifiedName currentModule _ <- qualify ident
   let owner = QualifiedIdent { moduleName = Just currentModule, name = ident }
       ctorNames = S.fromList [ctor.name | ctor <- ctors]
       typeSig = TypeSig ctorNames generics (ModNonExhaustive `elem` modifiers) (ownershipOf modifiers)

   case [ctor | ctor <- ctors, ctor.name == ident] of
      [selfCtor] -> do
         let (fieldNames, fieldTypes) = payloadFields selfCtor.payload
         constructorId <- freshConstructorId
         declareSymbol ident
               (SymbolTypeCtor loc typeSig constructorId (CtorSig owner generics fieldNames fieldTypes))
               SETypeRedeclaration
      _ ->
         declareSymbol ident (SymbolType loc typeSig) SETypeRedeclaration

   forM_ ctors $ \ctor ->
      unless (ctor.name == ident) $ do
         let (fieldNames, fieldTypes) = payloadFields ctor.payload
         constructorId <- freshConstructorId
         declareSymbol ctor.name
               (SymbolCtor ctor.location constructorId (CtorSig owner generics fieldNames fieldTypes))
               SECtorRedeclaration
   where
      payloadFields = \case
         PUnit -> (Nothing, [])
         (PTuple types) -> (Nothing, map (.node) types)
         (PRecord fields) -> (Just (map (.name) fields), map ((.node) . (.ty)) fields)

-- | A @Tag type is a plain marker component, so it must look like @type X = X;@.
checkTagType :: TypeDef -> SemAnalyzer ()
checkTagType typeDef =
   let isTag = ModTag `elem` typeDef.modifiers
       invalidTag = SEInvalidTagType typeDef.location typeDef.name
   in when (isTag && not (isPlainMarker typeDef)) $
         errSem invalidTag

-- | @'@ is only for the unique variant of a function, as its last character: @map'@.
warnMisplacedPrime :: Location -> Ident -> SemAnalyzer ()
warnMisplacedPrime location name =
   when (hasMisplacedPrime name) $
      warnSem (SWMisplacedPrime location name)

-- | E.g. @map'wrong@ or @map''@, but not @map'@.
hasMisplacedPrime :: Ident -> Bool
hasMisplacedPrime name =
   let withoutLast = T.dropEnd 1 name.inner
   in T.any (== '\'') withoutLast

-- | An @Intrinsic declaration must be in Std and match a compiler definition,
--   including arity.
checkIntrinsic :: ExternFunc -> SemAnalyzer ()
checkIntrinsic extern = do
   moduleName <- gets (.currentModule)
   inStd <- isStdModule moduleName
   let isIntrinsic = ModIntrinsic `elem` extern.modifiers
   when (isIntrinsic && not inStd) $
      errSem (SEIntrinsicOutsideStd extern.location extern.name)
   when (isIntrinsic && inStd) $
      checkIntrinsicDefinition extern

-- | The compiler defines the intrinsic, with as many parameters.
checkIntrinsicDefinition :: ExternFunc -> SemAnalyzer ()
checkIntrinsicDefinition extern =
   let declaredArity = length extern.args
       arityMismatch expectedArity = SEIntrinsicArityMismatch
         { location      = extern.location
         , name          = extern.name
         , expectedArity = expectedArity
         , actualArity   = declaredArity
         }
   in case M.lookup extern.name intrinsicArities of
      Nothing -> errSem (SEUnknownIntrinsic extern.location extern.name)
      Just expectedArity -> unless (expectedArity == declaredArity) $
         errSem (arityMismatch expectedArity)

isStdModule :: Module -> SemAnalyzer Bool
isStdModule moduleName = do
   packages <- asks (.modulePackages)
   pure (M.lookup moduleName packages == Just stdPackageName)

-- | Outside Std, a declaration may not use @number@ or @concatenative@.
checkNoIntrinsicTypes :: Stmt Parsed -> SemAnalyzer ()
checkNoIntrinsicTypes stmt = forM_ (declaredTypes stmt) $ \(Located location type') ->
   forM_ (collectIntrinsics type') $ \kind ->
      errSem (SEIntrinsicTypeOutsideStd location kind)

-- | The types written in a declaration.
declaredTypes :: Stmt Parsed -> [Located Type]
declaredTypes = \case
   StmtFunc (FnDef funcDef)      -> map (fmap (.ty)) (funcDef.args ++ funcDef.results)
   StmtExtern (ExtFunc extern)   -> map (fmap (.ty)) (extern.args ++ extern.results)
   StmtTypeDef typeDef           -> concatMap (payloadTypes . (.payload)) typeDef.ctors
   StmtSystem (SysDef systemDef) -> systemDef.retType : concatMap (.types) systemDef.entities
   _                             -> []

-- | Whether a type has no type parameters and a single constructor without fields.
isPlainMarker :: TypeDef -> Bool
isPlainMarker typeDef = case typeDef.ctors of
   [ctor] -> null typeDef.generics && hasNoPayload ctor
   _      -> False

hasNoPayload :: Ctor -> Bool
hasNoPayload ctor = case ctor.payload of
   PUnit -> True
   _     -> False

declareExternFn :: Ident -> ExternFunc -> SemAnalyzer ()
declareExternFn ident externFunc@(ExternFunc loc _ _ _ _) = do
   warnMisplacedPrime loc ident
   externId <- freshExternId
   declareSymbol ident (SymbolExternFn loc externId (externFuncSig externFunc)) SEExternFnRedeclaration

declareExternType :: Ident -> ExternType -> SemAnalyzer ()
declareExternType ident (ExternType loc modifiers _ _) =
   declareSymbol ident (SymbolExternType loc (ownershipOf modifiers)) SEExternTypeRedeclaration

declareSystem :: Ident -> SystemDef -> SemAnalyzer ()
declareSystem ident systemDef@(SystemDef loc _ _ _ _ _) =
   declareSymbol ident (SymbolSystem loc (systemSig systemDef)) SESystemRedeclaration
