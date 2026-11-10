{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Pass where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.State
import Control.Monad (forM)
import qualified Data.Set as S
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.Utils.Compiler (Target)
import Xast.SemAnalyzer.ResolveNames (resolveNames)
import Xast.SemAnalyzer.TypeUsages (resolveTypeUsages)
import Xast.SemAnalyzer.Declare (declareStmts)
import Xast.SemAnalyzer.Imports (importAnalysis)
import Xast.SemAnalyzer.TypeCheck (typeCheck)
import Xast.SemAnalyzer.Desugar (desugarProgram)
import Xast.SemAnalyzer.Unify (zonkProgram, reportAmbiguousConstraints)

-- #### FULL ANALYSIS ####

fullAnalysis
   :: Monad m
   => ([SemWarning] -> m ())
   -> (FilePath -> String -> m ())
   -> [Program Parsed]
   -> M.Map Module PackageName  -- ^ the package of every module
   -> Target
   -> ExceptT [SemError] m AnalysisResult
fullAnalysis reportWarnings saveFile programs packageOfModule target = do
   let env = emptyEnv packageOfModule target
       symTable0 = emptySymTable

   (_, symTable1, warnings1) <- ExceptT $ pure $ runPhase env symTable0 (forM programs declareStmts)
   lift $ reportWarnings warnings1

   (_, symTable2, warnings2) <- ExceptT $ pure $ runPhase env symTable1 (importAnalysis programs)
   lift $ reportWarnings warnings2

   (programsQualified, symTable3, warnings3) <- ExceptT $ pure $ runPhase env symTable2 (forM programs resolveTypeUsages)
   lift $ reportWarnings warnings3

   (programsResolved, symTable4, warnings4) <- ExceptT $ pure $ runPhase env symTable3 (forM programsQualified resolveNames)
   lift $ reportWarnings warnings4

   (programsTyped, symTable5, warnings5) <- ExceptT $ pure $ runPhase env symTable4 (forM programsResolved typeCheck <* reportAmbiguousConstraints)
   lift $ reportWarnings warnings5

   -- Types stored on AST nodes may have been frozen before later unification
   -- resolved their type variables (e.g. a variable's own reference is typed
   -- before its use forces a substitution). Zonk every node against the final
   -- substitution map so the typed AST reflects fully-resolved types.
   let programsZonked = map (zonkProgram symTable5.tySubst) programsTyped

   (programsDesugared, symTable6, warnings6) <- ExceptT $ pure $ runPhase env symTable5 (forM programsZonked desugarProgram)

   let warningsCount = sum $ map length [warnings1, warnings2, warnings3, warnings4, warnings5, warnings6]

   return $ AnalysisResult
      { warningsCount = warningsCount
      , progs = programsDesugared
      , ctorSpans = collectCtorSpans symTable6
      , intrinsics = collectIntrinsics programsDesugared symTable6
      , functionModules = collectFunctionModules symTable6
      }

-- | All @Intrinsic declarations, by extern id.
collectIntrinsics :: [Program Desugared] -> SymTable -> M.Map ExternId IntrinsicDecl
collectIntrinsics programs symTable = M.fromList
   [ (externId, intrinsicDecl program extern)
   | program <- programs
   , StmtExtern (ExtFunc extern) <- program.stmts
   , ModIntrinsic `elem` extern.modifiers
   , Just moduleInfo <- [M.lookup program.moduleDef.name symTable.modules]
   , Just (SymbolExternFn _ externId _) <- [M.lookup extern.name moduleInfo.symbols]
   ]

collectFunctionModules :: SymTable -> M.Map FunctionId Module
collectFunctionModules symTable = M.fromList
   [ (functionId, moduleName)
   | (moduleName, moduleInfo) <- M.toList symTable.modules
   , SymbolFn _ functionId _ <- M.elems moduleInfo.symbols
   ]

intrinsicDecl :: Program Desugared -> ExternFunc -> IntrinsicDecl
intrinsicDecl program extern = IntrinsicDecl
   { name    = QualifiedIdent { moduleName = Just program.moduleDef.name, name = extern.name }
   , args    = map (.node) extern.args
   , retType = extern.retType.node
   }

-- | Number of constructors of the owning type for every constructor id
-- (`maxBound` for non-exhaustive types), used by the decision tree compiler.
collectCtorSpans :: SymTable -> M.Map ConstructorId Int
collectCtorSpans symTable = M.fromList
   [ (constructorId, spanOf moduleInfo signature.owner.name)
   | moduleInfo <- M.elems symTable.modules
   , symbol <- M.elems moduleInfo.symbols
   , (constructorId, signature) <- case symbol of
         SymbolCtor _ ctorId ctorSignature       -> [(ctorId, ctorSignature)]
         SymbolTypeCtor _ _ ctorId ctorSignature -> [(ctorId, ctorSignature)]
         _                                       -> []
   ]
   where
      spanOf moduleInfo owner = case M.lookup owner moduleInfo.symbols of
         Just (SymbolType _ typeSignature)         -> typeSignatureSpan typeSignature
         Just (SymbolTypeCtor _ typeSignature _ _) -> typeSignatureSpan typeSignature
         _                                         -> maxBound
      typeSignatureSpan typeSignature
         | typeSignature.nonExhaustive = maxBound
         | otherwise                   = S.size typeSignature.ctors
