{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.EraseZero where

import Data.List (mapAccumL, partition)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Xast.Utils.List as L
import Xast.Lowerer.Types
import Xast.Utils.Generic ((|>), unreachableWith)
import Xast.AST

eraseZeroSized :: [Kira] -> [KirTypeDef] -> ([Kira], [KirTypeDef])
eraseZeroSized kira typeDefs =
   let context = newErasureContext kira typeDefs
       zstNames = context.zstTypes
       realTypeDefs = typeDefs |> filter (not . isZeroSizedTypeDef zstNames)
       erasedTypeDefs =
         L.forEach realTypeDefs $ \typeDef ->
            let realCtors =
                  L.forEach typeDef.ctors $ \ctor ->
                     let realFields = ctor.fields
                           |> filter (\field -> field.ty
                              |> not . isZeroSized zstNames
                           )
                     in ctor { fields = realFields }
            in KirTypeDef
               { name = typeDef.name
               , ctors = realCtors
               }
       erasedKira = L.forEach kira (eraseProgram context)

   in (erasedKira, erasedTypeDefs)

-- | What the erasure of one function did to its signature.
data ErasedSignature = ErasedSignature
   { keepParams   :: [Bool]  -- ^ one flag per original parameter, False = dropped
   , erasedReturn :: Bool    -- ^ the return type was zero-sized and became unit
   }
   deriving Show

-- | Facts about the whole program, computed once and shared by every function.
data ErasureContext = ErasureContext
   { zstTypes   :: S.Set KirName                  -- ^ zero-sized types
   , zstCtors   :: S.Set CtorName                 -- ^ constructors of zero-sized types
   , signatures :: M.Map KirName ErasedSignature  -- ^ erased signature of every function
   }

-- | State while erasing one function body.
data ErasureEnv = ErasureEnv
   { context      :: ErasureContext   -- ^ shared by every function
   , erasedVars   :: S.Set KirName    -- ^ params and call/field results that were dropped
   , erasedLocals :: S.Set KirLocalId -- ^ locals that were stored with a zero-sized type
   , erasedBindings :: S.Set KirBindingId -- ^ system bindings of zero-sized (tag) components
   }

-- | The program-wide facts, including the erased signature of every function.
newErasureContext
   :: [Kira]        -- ^ every lowered program
   -> [KirTypeDef]  -- ^ the original type definitions, before erasure
   -> ErasureContext
newErasureContext programs typeDefs =
   let zstNames = zstSet typeDefs
   in ErasureContext
      { zstTypes   = zstNames
      , zstCtors   = zeroSizedCtors typeDefs zstNames
      , signatures = buildSignatures zstNames programs
      }

-- | Start of a function body: nothing erased yet.
newErasureEnv :: ErasureContext -> ErasureEnv
newErasureEnv sharedContext = ErasureEnv
   { context      = sharedContext
   , erasedVars   = S.empty
   , erasedLocals = S.empty
   , erasedBindings = S.empty
   }

-- | The erased signature of every function, by function name.
--   It must be complete before any body is rewritten, because a call has to
--   know which parameters of its callee were dropped.
buildSignatures
   :: S.Set KirName  -- ^ zero-sized types
   -> [Kira]
   -> M.Map KirName ErasedSignature
buildSignatures zstNames programs =
   let entries = 
         L.forFlat programs $ \prog ->
            L.forEach prog.functions $ \function ->
               (function.name, erasedSignatureOf zstNames function)
   in M.fromList entries

erasedSignatureOf :: S.Set KirName -> KirFunction -> ErasedSignature
erasedSignatureOf zstNames function =
   ErasedSignature
      { keepParams   = function.params |> map (isRealParam zstNames)
      , erasedReturn = isZeroSized zstNames function.retTy
      }

-- | A parameter that still exists after erasure.
isRealParam :: S.Set KirName -> KirParam -> Bool
isRealParam zstNames param = (not . isZeroSized zstNames) param.ty

-- | Drops zero-sized parameters and turns a zero-sized return type into unit.
--   The returned environment starts with the dropped parameters marked erased,
--   ready for rewriting the body.
eraseFunctionHeader
   :: ErasureContext
   -> KirFunction
   -> (KirFunction, ErasureEnv)
eraseFunctionHeader context function =
   let signature = lookupSignature context function.name
       (keptParams, droppedParams) = splitParams signature.keepParams function.params
       droppedNames = droppedParams 
         |> map (.name) 
         |> S.fromList
       newReturnType = if signature.erasedReturn then TyTuple [] else function.retTy
       newFunction = KirFunction
          { name   = function.name
          , params = keptParams
          , retTy  = newReturnType
          , body   = function.body
          }
       env = (newErasureEnv context) { erasedVars = droppedNames }
   in (newFunction, env)

-- | The erased signature of a function that is known to exist in the program.
lookupSignature :: ErasureContext -> KirName -> ErasedSignature
lookupSignature context name = case M.lookup name context.signatures of
   Just signature -> signature
   Nothing        -> unreachableWith $ "no erased signature for function " <> show name

-- | Splits parameters by their keep flag, into (kept, dropped).
splitParams :: [Bool] -> [KirParam] -> ([KirParam], [KirParam])
splitParams keepFlags params =
   let flagged = zip keepFlags params
       (kept, dropped) = partition fst flagged
   in (map snd kept, map snd dropped)

-- | Erases one lowered program: every function and every system.
eraseProgram :: ErasureContext -> Kira -> Kira
eraseProgram context program = Kira
   { moduleName = program.moduleName
   , systems    = L.forEach program.systems (eraseSystem context)
   , functions  = L.forEach program.functions (eraseFunction context)
   }

-- | Erases the header of a function, then its body and its return.
eraseFunction :: ErasureContext -> KirFunction -> KirFunction
eraseFunction context function =
   let (headerFunction, env) = eraseFunctionHeader context function
       signature = lookupSignature context function.name
       (_, bodyInstructs) = eraseInstructs env function.body.instructs
       bodyTerm = eraseTerm signature function.body.term
   in KirFunction
      { name   = headerFunction.name
      , params = headerFunction.params
      , retTy  = headerFunction.retTy
      , body   = KirBlock { instructs = bodyInstructs, term = bodyTerm }
      }

-- | A function with an erased return type returns nothing.
eraseTerm :: ErasedSignature -> KirTerm -> KirTerm
eraseTerm signature term
   | signature.erasedReturn = KirReturn Nothing
   | otherwise              = term

-- | Erases a system body. Every binding stays, so the query and the field
--   indices keep their positions, but a zero-sized (tag) binding gets the unit
--   type and no longer has data to read.
eraseSystem :: ErasureContext -> KirSystem -> KirSystem
eraseSystem context system =
   let zstNames = context.zstTypes
       erasedIds = erasedBindingIds zstNames system.bindings
       env = (newErasureEnv context) { erasedBindings = erasedIds }
       (_, bodyInstructs) = eraseInstructs env system.body.instructs
   in KirSystem
      { name     = system.name
      , bindings = L.forEach system.bindings (eraseBinding zstNames)
      , body     = KirBlock { instructs = bodyInstructs, term = system.body.term }
      }

-- | Ids of the bindings whose component is zero-sized, i.e. a tag.
erasedBindingIds :: S.Set KirName -> [KirBinding] -> S.Set KirBindingId
erasedBindingIds zstNames bindings = zip [0 ..] bindings
   |> filter (\(_, binding) -> isZeroSized zstNames binding.bindType)
   |> map (KirBindingId . fst)
   |> S.fromList

eraseBinding :: S.Set KirName -> KirBinding -> KirBinding
eraseBinding zstNames binding
   | isZeroSized zstNames binding.bindType = KirBinding
      { bindType   = TyTuple []
      , bindSrc    = binding.bindSrc
      , bindAccess = binding.bindAccess
      }
   | otherwise = binding

-- | Erases a list of instructions in order, threading the environment:
--   what one instruction erases is known to the instructions after it.
eraseInstructs :: ErasureEnv -> [KirInstruct] -> (ErasureEnv, [KirInstruct])
eraseInstructs env instructs =
   let (finalEnv, erasedGroups) = mapAccumL eraseInstruct env instructs
   in (finalEnv, concat erasedGroups)

eraseInstruct :: ErasureEnv -> KirInstruct -> (ErasureEnv, [KirInstruct])
eraseInstruct env = \case
   KirCall resultType callee args out -> eraseCall env resultType callee args out
   KirAssign binding value            -> eraseAssign env binding value
   KirStore storedType dest value     -> eraseStore env storedType dest value
   KirField fieldType src ctor fieldName dst -> eraseField env fieldType src ctor fieldName dst
   KirMatch matchType scrutinee arms fallback dest exhaustive ->
      eraseMatch env matchType scrutinee arms fallback dest exhaustive

-- | Drops the arguments of erased parameters. A call whose result is
--   zero-sized stays as a statement without a result.
eraseCall
   :: ErasureEnv
   -> Type
   -> KirName
   -> [KirValue]
   -> Maybe KirName
   -> (ErasureEnv, [KirInstruct])
eraseCall env resultType callee args out =
   let keptArgs = keepCallArgs env callee args
       resultErased = isZeroSized env.context.zstTypes resultType
       newResultType = if resultErased then TyTuple [] else resultType
       newOut = if resultErased then Nothing else out
       newEnv = case out of
          Just resultName | resultErased -> markVarErased env resultName
          _                              -> env
       call = KirCall
          { ty     = newResultType
          , callee = callee
          , args   = keptArgs
          , out    = newOut
          }
   in (newEnv, [call])

-- | The arguments that still exist. A function of the program says which of its
--   parameters were dropped; an extern function is judged by the values alone.
keepCallArgs :: ErasureEnv -> KirName -> [KirValue] -> [KirValue]
keepCallArgs env callee args = case M.lookup callee env.context.signatures of
   Just signature -> zip signature.keepParams args
      |> filter fst
      |> map snd
   Nothing -> args |> filter (not . isErasedValue env)

-- | Writing an erased value, or writing to an erased binding, does nothing.
eraseAssign :: ErasureEnv -> KirBindingId -> KirValue -> (ErasureEnv, [KirInstruct])
eraseAssign env binding value
   | binding `S.member` env.erasedBindings = (env, [])
   | isErasedValue env value               = (env, [])
   | otherwise                             = (env, [KirAssign binding value])

-- | Storing a zero-sized value is dropped. A local that was to hold it becomes
--   erased, so later uses of that local disappear too.
eraseStore :: ErasureEnv -> Type -> KirDest -> KirValue -> (ErasureEnv, [KirInstruct])
eraseStore env storedType dest value
   | isZeroSized env.context.zstTypes storedType = (markDestErased env dest, [])
   | otherwise = (env, [KirStore storedType dest value])

-- | Reading a zero-sized field is dropped, and what it would have bound is erased.
eraseField
   :: ErasureEnv
   -> Type
   -> KirValue
   -> CtorName
   -> FieldName
   -> KirName
   -> (ErasureEnv, [KirInstruct])
eraseField env fieldType src ctor fieldName dst
   | isZeroSized env.context.zstTypes fieldType = (markVarErased env dst, [])
   | otherwise = (env, [KirField fieldType src ctor fieldName dst])

-- | A match with a zero-sized result keeps its branches, which may still do
--   things, but stops producing a value. A match on a zero-sized scrutinee has
--   only one possible branch, so that branch is taken directly.
eraseMatch
   :: ErasureEnv
   -> Type
   -> KirValue
   -> [KirMatchArm]
   -> Maybe KirBranch
   -> KirDest
   -> Bool
   -> (ErasureEnv, [KirInstruct])
eraseMatch env matchType scrutinee arms fallback dest exhaustive =
   let resultErased = isZeroSized env.context.zstTypes matchType
       envAfter = if resultErased then markDestErased env dest else env
   in if isErasedValue env scrutinee
         then (envAfter, inlineOnlyBranch env resultErased matchType dest arms fallback)
         else
            let newArms = L.forEach arms $ \arm -> KirMatchArm
                  { tag    = arm.tag
                  , branch = eraseBranch env resultErased arm.branch
                  }
                newFallback = fmap (eraseBranch env resultErased) fallback
                newMatchType = if resultErased then TyTuple [] else matchType
                match = KirMatch
                  { ty         = newMatchType
                  , scrutinee  = scrutinee
                  , arms       = newArms
                  , fallback   = newFallback
                  , dest       = dest
                  , exhaustive = exhaustive
                  }
            in (envAfter, [match])

-- | The match is on a value that no longer exists, so there was only one
--   constructor to match: run its branch, then store its value if it has one.
inlineOnlyBranch
   :: ErasureEnv
   -> Bool        -- ^ the match result is zero-sized
   -> Type
   -> KirDest
   -> [KirMatchArm]
   -> Maybe KirBranch
   -> [KirInstruct]
inlineOnlyBranch env resultErased matchType dest arms fallback =
   let chosenBranch = case arms of
          (arm : _) -> Just arm.branch
          []        -> fallback
   in case chosenBranch of
         Nothing -> []
         Just branch ->
            let (_, branchInstructs) = eraseInstructs env branch.instructs
                storeResult = [KirStore matchType dest branch.value | not resultErased]
            in branchInstructs ++ storeResult

-- | Erases the inside of a branch. What is defined in a branch is not visible
--   outside it, so its environment is not kept.
eraseBranch :: ErasureEnv -> Bool -> KirBranch -> KirBranch
eraseBranch env resultErased branch =
   let (_, branchInstructs) = eraseInstructs env branch.instructs
       branchValue = if resultErased then KirUnit else branch.value
   in KirBranch { instructs = branchInstructs, value = branchValue }

markVarErased :: ErasureEnv -> KirName -> ErasureEnv
markVarErased env name = env { erasedVars = S.insert name env.erasedVars }

markDestErased :: ErasureEnv -> KirDest -> ErasureEnv
markDestErased env = \case
   DestLocal localId -> env { erasedLocals = S.insert localId env.erasedLocals }
   DestBinding _     -> env

-- | Constructors of zero-sized types, e.g. @X@ of @type X = X@.
zeroSizedCtors :: [KirTypeDef] -> S.Set KirName -> S.Set CtorName
zeroSizedCtors typeDefs zstNames =
   let zeroSizedDefs = typeDefs |> filter (isZeroSizedTypeDef zstNames)
       ctorNames = L.forFlat zeroSizedDefs $ \typeDef ->
         L.forEach typeDef.ctors (.name)
   in S.fromList ctorNames

isZeroSizedTypeDef :: S.Set KirName -> KirTypeDef -> Bool
isZeroSizedTypeDef zstNames typeDef = typeDef.name `S.member` zstNames

-- | Whether a value no longer exists after erasure.
--   Values carry no types, so this goes by what the environment has seen erased.
isErasedValue :: ErasureEnv -> KirValue -> Bool
isErasedValue env = \case
   KirConst (TagCtor ctor) -> ctor `S.member` env.context.zstCtors
   KirConst (TagLit _)     -> False
   KirVar name             -> name `S.member` env.erasedVars
   KirLocalRef localId     -> localId `S.member` env.erasedLocals
   KirBindingRef bindingId -> bindingId `S.member` env.erasedBindings
   KirUnit                 -> True

isZeroSized :: S.Set KirName -> Type -> Bool
isZeroSized zstNames = \case
   TyCon name -> KirName name.inner `S.member` zstNames
   TyTuple [] -> True
   TyFn _ _   -> False
   other      -> unreachableWith $ "invalid type in EraseZero phase: " <> show other

zstSet :: [KirTypeDef] -> S.Set KirName
zstSet typeDefs =
   let typeDefMap = L.intoMap typeDefs (.name)
       candidates = typeDefs
         |> filter (\td ->
            let isSingleCtor = L.isSingleton td.ctors
                ctorsSuitable = td.ctors
                  |> filter (\ctor -> any (unhandyField typeDefMap . (.ty)) ctor.fields)
                  |> null
            in isSingleCtor && ctorsSuitable
         )

       waitingOn =
         L.forEach candidates $ \typeDef ->
            let count = (head typeDef.ctors).fields
                  |> filter (notUnit . (.ty))
                  |> length
            in (typeDef.name, count)

       pairs =
         L.forFlat candidates $ \container ->
            L.forMaybe (head container.ctors).fields $ \field ->
               fmap (, container.name) (fieldTypeName field.ty)

       dependents =
         M.fromListWith (++)
            [ (typeName, [containerName]) | (typeName, containerName) <- pairs ]

       queue = waitingOn
         |> filter (\(_, count) -> count == 0)
         |> map fst

   in goZst queue dependents (M.fromList waitingOn) S.empty

goZst
   :: [KirName]               -- ^ queue of ZST being processed
   -> M.Map KirName [KirName] -- ^ map of types to dependent types, waiting on
   -> M.Map KirName Int       -- ^ map of types to dependent types count 
   -> S.Set KirName           -- ^ initial set of ZST
   -> S.Set KirName           -- ^ final set of ZST
goZst [] _ _ final = final
goZst (zst:queueRest) deps waitingOn initial =
   let newSet = S.insert zst initial
       dependentTypes = M.findWithDefault [] zst deps
       newWaitingOn = L.updateKeys dependentTypes (subtract 1) waitingOn
       dependentDeduped = L.dedup dependentTypes
       newZsts = flip filter dependentDeduped $ \name ->
         let count = newWaitingOn M.! name
         in count == 0
       newQueue = newZsts ++ queueRest
   in goZst newQueue deps newWaitingOn newSet

notUnit :: Type -> Bool
notUnit (TyTuple []) = False
notUnit _ = True

fieldTypeName :: Type -> Maybe KirName
fieldTypeName = \case
   TyCon name -> Just (KirName name.inner)
   TyFn _ _   -> Nothing
   TyTuple [] -> Nothing
   other      -> unreachableWith $ "invalid type in EraseZero phase: " <> show other

unhandyField :: M.Map KirName KirTypeDef -> Type -> Bool
unhandyField typeDefs ty = case fieldTypeName ty of
   Just name ->
      let foundType = M.lookup (KirName name.text) typeDefs
      in maybe True isMultiCtor foundType -- maybe True to support builtins

   Nothing -> False

isMultiCtor :: KirTypeDef -> Bool
isMultiCtor = not . L.isSingleton . (.ctors)