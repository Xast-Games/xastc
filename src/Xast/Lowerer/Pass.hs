{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.Pass where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, runLowerer)
import Xast.Lowerer.Types
import Xast.Lowerer.Mono (drainTypes, requestTagTypes, substituteTypeParams, mangle, lookupFunctionTemplate)
import Control.Monad.State (gets, modify)
import Control.Monad (forM)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Identity (Identity(runIdentity))
import Xast.Lowerer.Definition (lowerFunction, lowerSystem)
import Xast.SemAnalyzer.Types (AnalysisResult (..))

lowerPrograms :: AnalysisResult -> ([Kira], [KirTypeDef], [KirIntrinsicDef])
lowerPrograms analysis =
   let lowered = do
         kira <- forM analysis.progs lowerProgram
         requestTagTypes
         -- functions before types: function instances request types
         drainFunctions
         drainTypes
         pure kira
       ((irs, typeDefs), finalState) = runIdentity $
         runLowerer (newLowerState analysis) lowered
       withInstances = map (addInstances finalState.loweredInsts) irs
   in (withInstances, typeDefs, M.elems finalState.intrinsicInsts)

-- | Lowers every requested function instance, and the ones they request.
drainFunctions :: Lowerer ()
drainFunctions = gets (.funcQueue) >>= \case
   [] -> pure ()
   request : rest -> do
      modify $ \state -> state { funcQueue = rest }
      template <- lookupFunctionTemplate request.instance'.name

      let impl = fmap (substituteInfo request.typeParams) template.impl
      function <- lowerFunction (KirName (mangle request.instance')) impl
      recordInstance template.moduleName function
      drainFunctions

recordInstance :: Module -> KirFunction -> Lowerer ()
recordInstance module' function = modify $ \state -> state
   { loweredInsts = state.loweredInsts ++ [(module', function)] }

substituteInfo :: M.Map Type Type -> Desugared -> Desugared
substituteInfo typeParams info = DesugaredInfo
   { ty  = substituteTypeParams typeParams info.ty
   , res = info.res
   }

-- | Puts function instances into the module that declares their function,
--   e.g. @Std_Option_isSome_Std_Num_Int@ into @Std.Option@, even if @Main@ asked for it.
addInstances :: [(Module, KirFunction)] -> Kira -> Kira
addInstances instances kira =
   let own = [ function | (module', function) <- instances, module' == kira.moduleName ]
   in kira { functions = kira.functions ++ own }

lowerProgram :: Program Desugared -> Lowerer Kira
lowerProgram program = do
   let systemImpls = [systemImpl | StmtSystem (SysImpl systemImpl) <- program.stmts]
   let genericFunctions = genericFunctionNames program
   let funcImpls   =
         [ funcImpl
         | StmtFunc (FnImpl funcImpl) <- program.stmts
         , funcImpl.name `S.notMember` genericFunctions
         ]

   systems   <- forM systemImpls (lowerSystem program.moduleDef.name)
   functions <- forM funcImpls $ \impl ->
      let name = KirName (namespacedName program.moduleDef.name impl.name)
      in lowerFunction name impl

   return $ Kira
      program.moduleDef.name
      systems
      functions

-- | Functions with a generic, @number@ or @concatenative@ in their signature.
--   Skipped until function monomorphization lowers them per use.
genericFunctionNames :: Program Desugared -> S.Set FunctionName
genericFunctionNames program = S.fromList
   [ funcDef.name
   | StmtFunc (FnDef funcDef) <- program.stmts
   , any (isPolymorphic . (.ty) . (.node)) (funcDef.args ++ funcDef.results)
   ]

isPolymorphic :: Type -> Bool
isPolymorphic = \case
   TyGnr _          -> True
   TyInt _          -> True
   TyApp func arg   -> isPolymorphic func || isPolymorphic arg
   TyTuple elements -> any isPolymorphic elements
   TyFn params results -> any isPolymorphic (functionTypes params results)
   _                -> False
