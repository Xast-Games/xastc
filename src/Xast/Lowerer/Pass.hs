{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.Pass where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, runLowerer)
import Xast.Lowerer.Types
import Xast.Lowerer.Mono (drainTypes, requestTagTypes)
import Control.Monad (forM)
import qualified Data.Map as M
import Control.Monad.Identity (Identity(runIdentity))
import Xast.Lowerer.Definition (lowerFunction, lowerSystem)

lowerPrograms
   :: M.Map ConstructorId Int
   -> M.Map ExternId IntrinsicDecl  -- ^ @Intrinsic declarations
   -> [Program Desugared]
   -> ([Kira], [KirTypeDef], [KirIntrinsicDef])
lowerPrograms spans intrinsics programs =
   let lowered = forM programs lowerProgram <* requestTagTypes <* drainTypes
       ((irs, typeDefs), finalState) = runIdentity $
         runLowerer (newLowerState spans intrinsics programs) lowered
   in (irs, typeDefs, M.elems finalState.intrinsicInsts)

lowerProgram :: Program Desugared -> Lowerer Kira
lowerProgram program = do
   let systemImpls = [systemImpl | StmtSystem (SysImpl systemImpl) <- program.stmts]
   let funcImpls   = [funcImpl | StmtFunc (FnImpl funcImpl) <- program.stmts]

   systems   <- forM systemImpls (lowerSystem program.moduleDef.name)
   functions <- forM funcImpls (lowerFunction program.moduleDef.name)

   return $ Kira
      program.moduleDef.name
      systems
      functions
