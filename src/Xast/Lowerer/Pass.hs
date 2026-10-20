{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.Pass where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, runLowerer)
import Xast.Lowerer.Types
import Xast.Lowerer.Mono (drainTypes)
import Control.Monad (forM)
import qualified Data.Map as M
import Control.Monad.Identity (Identity(runIdentity))
import Xast.Lowerer.Definition (lowerFunction, lowerSystem)

lowerPrograms :: M.Map ConstructorId Int -> [Program Desugared] -> ([Kira], [KirTypeDef])
lowerPrograms spans programs =
   let lowered = forM programs lowerProgram <* drainTypes
       ((irs, typeDefs), _) = runIdentity $
         runLowerer (newLowerState spans programs) lowered
   in (irs, typeDefs)

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
