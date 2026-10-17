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
lowerPrograms spans progs =
   let lowered = forM progs lowerProgram <* drainTypes
       ((irs, typeDefs), _) = runIdentity $
         runLowerer (newLowerState spans progs) lowered
   in (irs, typeDefs)

lowerProgram :: Program Desugared -> Lowerer Kira
lowerProgram prog = do
   let systemImpls = [x | StmtSystem (SysImpl x) <- prog.stmts]
   let funcImpls   = [x | StmtFunc (FnImpl x) <- prog.stmts]

   systems   <- forM systemImpls (lowerSystem prog.moduleDef.name)
   functions <- forM funcImpls (lowerFunction prog.moduleDef.name)

   return $ Kira
      prog.moduleDef.name
      systems
      functions
