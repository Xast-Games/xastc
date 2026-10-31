module Xast.Codegen.C.Monad where

import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT (runReaderT))
import Xast.Lowerer.Types (KirTypeDef)
import Xast.Codegen.C.Types (EcsTag)

data CCodegenEnv = CCodegenEnv
   { types   :: [KirTypeDef]
   , ecsTags :: [EcsTag] 
   }

type CCodegen =
   ReaderT CCodegenEnv
   Identity

runCodegen
   :: CCodegen a
   -> CCodegenEnv
   -> Identity a
runCodegen = runReaderT