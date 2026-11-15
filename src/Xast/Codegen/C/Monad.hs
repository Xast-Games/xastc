module Xast.Codegen.C.Monad where

import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT (runReaderT))

import Xast.Codegen.C.Types (EcsTag)
import Xast.Lowerer.Types (KirTypeDef, KirIntrinsicDef)

data CCodegenEnv = CCodegenEnv
   { types      :: [KirTypeDef]
   , ecsTags    :: [EcsTag] 
   , intrinsics :: [KirIntrinsicDef]  -- ^ intrinsic instances the program uses
   }
   deriving Show

type CCodegen =
   ReaderT CCodegenEnv
   Identity

runCodegen
   :: CCodegen a
   -> CCodegenEnv
   -> Identity a
runCodegen = runReaderT