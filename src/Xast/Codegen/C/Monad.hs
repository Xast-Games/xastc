module Xast.Codegen.C.Monad where
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT (runReaderT))
import Xast.Lowerer.Types (KirTypeDef)

type CCodegen =
   ReaderT [KirTypeDef]
   Identity

runCodegen
   :: CCodegen a
   -> [KirTypeDef]
   -> Identity a
runCodegen = runReaderT