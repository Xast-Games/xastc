module Xast.Lowerer.Match where

import qualified Data.Map as M
import Xast.AST
import Xast.Lowerer.Monad (Lowerer)
import Xast.Lowerer.Types

lowerMatchTo
   :: M.Map LocalId KirValue
   -> Type
   -> KirDest
   -> Match Desugared
   -> Lowerer [KirInstruct]
