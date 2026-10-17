module Xast.Lowerer.Monad where

import Control.Monad.State (StateT (runStateT), get, put)
import Control.Monad.Identity (Identity)
import Data.Text (pack)

import Xast.Lowerer.Types
import Control.Monad.Writer (WriterT (runWriterT))

type Lowerer =
   WriterT [KirTypeDef]
      ( StateT
         LowerState
         Identity
      )

runLowerer 
   :: LowerState 
   -> Lowerer a 
   -> Identity ((a, [KirTypeDef]), LowerState)
runLowerer state lowerer =
   runStateT (runWriterT lowerer) state

freshKirName :: Lowerer KirName
freshKirName = do
   st <- get
   let n = st.nameSupply
   put st { nameSupply = n + 1 }
   pure $ KirName (pack ("t" <> show n))

freshKirLocalId :: Lowerer KirLocalId
freshKirLocalId = do
   st <- get
   let n = st.nameSupply
   put st { nameSupply = n + 1 }
   pure $ KirLocalId n