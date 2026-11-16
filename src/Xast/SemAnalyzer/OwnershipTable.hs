module Xast.SemAnalyzer.OwnershipTable where

import qualified Data.Map as M

import Xast.AST
import qualified Data.Set as S
import Control.Monad.Reader (ReaderT)
import Xast.SemAnalyzer.Monad (SemAnalyzer)

-- #### OWNERSHIP ANALYSIS TABLE ####

type Walk = ReaderT WalkEnv SemAnalyzer

type Needed = M.Map VarName Location

newtype WalkEnv = WalkEnv
   { notOwned :: S.Set VarName
   }

newtype OwnershipTable = OwnershipTable 
   { summaries :: M.Map FunctionId Summary
   }
   deriving (Eq, Show)

emptyOwnershipTable :: OwnershipTable
emptyOwnershipTable = OwnershipTable M.empty

-- | How a function treats its arguments, so that callers and monomorphization need not read the body.
newtype Summary = Summary
   { parameters :: [ParameterSummary]
   }
   deriving (Eq, Show)

data ParameterSummary = ParameterSummary
   { mark       :: Mark
   , returned   :: Bool              -- ^ can flow into the result
   , stored     :: Bool              -- ^ can flow into storage or a captured value
   , duplicated :: Maybe Location    -- ^ the place that uses it twice, so the type must be copyable
   }
   deriving (Eq, Show)
