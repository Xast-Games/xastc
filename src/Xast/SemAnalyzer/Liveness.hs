{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.SemAnalyzer.Liveness where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader (ask, local), asks, runReaderT)
import Control.Monad.State (MonadTrans (lift))

import Data.Maybe (fromMaybe, listToMaybe)

import Xast.AST
import Xast.SemAnalyzer.Monad (SemAnalyzer)
import Xast.SemAnalyzer.Ownership (copyRequirement)
import Xast.SemAnalyzer.OwnershipTable
import Xast.Utils.Generic (todo__)

analyzeLiveness :: Program Desugared -> SemAnalyzer (Program LivenessAnalyzed)
analyzeLiveness program = do
   let definitions = M.fromList
         [ (definition.name, definition)
         | StmtFunc (FnDef definition) <- program.stmts
         ]
   stmts' <- mapM (analyzeStmt definitions) program.stmts

   return Program
      { moduleDef = program.moduleDef
      , imports   = program.imports
      , stmts     = stmts'
      , src       = program.src
      }

analyzeStmt
   :: M.Map FunctionName FuncDef
   -> Stmt Desugared
   -> SemAnalyzer (Stmt LivenessAnalyzed)
analyzeStmt definitions = \case
   StmtFunc (FnImpl impl) -> do
      let marks = maybe [] (map (.node.mark) . (.args)) (M.lookup impl.name definitions)
      impl' <- analyzeFunction marks impl
      return (StmtFunc (FnImpl impl'))

   -- Systems are not analyzed yet
   other -> return (fmap (livenessInfo Nothing) other)

analyzeFunction
   :: [Mark]
   -> FuncImpl Desugared
   -> SemAnalyzer (FuncImpl LivenessAnalyzed)
analyzeFunction marks impl = do
   let env = WalkEnv { notOwned = borrowedParameters marks impl.body }
   (_, body') <- runReaderT (analyzeExpr (Handed Returned) M.empty impl.body) env

   return FuncImpl
      { location = impl.location
      , name     = impl.name
      , args     = map (fmap (livenessInfo Nothing)) impl.args
      , body     = body'
      }

-- | The parameters of a desugared function are the variables of the tuple
--   that its body matches on, in the order of the declaration.
borrowedParameters :: [Mark] -> Expr Desugared -> S.Set VarName
borrowedParameters marks body = S.fromList
   [ name
   | (ExpVar { name }, Read) <- zip (parameterVariables body) marks
   ]

parameterVariables :: Expr Desugared -> [Expr Desugared]
parameterVariables = \case
   ExpMatch { match } -> scrutineeParts match.baseExpr
   _                  -> []

analyzeExpr
   :: Usage
   -> Needed
   -> Expr Desugared
   -> Walk (Needed, Expr LivenessAnalyzed)
analyzeExpr usage neededAfter expr = case expr of
   ExpVar { info, bind, name }
      | isLocal info.res -> do
         decision <- case usage of
            Handed kind ->
               Just <$> decideUse kind info name neededAfter
            ReadOnly ->
               return Nothing

         return
            ( M.insert name info.location neededAfter
            , ExpVar { info = livenessInfo decision info, ..}
            )

      | otherwise -> return
         ( neededAfter
         , ExpVar { info = livenessInfo Nothing info, ..}
         )

   ExpApp {} -> do
      let (callee, applications) = applicationSpineInfos expr
      let usages = applicationUsages callee (map snd applications)
      analyzeCall neededAfter callee applications usages

   ExpTuple { info, elements } -> do
      let elementsAsStored = map (Handed Stored,) elements
      (needed, elements') <- analyzeEach neededAfter elementsAsStored

      return
         ( needed
         , ExpTuple
            { info = livenessInfo Nothing info
            , elements = elements'
            }
         )

   ExpList { info, elements } -> do
      let elementsAsStored = map (Handed Stored,) elements
      (needed, elements') <- analyzeEach neededAfter elementsAsStored

      return
         ( needed
         , ExpList
            { info = livenessInfo Nothing info
            , elements = elements'
            }
         )

   ExpRecConstruct { info, construct } -> do
      let values = map (.value) construct.assigns
      let valuesAsStored = map (Handed Stored,) values
      (needed, values') <- analyzeEach neededAfter valuesAsStored
      let appendValue assign value' = RecAssign
            { name = assign.name
            , value = value'
            }
      let assigns' = zipWith appendValue construct.assigns values'

      return
         ( needed
         , ExpRecConstruct
            { info = livenessInfo Nothing info
            , construct = RecConstruct
               { bind = construct.bind
               , con = construct.con
               , assigns = assigns'
               }
            }
         )

   ExpLetIn { info, letIn } -> do
      (neededInBody, body') <- analyzeExpr usage neededAfter letIn.bindExpr
      (needed, bindings')   <- analyzeBindings neededInBody letIn.bindings

      return
         ( needed
         , ExpLetIn
            { info = livenessInfo Nothing info
            , letIn = LetIn
               { bindExpr = body'
               , bindings = bindings'
               }
            }
         )

   ExpIfThen { info, ifThenElse } -> do
      (neededThen, then') <- analyzeExpr usage neededAfter ifThenElse.thenExpr
      (neededElse, else') <- analyzeExpr usage neededAfter ifThenElse.elseExpr
      let merged = M.unionWith min neededThen neededElse
      (needed, cond') <- analyzeExpr ReadOnly merged ifThenElse.ifExpr

      return
         ( needed
         , ExpIfThen
            { info = livenessInfo Nothing info
            , ifThenElse = IfThenElse
               { ifExpr = cond'
               , thenExpr = then'
               , elseExpr = else'
               }
            }
         )

   ExpMatch { info, match } -> do
      (needed, match') <- analyzeMatchExpr usage neededAfter match

      return
         ( needed
         , ExpMatch
            { info = livenessInfo Nothing info
            , match = match'
            }
         )

   ExpLit {} -> return (neededAfter, fmap (livenessInfo Nothing) expr)
   ExpCon {} -> return (neededAfter, fmap (livenessInfo Nothing) expr)

   _ -> undefined

analyzeMatchExpr
   :: Usage
   -> Needed
   -> Match Desugared
   -> Walk (Needed, Match LivenessAnalyzed)
analyzeMatchExpr usage neededAfter match = do
   let parts      = scrutineeParts match.baseExpr
   let arity      = length parts
   let armColumns = map (patternColumns arity . (.pat)) match.matches
   let columns    = foldr (zipWith (:)) (replicate arity []) armColumns

   borrowed <- mapM isBorrowed parts

   let scrutineeUsages = zipWith partUsage borrowed columns
   let borrowedByArm = map (borrowedNames borrowed) armColumns
   let armsWithBorrowed = zip borrowedByArm match.matches

   (neededByArms, matches') <- analyzeMatches usage neededAfter armsWithBorrowed
   (needed, base') <- analyzeScrutinee scrutineeUsages neededByArms match.baseExpr

   return
      ( needed
      , Match
         { baseExpr = base'
         , matches = matches'
         , exhaustive = match.exhaustive
         }
      )

borrowedNames
   :: [Bool]       -- ^ for each scrutinee part, whether it is borrowed
   -> [Pattern a]  -- ^ the arm's pattern for each part
   -> S.Set VarName
borrowedNames borrowed armPatterns = S.fromList $ concat
   [ patternNames pattern
   | (isBorrowedPart, pattern) <- zip borrowed armPatterns
   , isBorrowedPart
   ]

partUsage :: Bool -> [Pattern Desugared] -> Usage
partUsage borrowed patterns
   | borrowed                                 = ReadOnly
   | not (all (null . patternNames) patterns) = Handed Rebound
   | otherwise                                = ReadOnly

scrutineeParts :: Expr a -> [Expr a]
scrutineeParts = \case
   ExpTuple { elements } -> elements
   other                 -> [other]

patternColumns :: Int -> Pattern a -> [Pattern a]
patternColumns width = \case
   PatTuple { elements } | length elements == width -> elements
   other                                            -> replicate width other

isBorrowed :: Expr Desugared -> Walk Bool
isBorrowed = \case
   ExpVar { info, name } | isLocal info.res -> 
      asks (S.member name . (.notOwned))
   ExpVarGetter { base } -> 
      isBorrowed base
   _ -> 
      return False

analyzeMatches
   :: Usage
   -> Needed
   -> [(S.Set VarName, MatchWing Desugared)]  -- ^ each arm with the names it borrows
   -> Walk (Needed, [MatchWing LivenessAnalyzed])
analyzeMatches usage neededAfter matches = do
   results <- mapM (uncurry (analyzeMatch usage neededAfter)) matches
   let needed = M.unionsWith min (neededAfter : map fst results)
   return (needed, map snd results)

analyzeMatch
   :: Usage
   -> Needed
   -> S.Set VarName
   -> MatchWing Desugared
   -> Walk (Needed, MatchWing LivenessAnalyzed)
analyzeMatch usage neededAfter armBorrowed match = do
   let boundNames = S.fromList (patternNames match.pat)
   let afterBody  = M.withoutKeys neededAfter boundNames

   (needed, body') <- local (borrowing armBorrowed) (analyzeExpr usage afterBody match.body)

   let match' = MatchWing
         { pat  = fmap (livenessInfo Nothing) match.pat
         , body = body'
         }

   return (needed, match')

borrowing :: S.Set VarName -> WalkEnv -> WalkEnv
borrowing names env = WalkEnv { notOwned = S.union names env.notOwned }

analyzeScrutinee
   :: [Usage]
   -> Needed
   -> Expr Desugared
   -> Walk (Needed, Expr LivenessAnalyzed)
analyzeScrutinee usages neededAfter = \case
   ExpTuple { info, elements } -> do
      (needed, elements') <- analyzeEach neededAfter (zip usages elements)
      return
         ( needed
         , ExpTuple
            { info = livenessInfo Nothing info
            , elements = elements'
            }
         )

   other -> analyzeExpr (fromMaybe ReadOnly (listToMaybe usages)) neededAfter other

analyzeBindings
   :: Needed
   -> [Let Desugared]
   -> Walk (Needed, [Let LivenessAnalyzed])
analyzeBindings neededAfter bindings =
   foldM analyzeBinding (neededAfter, []) (reverse bindings)

analyzeBinding
   :: (Needed, [Let LivenessAnalyzed])
   -> Let Desugared
   -> Walk (Needed, [Let LivenessAnalyzed])
analyzeBinding (needed, done) binding = do
   let boundNames = S.fromList (patternNames binding.pat)
   let afterValue = M.withoutKeys needed boundNames
   let usage =
         if null boundNames then
            ReadOnly
         else
            Handed Rebound

   (needed', value') <- analyzeExpr usage afterValue binding.value

   let binding' = Let
         { pat   = fmap (livenessInfo Nothing) binding.pat
         , value = value'
         }

   return (needed', binding' : done)

analyzeCall
   :: Needed
   -> Expr Desugared
   -> [(Desugared, Expr Desugared)]
   -> [Usage]
   -> Walk (Needed, Expr LivenessAnalyzed)
analyzeCall neededAfter callee applications usages
   | length applications < length usages = todo__ $
      "Partial application is not supported in ownership analysis yet: "
      ++ show (length usages) ++ " argument usages with "
      ++ show (length applications) ++ " args given."

   | length applications > length usages = todo__ $
      "Functions returning functions are not supported in ownership analysis yet: "
      ++ show (length usages) ++ " argument usages with "
      ++ show (length applications) ++ " args given."

   | otherwise = do
      let args = map snd applications
      let argUsages = zip usages args
      (neededBefore, args') <- analyzeEach neededAfter argUsages
      (neededWithCallee, callee') <- analyzeExpr ReadOnly neededBefore callee
      let rebuiltApp = foldl applyOne callee' (zip (map fst applications) args')

      return (neededWithCallee, rebuiltApp)

analyzeEach
   :: Needed
   -> [(Usage, Expr Desugared)]
   -> Walk (Needed, [Expr LivenessAnalyzed])
analyzeEach neededAfter usagesAndExprs =
   foldM walkStep (neededAfter, []) (reverse usagesAndExprs)
      where
         walkStep
            :: (Needed, [Expr LivenessAnalyzed])
            -> (Usage, Expr Desugared)
            -> Walk (Needed, [Expr LivenessAnalyzed])
         walkStep (needed, done) (argUsage, arg) = do
            (needed', arg') <- analyzeExpr argUsage needed arg
            return (needed', arg' : done)

applicationUsages :: Expr Desugared -> [Expr Desugared] -> [Usage]
applicationUsages callee args = case callee of
   ExpCon {} -> map (const (Handed Stored)) args
   _         -> case exprMarks callee of
      Just marks -> map usageOfMark marks
      Nothing    -> map (const ReadOnly) args

applyOne
   :: Expr LivenessAnalyzed
   -> (Desugared, Expr LivenessAnalyzed)
   -> Expr LivenessAnalyzed
applyOne operand (info, applicant) =
   ExpApp { info = livenessInfo Nothing info, .. }

patternNames :: Pattern a -> [VarName]
patternNames = \case
   PatVar { name }      -> [name]
   PatTuple { elements } -> concatMap patternNames elements
   PatList { elements }  -> concatMap patternNames elements
   PatCon { args }       -> concatMap patternNames args
   _                     -> [] -- wildcard, literal

exprMarks :: Expr Desugared -> Maybe [Mark]
exprMarks expr = parameterMarks (exprAnnotation expr).ty

usageOfMark :: Mark -> Usage
usageOfMark = \case
   Read -> ReadOnly
   Take -> Handed PassedToTake

parameterMarks :: Type -> Maybe [Mark]
parameterMarks = \case
   TyFn params _ -> Just (map (.mark) params)
   _             -> Nothing

isLocal :: Maybe Resolution -> Bool
isLocal = \case
   Just (ResLocal _) -> True
   _ -> False

-- | A clone is made, is made only if the type's generics are copyable, or is an error.
cloneOutcome :: CopyRequirement -> NoMoveOutcome
cloneOutcome = \case
   NeverCopyable -> Reject
   CopyableIf required
      | S.null required -> Clone { inLoop = False }
      | otherwise       -> CloneIf { inLoop = False, requires = required }

decideUse
   :: SecondNameKind
   -> Desugared
   -> VarName
   -> Needed
   -> Walk OwnershipDecision
decideUse kind info name neededAfter = do
   env      <- ask
   requirement <- lift $ copyRequirement info.ty
   let noMove cause = NoMove
         { valueType = info.ty
         , outcome = cloneOutcome requirement
         , ..
         }

   return $ case M.lookup name neededAfter of
      _ | name `S.member` env.notOwned -> noMove NotOwned
      Just laterUse                    -> noMove (OldNameLive laterUse)
      Nothing                          -> Move