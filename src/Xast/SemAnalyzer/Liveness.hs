{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.SemAnalyzer.Liveness where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad (forM_, foldM)
import Control.Monad.State (StateT, modify, gets, MonadTrans (lift))

import Xast.AST
import Xast.SemAnalyzer.Monad (SemAnalyzer)
import Xast.SemAnalyzer.OwnershipTable
import Xast.Utils.Generic ((|>), todo__)
import Xast.SemAnalyzer.Ownership (isCopyableType)
import Xast.SemAnalyzer.Unify (typeOf)
import Control.Monad.Reader (ReaderT, MonadReader (ask))

data Usage
   = ReadOnly              -- T, match wing, condition
   | Handed SecondNameKind -- *T, let value, returned, stored, captured

analyzeLiveness :: Program Desugared -> SemAnalyzer (Program LivenessAnalyzed)
analyzeLiveness program = do
   let fnImpls = [ impl | StmtFunc (FnImpl impl) <- program.stmts ]

   undefined

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

   ExpLit {} -> return (neededAfter, fmap (livenessInfo Nothing) expr)
   ExpCon {} -> return (neededAfter, fmap (livenessInfo Nothing) expr)

   _ -> undefined

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
      let acc = (neededAfter, [])
      let argUsages = reverse (zip usages args)
      (neededBefore, args') <- foldM walkStep acc argUsages
      (neededWithCallee, callee') <- analyzeExpr ReadOnly neededBefore callee
      let rebuiltApp = foldl applyOne callee' (zip (map fst applications) args')

      return (neededWithCallee, rebuiltApp)

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

exprMarks :: Expr Desugared -> Maybe [Mark]
exprMarks expr = parameterMarks (exprAnnotation expr).ty

walkStep
   :: (Needed, [Expr LivenessAnalyzed])
   -> (Usage, Expr Desugared)
   -> Walk (Needed, [Expr LivenessAnalyzed])
walkStep (needed, done) (argUsage, arg) = do
   (needed', arg') <- analyzeExpr argUsage needed arg
   return (needed', arg' : done)

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

decideUse
   :: SecondNameKind
   -> Desugared
   -> VarName
   -> Needed
   -> Walk OwnershipDecision
decideUse kind info name neededAfter = do
   env      <- ask
   copyable <- lift $ isCopyableType info.ty
   let noMove cause = NoMove
         { valueType = info.ty
         , outcome =
            if copyable then
               Clone { inLoop = False }
            else
               Reject
         , ..
         }

   return $ case M.lookup name neededAfter of
      _ | name `S.member` env.notOwned -> noMove NotOwned
      Just laterUse                    -> noMove (OldNameLive laterUse)
      Nothing                          -> Move