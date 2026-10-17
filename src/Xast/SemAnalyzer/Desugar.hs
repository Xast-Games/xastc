{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Desugar where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.List (sortOn, groupBy)
import Xast.AST
import Xast.SemAnalyzer.Monad
import Data.Function (on)
import qualified Data.Text as T
import Xast.SemAnalyzer.Exhaustive (checkClausesExhaustive)
import Xast.SemAnalyzer.Unify (freshLocalId)

-- #### Desugaring ####

desugarProgram :: Program Typed -> SemAnalyzer (Program Desugared)
desugarProgram prog = do
   let notImpl = [desugaredAnn <$> x | x <- prog.stmts, isNotImpl x]

   -- Functions
   let fmImpls = [x | StmtFunc (FnImpl x) <- prog.stmts]
   let fnGroups =
         groupBy ((==) `on` (.name))
            $ sortOn (.name) fmImpls
   desugaredFns <- forM fnGroups desugarFn

   -- Systems
   let sysImpls = [x | StmtSystem (SysImpl x) <- prog.stmts]
   let sysGroups =
         groupBy ((==) `on` (.name))
            $ sortOn (.name) sysImpls
   desugaredSystems <- forM sysGroups desugarSys

   -- Combine stmts
   let combinedStmts = notImpl
         <> fmap (StmtFunc . FnImpl) desugaredFns
         <> fmap (StmtSystem . SysImpl) desugaredSystems

   return $ Program
      prog.moduleDef
      prog.imports
      combinedStmts
      prog.src

desugarFn :: [FuncImpl Typed] -> SemAnalyzer (FuncImpl Desugared)
desugarFn group = do
   let fstImpl = head group
   let argTypes = map ((.ty) . patAnnotation) fstImpl.args

   isExhaustive <- checkClausesExhaustive fstImpl.location argTypes (map (.args) group)

   -- Tuple patterns and bodies
   let tupleAnn = DesugaredInfo {res = Nothing, ty = TyTuple argTypes}
   let patBodies = flip map group $ \i ->
         ( PatTuple tupleAnn $ map (fmap desugaredAnn) i.args
         , fmap desugaredAnn i.body
         )

   -- Define args names
   let argsCnt = length fstImpl.args
   args <- forM [0..argsCnt] $ const freshParam
   let (ids, names) = unzip args
   let typedArgs = zip3 ids names argTypes

   -- Base expression
   let baseExpr = ExpTuple tupleAnn $ flip map typedArgs $ \(lid, name, ty) ->
         ExpVar DesugaredInfo {res = Just (ResLocal lid), ty = ty} Nothing name

   -- Generate match
   let matchExpr = Match baseExpr (map (uncurry MatchWing) patBodies) isExhaustive
   let retType = (exprAnnotation (snd (head patBodies))).ty

   -- Construct new func impl
   return $ FuncImpl
      fstImpl.location
      fstImpl.name
      (map fst patBodies)
      (ExpMatch DesugaredInfo {res = Nothing, ty = retType} matchExpr)

desugarSys :: [SystemImpl Typed] -> SemAnalyzer (SystemImpl Desugared)
desugarSys group = do
   let fstImpl = head group

   -- Flatten every entity's component patterns
   let flattenClause i =
         concatMap (\(EntityPattern bs) -> map (.pat) bs) i.entities
            ++ fromMaybe [] i.with

   let entityTypes = concatMap (\(EntityPattern bs) -> map ((.ty) . patAnnotation . (.pat)) bs) fstImpl.entities
   let withTypes = maybe [] (map ((.ty) . patAnnotation)) fstImpl.with
   let argTypes = entityTypes ++ withTypes

   isExhaustive <- checkClausesExhaustive fstImpl.location argTypes (map flattenClause group)

   -- Tuple patterns and bodies
   let tupleAnn = DesugaredInfo {res = Nothing, ty = TyTuple argTypes}
   let patBodies = flip map group $ \i ->
         ( PatTuple tupleAnn $ map (fmap desugaredAnn) (flattenClause i)
         , fmap desugaredAnn i.body
         )

   -- Define param names, one per flattened position
   args <- forM argTypes $ const freshParam
   let typedArgs = zipWith (\(lid, name) ty -> (lid, name, ty)) args argTypes

   -- Base expression
   let baseExpr = ExpTuple tupleAnn $ flip map typedArgs $ \(lid, name, ty) ->
         ExpVar DesugaredInfo {res = Just (ResLocal lid), ty = ty} Nothing name

   -- Generate match
   let matchExpr = Match baseExpr (map (uncurry MatchWing) patBodies) isExhaustive
   let retType = (exprAnnotation (snd (head patBodies))).ty

   -- Rebuild the entity/with shape from `fstImpl`, substituting each
   -- component's pattern with its freshly named param.
   let (entityArgs, withArgs) = splitAt (length entityTypes) typedArgs

   let mkVar (lid, name, ty) = PatVar (DesugaredInfo {res = Just (ResLocal lid), ty = ty}) name

   let rebuildEntities _ [] = []
       rebuildEntities avail (EntityPattern bindings : rest) =
         let (used, remaining) = splitAt (length bindings) avail
             newBindings = zipWith
                  (\arg binding -> EntPatBinding (mkVar arg) binding.access)
                  used bindings
         in EntityPattern newBindings : rebuildEntities remaining rest

   let newEntities = rebuildEntities entityArgs fstImpl.entities
   let newWith = fmap (const (map mkVar withArgs)) fstImpl.with

   -- Construct new system impl
   return $ SystemImpl
      fstImpl.location
      fstImpl.name
      newEntities
      newWith
      (ExpMatch DesugaredInfo {res = Nothing, ty = retType} matchExpr)

freshParam :: SemAnalyzer (LocalId, Ident)
freshParam = do
   lid@(LocalId n) <- freshLocalId
   pure (lid, Ident ("p_" <> T.pack (show n)))

isNotImpl :: Stmt a -> Bool
isNotImpl (StmtFunc (FnImpl _)) = False
isNotImpl (StmtSystem (SysImpl _)) = False
isNotImpl _ = True
