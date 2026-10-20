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
import qualified Xast.Utils.List as L
import Xast.Utils.Generic ((|>))

-- #### Desugaring ####

desugarProgram :: Program Typed -> SemAnalyzer (Program Desugared)
desugarProgram program = do
   let nonImplStmts = [desugaredAnn <$> stmt | stmt <- program.stmts, isNotImpl stmt]

   -- Functions
   let functionImpls = [funcImpl | StmtFunc (FnImpl funcImpl) <- program.stmts]
   let fnGroups =
         groupBy ((==) `on` (.name))
            $ sortOn (.name) functionImpls
   desugaredFns <- forM fnGroups desugarFn

   -- Systems
   let systemImpls = [systemImpl | StmtSystem (SysImpl systemImpl) <- program.stmts]
   let sysGroups =
         groupBy ((==) `on` (.name))
            $ sortOn (.name) systemImpls
   desugaredSystems <- forM sysGroups desugarSys

   -- Combine stmts
   let combinedStmts = nonImplStmts
         <> fmap (StmtFunc . FnImpl) desugaredFns
         <> fmap (StmtSystem . SysImpl) desugaredSystems

   return $ Program
      program.moduleDef
      program.imports
      combinedStmts
      program.src

desugarFn :: [FuncImpl Typed] -> SemAnalyzer (FuncImpl Desugared)
desugarFn group = do
   let fstImpl = head group
   let argTypes = map ((.ty) . patAnnotation) fstImpl.args

   isExhaustive <- checkClausesExhaustive fstImpl.location argTypes (map (.args) group)

   -- Tuple patterns and bodies
   let tupleAnn = DesugaredInfo {res = Nothing, ty = TyTuple argTypes}
   let patBodies = L.forEach group $ \impl ->
         ( PatTuple tupleAnn $ map (fmap desugaredAnn) impl.args
         , fmap desugaredAnn impl.body
         )

   -- Define args names
   let argsCount = length fstImpl.args
   args <- forM [0..argsCount] $ const freshParam
   let (ids, names) = unzip args
   let typedArgs = zip3 ids names argTypes

   -- Base expression
   let baseExpr = ExpTuple tupleAnn $ L.forEach typedArgs $ \(localId, name, ty) ->
         ExpVar DesugaredInfo {res = Just (ResLocal localId), ty = ty} Nothing name

   -- Generate match
   let matchExpr = Match baseExpr (map (uncurry MatchWing) patBodies) isExhaustive
   let retType = (patBodies |> head |> snd |> exprAnnotation).ty

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
   let flattenClause impl =
         L.forFlat impl.entities (\(EntityPattern bindings) -> map (.pat) bindings)
            ++ fromMaybe [] impl.with

   let entityTypes = L.forFlat fstImpl.entities $ \(EntityPattern bindings) ->
         bindings |> map (\binding -> binding.pat |> patAnnotation |> (.ty))
   let withTypes = maybe [] (map ((.ty) . patAnnotation)) fstImpl.with
   let argTypes = entityTypes ++ withTypes

   isExhaustive <- checkClausesExhaustive fstImpl.location argTypes (map flattenClause group)

   -- Tuple patterns and bodies
   let tupleAnn = DesugaredInfo {res = Nothing, ty = TyTuple argTypes}
   let patBodies = L.forEach group $ \impl ->
         ( PatTuple tupleAnn $ map (fmap desugaredAnn) (flattenClause impl)
         , fmap desugaredAnn impl.body
         )

   -- Define param names, one per flattened position
   args <- forM argTypes $ const freshParam
   let typedArgs = zipWith (\(localId, name) ty -> (localId, name, ty)) args argTypes

   -- Base expression
   let baseExpr = ExpTuple tupleAnn $ L.forEach typedArgs $ \(localId, name, ty) ->
         ExpVar DesugaredInfo {res = Just (ResLocal localId), ty = ty} Nothing name

   -- Generate match
   let matchExpr = Match baseExpr (map (uncurry MatchWing) patBodies) isExhaustive
   let retType = (patBodies |> head |> snd |> exprAnnotation).ty

   -- Rebuild the entity/with shape from `fstImpl`, substituting each
   -- component's pattern with its freshly named param.
   let (entityArgs, withArgs) = splitAt (length entityTypes) typedArgs

   let mkVar (localId, name, ty) = PatVar (DesugaredInfo {res = Just (ResLocal localId), ty = ty}) name

   let rebuildEntities _ [] = []
       rebuildEntities available (EntityPattern bindings : rest) =
         let (used, remaining) = splitAt (length bindings) available
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
   localId@(LocalId idNumber) <- freshLocalId
   pure (localId, Ident ("p_" <> T.pack (show idNumber)))

isNotImpl :: Stmt a -> Bool
isNotImpl (StmtFunc (FnImpl _)) = False
isNotImpl (StmtSystem (SysImpl _)) = False
isNotImpl _ = True
