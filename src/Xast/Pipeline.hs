{-# LANGUAGE LambdaCase #-}
module Xast.Pipeline where

import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (dropWhileEnd)
import System.Directory (getCurrentDirectory)
import System.Process
import System.IO

import Xast.AST
import Xast.Package (loadPackages, modulePackages, parsePackage, preludeImport)
import Xast.Error.Types (XastError (..))
import Xast.SemAnalyzer.Pass (fullAnalysis)
import Xast.SemAnalyzer.Types (AnalysisResult(..))
import Xast.Error.Pretty (PrintError(printError), printWarnings)
import Xast.Utils.Pretty
import Control.Monad.RWS (MonadTrans(lift))
import Xast.Lowerer.Pass (lowerPrograms)
import Xast.Lowerer.EraseZero (eraseZeroSized, EraseResult (EraseResult))
import Xast.Codegen.C.Pretty (debugPrograms, prettyProgram)
import Xast.Codegen.C.Pass (codegen)
import Xast.Utils.Compiler (Target(Target64))
import GHC.IO.Exception (ExitCode)
import qualified Data.Map as M

runCompile :: Maybe FilePath -> IO ()
runCompile dir = runCompile_ dir >>= \case
   Left err -> do
      let len = length err
      forM_ err printError

      putStrLn
         (  show (red (bold "Compilation failed with "))
         ++ show (yellow (bold (show len)))
         ++ show (red (bold " errors."))
         )

   Right 0 ->
      print $ green $ bold "Compilation completed."

   Right warnings ->
      putStrLn
         (  show (green (bold "Compilation completed "))
         ++ show (yellow (bold ("with " <> show warnings <> " warnings")))
         ++ show (green (bold "."))
         )

runCompile_ :: Maybe FilePath -> IO (Either [XastError] Int)
runCompile_ dir = runExceptT $ do
   -- Get current dir
   currentDir <- liftIO $ maybe getCurrentDirectory (pure . dropWhileEnd (== '/')) dir

   -- Load the project and its dependencies, Std included
   packages <- loadPackages currentDir
   packageOfModule <- liftEither (modulePackages packages)

   -- Parse modules, dependencies first
   programsNoPrelude <- concat <$> mapM parsePackage packages

   let programs = map (withPrelude packageOfModule) programsNoPrelude

   -- Get compile target
   -- FIXME: Target64 only
   let target = Target64

   -- Semantic analysis
   semResult <- runExceptT $ fullAnalysis (lift . printWarnings) (\path content -> liftIO $ writeFile path content) programs packageOfModule target
   analysis <- case semResult of
      Left errs -> throwError (XastSemAnalyzeError <$> errs)
      Right res -> return res

   -- Lowering AST into KIRA
   let (loweredIR, typeDefs, intrinsicDefs) = lowerPrograms analysis

   -- Erasing zero-sized types: they have no representation in C
   let EraseResult erasedIR erasedTypeDefs ecsTags = eraseZeroSized loweredIR typeDefs

   -- Generating C
   let generatedC = codegen erasedIR erasedTypeDefs ecsTags intrinsicDefs

   -- Temporarily print C programs as debug
   ----------------------------------------
   liftIO $ putStrLn $ debugPrograms generatedC
   ----------------------------------------

   _ <- liftIO $ compileGcc $ show $ prettyProgram $ head generatedC

   return analysis.warningsCount

-- | Every module outside Std implicitly imports Std.Prelude.
withPrelude :: M.Map Module PackageName -> Program Parsed -> Program Parsed
withPrelude packageOfModule program =
   let inStd = M.lookup program.moduleDef.name packageOfModule == Just stdPackageName
   in if inStd
      then program
      else program { imports = preludeImport program : program.imports }

compileGcc :: String -> IO ExitCode
compileGcc source = do
   (Just hin, _, _, ph) <-
      createProcess
         (proc "gcc" ["-fsyntax-only", "-x", "c", "-"])
            { std_in  = CreatePipe
            , std_out = Inherit
            , std_err = Inherit
            }

   hPutStr hin source
   hClose hin

   waitForProcess ph
