{-# LANGUAGE LambdaCase #-}
module Xast.Pipeline where

import Control.Monad (unless, filterM, forM_)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (pack)
import Data.List (dropWhileEnd)
import Data.Either (partitionEithers)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.Process
import System.IO

import Xast.Config (xastConfigCodec, projectConfig, modules)
import Xast.Error.Types (XastError (..))
import Xast.Parser.Program (parseProgram)
import Xast.AST
import Xast.SemAnalyzer.Pass (fullAnalysis)
import Xast.SemAnalyzer.Types (AnalysisResult(..))
import Xast.Error.Pretty (PrintError(printError), printWarnings)
import Xast.Utils.Pretty
import qualified Toml
import Control.Monad.RWS (MonadTrans(lift))
import Xast.Lowerer.Pass (lowerPrograms)
import Xast.Lowerer.EraseZero (eraseZeroSized)
import Xast.Codegen.C.Pretty (debugPrograms, prettyProgram)
import Xast.Codegen.C.Pass (codegen)
import Xast.Utils.Compiler (Target(Target64))
import GHC.IO.Exception (ExitCode)

runCompile :: Maybe FilePath -> IO ()
runCompile dir = runCompile_ dir >>= \case
   Left err -> do
      let len = length err
      forM_ err printError

      putStrLn
         (  show (red (bold ("Compilation failed with " :: String)))
         ++ show (yellow (bold (show len)))
         ++ show (red (bold (" errors." :: String)))
         )

   Right 0 ->
      print $ green $ bold ("Compilation completed." :: String)

   Right warnings ->
      putStrLn
         (  show (green (bold ("Compilation completed " :: String)))
         ++ show (yellow (bold ("with " <> show warnings <> " warnings")))
         ++ show (green (bold ("." :: String)))
         )

runCompile_ :: Maybe FilePath -> IO (Either [XastError] Int)
runCompile_ dir = runExceptT $ do
   -- Get current dir
   currentDir <- liftIO $ maybe getCurrentDirectory (pure . dropWhileEnd (== '/')) dir

   -- Load project configuration
   let configFile = currentDir ++ "/xast.toml"
   configFileExists <- liftIO $ doesFileExist configFile
   unless configFileExists $
      throwError [XastFileNotFound "xast.toml" currentDir]

   tomlRes <- Toml.decodeFileEither xastConfigCodec configFile
   config <- case tomlRes of
      Left errors -> throwError (XastTomlDecodeError configFile <$> errors)
      Right cfg   -> pure cfg

   invalidModules <- liftIO $ filterM
      (\m -> not <$> doesFileExist (currentDir ++ "/" ++ moduleToPath m ".xst"))
      config.projectConfig.modules

   case invalidModules of
      (m:_) -> throwError [XastModuleNotFound m currentDir]
      []    -> return ()

   -- Parse modules
   results <- liftIO $ traverse (parseOne currentDir) config.projectConfig.modules
   let (errors, programs) = partitionEithers results
   unless (null errors) $
      throwError errors

   -- Get compile target
   -- FIXME: Target64 only
   let target = Target64

   -- Semantic analysis
   semResult <- runExceptT $ fullAnalysis  (lift . printWarnings) (\path content -> liftIO $ writeFile path content) programs target
   (warnings, progsAnalyzed, ctorSpans) <- case semResult of
      Left errs -> throwError (XastSemAnalyzeError <$> errs)
      Right res -> return (res.warningsCount, res.progs, res.ctorSpans)

   -- Lowering AST into KIRA
   let (loweredIR, typeDefs) = lowerPrograms ctorSpans progsAnalyzed

   -- Erasing zero-sized types: they have no representation in C
   let (erasedIR, erasedTypeDefs) = eraseZeroSized loweredIR typeDefs

   -- Generating C
   let generatedC = codegen erasedIR erasedTypeDefs

   -- Temporarily print C programs as debug
   ----------------------------------------
   liftIO $ putStrLn $ debugPrograms generatedC
   ----------------------------------------

   _ <- liftIO $ compileGcc $ show $ prettyProgram $ head generatedC

   return warnings

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

parseOne :: FilePath -> Module -> IO (Either XastError (Program Parsed))
parseOne currentDir module_ = runExceptT $ do
   let filepath = currentDir ++ "/" ++ moduleToPath module_ ".xst"
   code <- liftIO $ readFile filepath
   ExceptT $ pure $ parseProgram filepath (pack code)