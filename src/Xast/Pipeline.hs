{-# LANGUAGE LambdaCase #-}
module Xast.Pipeline where

import Control.Exception (IOException, try)
import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromMaybe, maybeToList)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory)
import System.IO
import System.Process
import Text.Pretty.Simple (pShowNoColor)

import Xast.AST
import Xast.Codegen.C.Pass (codegen)
import Xast.Codegen.C.Pretty (debugPrograms, prettyProgram)
import Xast.Codegen.C.Types (CProgram (..))
import Xast.Error.Pretty (PrintError (printError), printWarnings)
import Xast.Error.Types (XastError (..))
import Xast.Lowerer.EraseZero (eraseZeroSized, EraseResult (..))
import Xast.Lowerer.Pass (lowerPrograms)
import Xast.Lowerer.Types (Kira (..))
import Xast.Package (loadPackages, modulePackages, parsePackage, preludeImport)
import Xast.SemAnalyzer.Pass (fullAnalysis)
import Xast.SemAnalyzer.Types (AnalysisResult (..))
import Xast.Utils.Compiler (Target (Target64))
import Xast.Utils.Pretty

-- | What every command needs to find a project.
data ProjectOptions = ProjectOptions
   { directory    :: FilePath        -- ^ the project, e.g. @examples/Shapes@
   , stdDirectory :: Maybe FilePath  -- ^ @--std@
   }

data BuildOptions = BuildOptions
   { project         :: ProjectOptions
   , outputDirectory :: Maybe FilePath  -- ^ @-o@, otherwise @<project>/build@
   , compiler        :: Maybe String    -- ^ checks the generated C; @Nothing@ for @--no-cc@
   , flecsDirectory  :: Maybe FilePath  -- ^ @--flecs@, otherwise @XAST_FLECS@
   }

-- | A stage of the compiler to print.
data DumpStage
   = DumpParsed     -- ^ the syntax tree of every module
   | DumpDesugared  -- ^ after semantic analysis
   | DumpKira       -- ^ the intermediate representation, after erasure
   | DumpC          -- ^ the generated C

data DumpOptions = DumpOptions
   { stage   :: DumpStage
   , project :: ProjectOptions
   , module' :: Maybe Module  -- ^ @--module@: print only this one
   }

-- #### COMMANDS ####

-- | @xastc check@: parses and analyses the project.
runCheck :: ProjectOptions -> IO ExitCode
runCheck options = report "Check" $ do
   analysis <- analyzeProject options
   pure analysis.warningsCount

-- | @xastc build@: writes the generated C and checks it with a C compiler.
runBuild :: BuildOptions -> IO ExitCode
runBuild options = report "Compilation" $ do
   analysis <- analyzeProject options.project
   let generated = generate analysis
   let outputDirectory = fromMaybe (options.project.directory </> "build") options.outputDirectory
   liftIO $ writePrograms outputDirectory generated.programs
   forM_ options.compiler $ \compiler ->
      checkGeneratedCode compiler options.flecsDirectory outputDirectory generated.programs
   pure analysis.warningsCount

-- | @xastc dump@: prints one stage of the compiler.
runDump :: DumpOptions -> IO ExitCode
runDump options = report "Dump" $ case options.stage of
   DumpParsed -> do
      loaded <- loadProject options.project
      liftIO $ dumpModules options.module' (.moduleDef.name) loaded.programs
      pure 0
   DumpDesugared -> do
      analysis <- analyzeProject options.project
      liftIO $ dumpModules options.module' (.moduleDef.name) analysis.progs
      pure analysis.warningsCount
   DumpKira -> do
      analysis <- analyzeProject options.project
      liftIO $ dumpModules options.module' (.moduleName) (generate analysis).kira
      pure analysis.warningsCount
   DumpC -> do
      analysis <- analyzeProject options.project
      let programs = (generate analysis).programs
      liftIO $ putStrLn $ debugPrograms (selectPrograms options.module' programs)
      pure analysis.warningsCount

-- #### STAGES ####

-- | The project and its dependencies, Std included, parsed.
data LoadedProject = LoadedProject
   { packageOfModule :: M.Map Module PackageName
   , programs        :: [Program Parsed]
   }

loadProject :: ProjectOptions -> ExceptT [XastError] IO LoadedProject
loadProject options = do
   packages <- loadPackages options.stdDirectory options.directory
   packageOfModule <- liftEither (modulePackages packages)
   programs <- concat <$> mapM parsePackage packages
   pure LoadedProject
      { packageOfModule = packageOfModule
      , programs        = map (withPrelude packageOfModule) programs
      }

analyzeProject :: ProjectOptions -> ExceptT [XastError] IO AnalysisResult
analyzeProject options = do
   loaded <- loadProject options
   -- FIXME: Target64 only
   let analysis = fullAnalysis printWarnings ignoreSavedFile loaded.programs loaded.packageOfModule Target64
   result <- liftIO (runExceptT analysis)
   case result of
      Left errors    -> throwError (map XastSemAnalyzeError errors)
      Right analyzed -> pure analyzed

-- | The analysis may save files; nothing uses that yet.
ignoreSavedFile :: FilePath -> String -> IO ()
ignoreSavedFile _ _ = pure ()

-- | What the backend produces from an analysed project.
data Generated = Generated
   { kira     :: [Kira]
   , programs :: [CProgram]
   }

generate :: AnalysisResult -> Generated
generate analysis =
   let (lowered, typeDefs, intrinsicDefs) = lowerPrograms analysis
       erased = eraseZeroSized lowered typeDefs
       programs = codegen erased.programs erased.types erased.ecsTags intrinsicDefs
   in Generated { kira = erased.programs, programs = programs }

-- | Every module outside Std implicitly imports Std.Prelude.
withPrelude :: M.Map Module PackageName -> Program Parsed -> Program Parsed
withPrelude packageOfModule program =
   let inStd = M.lookup program.moduleDef.name packageOfModule == Just stdPackageName
   in if inStd
      then program
      else program { imports = preludeImport program : program.imports }

-- #### OUTPUT ####

-- | Writes each program to its path under the output directory,
--   e.g. @build/src/Main.h@.
writePrograms :: FilePath -> [CProgram] -> IO ()
writePrograms outputDirectory programs = forM_ programs $ \program -> do
   let file = outputDirectory </> program.path
   createDirectoryIfMissing True (takeDirectory file)
   writeFile file (show (prettyProgram program))

-- | Compiles a file including every generated header, without linking,
--   e.g. @gcc -fsyntax-only -x c -I build -@.
checkGeneratedCode
   :: String          -- ^ the C compiler, e.g. @gcc@
   -> Maybe FilePath  -- ^ @--flecs@
   -> FilePath        -- ^ the output directory
   -> [CProgram]
   -> ExceptT [XastError] IO ()
checkGeneratedCode compiler flecsOption outputDirectory programs = do
   flecsDirectory <- liftIO (findFlecs flecsOption)
   includeDirectories <- liftIO $ mapM makeAbsolute (outputDirectory : maybeToList flecsDirectory)
   let arguments = ["-fsyntax-only", "-x", "c"] ++ map ("-I" <>) includeDirectories ++ ["-"]
   let source = concatMap includeLine programs
   result <- liftIO $ try (readProcessWithExitCode compiler arguments source)
   case result of
      Left (_ :: IOException) -> throwError [XastCompilerNotFound compiler]
      Right (exitCode, out, err) -> do
         liftIO $ putStr out >> hPutStr stderr err
         unless (exitCode == ExitSuccess) $
            throwError [XastGeneratedCodeRejected compiler outputDirectory]

-- | E.g. @#include "src/Main.h"@.
includeLine :: CProgram -> String
includeLine program = "#include \"" <> program.path <> "\"\n"

-- | The directory with @flecs.h@: @--flecs@, otherwise @XAST_FLECS@.
findFlecs :: Maybe FilePath -> IO (Maybe FilePath)
findFlecs flecsOption = do
   fromEnvironment <- lookupEnv "XAST_FLECS"
   pure (flecsOption <|> fromEnvironment)

-- | Prints each module, or only the one asked for.
dumpModules :: Show a => Maybe Module -> (a -> Module) -> [a] -> IO ()
dumpModules wanted moduleOf items =
   forM_ items $ \item ->
      when (isWanted wanted (moduleOf item)) $ do
         putStrLn (show (bold (cyan (show (moduleOf item)))) <> ":")
         putStrLn (TL.unpack (pShowNoColor item))

-- | Whether to print a module: every one without @--module@.
isWanted :: Maybe Module -> Module -> Bool
isWanted wanted module' = maybe True (== module') wanted

-- | The programs of one module, e.g. @src/Main.h@ for @Main@, or all of them.
selectPrograms :: Maybe Module -> [CProgram] -> [CProgram]
selectPrograms wanted programs = case wanted of
   Nothing      -> programs
   Just module' -> filter (\program -> program.path == moduleToPath module' ".h") programs

-- | Prints the errors or the success line, e.g. "Check completed with 2 warnings."
report :: String -> ExceptT [XastError] IO Int -> IO ExitCode
report what action = runExceptT action >>= \case
   Left errors -> do
      forM_ errors printError
      putStrLn
         (  show (red (bold (what <> " failed with ")))
         ++ show (yellow (bold (show (length errors))))
         ++ show (red (bold " errors."))
         )
      pure (ExitFailure 1)

   Right 0 -> do
      print $ green $ bold (what <> " completed.")
      pure ExitSuccess

   Right warnings -> do
      putStrLn
         (  show (green (bold (what <> " completed ")))
         ++ show (yellow (bold ("with " <> show warnings <> " warnings")))
         ++ show (green (bold "."))
         )
      pure ExitSuccess
