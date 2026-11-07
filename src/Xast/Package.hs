{-# LANGUAGE OverloadedStrings #-}
module Xast.Package where

import Control.Monad (unless, filterM, foldM)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (partitionEithers)
import Data.Text (pack)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), takeDirectory)
import qualified Toml

import Xast.AST
import Xast.Config (XastConfig (..), ProjectConfig (..), Dependency (..), xastConfigCodec)
import Xast.Error.Types (XastError (..))
import Xast.Parser.Program (parseProgram)
import Xast.Utils.Generic ((|>))

-- | A project or one of its dependencies.
data Package = Package
   { name      :: PackageName
   , directory :: FilePath
   , config    :: XastConfig
   }

preludeImport :: Program Parsed -> Located ImportDef
preludeImport prog = Located
   { location = prog.moduleDef.location
   , node = ImportDef
      { importModule = Module [Ident "Std", Ident "Prelude"]
      , payload      = ImpFull
      } 
   }

-- | The project and every package it depends on, dependencies first.
--   Every package except std depends on std.
loadPackages
   :: FilePath  -- ^ the project
   -> ExceptT [XastError] IO [Package]
loadPackages projectDirectory = do
   project <- loadPackage projectDirectory
   stdDirectory <- liftIO findStd
   stdExists <- liftIO $ doesFileExist (stdDirectory </> "xast.toml")
   unless (stdExists || project.name == stdPackageName) $
      throwError [XastStdNotFound stdDirectory]
   visitPackage stdDirectory [] [] project

-- | Std's directory: @XAST_STD@ if set, otherwise @std@ next to the xastc binary.
findStd :: IO FilePath
findStd = do
   override <- lookupEnv "XAST_STD"
   executable <- getExecutablePath
   let besideBinary = takeDirectory executable </> "std"
   pure (fromMaybe besideBinary override)

-- | Adds a package after its dependencies.
visitPackage
   :: FilePath       -- ^ Std
   -> [PackageName]  -- ^ packages being visited, innermost first
   -> [Package]      -- ^ packages loaded so far
   -> Package
   -> ExceptT [XastError] IO [Package]
visitPackage stdDirectory visiting loaded package
   | package.name `elem` visiting =
      throwError [XastPackageCycle (cycleOf package.name visiting)]
   | any (\other -> other.name == package.name) loaded =
      pure loaded
   | otherwise = do
      dependencies <- mapM loadPackage (dependencyDirectories stdDirectory package)
      let visitDependency = visitPackage stdDirectory (package.name : visiting)
      loadedWithDependencies <- foldM visitDependency loaded dependencies
      pure (loadedWithDependencies ++ [package])

-- | The cycle a package closes, e.g. @[a, b, a]@ when @a@ is reached again
--   while visiting @[b, a]@.
cycleOf
   :: PackageName
   -> [PackageName]  -- ^ packages being visited, innermost first
   -> [PackageName]
cycleOf package visiting =
   let between = visiting
         |> takeWhile (/= package)
         |> reverse
   in [package] ++ between ++ [package]

-- | Directories of a package's dependencies, Std included.
dependencyDirectories :: FilePath -> Package -> [FilePath]
dependencyDirectories stdDirectory package =
   let dependencies = package.config.dependencies
       listed = dependencies
         |> M.elems
         |> map (\dependency -> package.directory </> dependency.path)
       needsStd = package.name /= stdPackageName && M.notMember stdPackageName dependencies
   in [stdDirectory | needsStd] ++ listed

-- | Loads the xast.toml of the package in a directory.
loadPackage :: FilePath -> ExceptT [XastError] IO Package
loadPackage directory = do
   let configFile = directory </> "xast.toml"
   configFileExists <- liftIO $ doesFileExist configFile
   unless configFileExists $
      throwError [XastFileNotFound "xast.toml" directory]

   decoded <- Toml.decodeFileEither xastConfigCodec configFile
   config <- case decoded of
      Left errors -> throwError (XastTomlDecodeError configFile <$> errors)
      Right found -> pure found

   pure Package
      { name      = config.projectConfig.name
      , directory = directory
      , config    = config
      }

-- | The package of every module. Two packages may not declare the same module.
modulePackages :: [Package] -> Either [XastError] (M.Map Module PackageName)
modulePackages packages =
   let entries =
         [ (module', package.name)
         | package <- packages
         , module' <- package.config.projectConfig.modules
         ]
   in foldM addModulePackage M.empty entries

addModulePackage
   :: M.Map Module PackageName
   -> (Module, PackageName)
   -> Either [XastError] (M.Map Module PackageName)
addModulePackage known (module', package) = case M.lookup module' known of
   Just other | other /= package -> Left [XastDuplicateModule module' other package]
   _                             -> Right (M.insert module' package known)

-- | Parses every module of a package.
parsePackage :: Package -> ExceptT [XastError] IO [Program Parsed]
parsePackage package = do
   let modules = package.config.projectConfig.modules
   missingModules <- liftIO $ filterM
      (\module' -> not <$> doesFileExist (package.directory </> moduleToPath module' ".xst"))
      modules

   case missingModules of
      (module' : _) -> throwError [XastModuleNotFound module' package.directory]
      []            -> pure ()

   results <- liftIO $ traverse (parseOne package.directory) modules
   let (errors, programs) = partitionEithers results
   unless (null errors) $
      throwError errors
   pure programs

parseOne :: FilePath -> Module -> IO (Either XastError (Program Parsed))
parseOne directory module_ = runExceptT $ do
   let filepath = directory </> moduleToPath module_ ".xst"
   code <- liftIO $ readFile filepath
   program <- ExceptT $ pure $ parseProgram filepath (pack code)
   checkModuleName module_ program
   pure program

-- | A module must be declared in the file its name points to,
--   e.g. module @MyModule.MyUtils@ in @src/MyModule/MyUtils.xst@.
checkModuleName
   :: Monad m
   => Module          -- ^ the module the file was loaded for
   -> Program Parsed
   -> ExceptT XastError m ()
checkModuleName expected program =
   let declared = program.moduleDef.name
       mismatch = XastModuleNameMismatch
         { location = program.moduleDef.location
         , expected = expected
         , declared = declared
         }
   in unless (declared == expected) $
         throwError mismatch
