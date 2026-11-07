{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Config where

import Data.Text  
import GHC.Generics (Generic)
import Toml
import Xast.AST
import Text.Megaparsec (runParser, errorBundlePretty)
import Xast.Parser.Headers (module')
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data XastConfig = XastConfig
   { projectConfig   :: ProjectConfig
   , apiConfig       :: ApiConfig
   , dependencies    :: M.Map PackageName Dependency  -- ^ Std is not listed: every package but std gets it
   }
   deriving (Eq, Show, Generic)

xastConfigCodec :: TomlCodec XastConfig
xastConfigCodec = XastConfig
   <$> Toml.table projectConfigCodec "project"  .= (.projectConfig)
   <*> Toml.table apiConfigCodec     "api"      .= (.apiConfig)
   <*> dependenciesCodec "dependencies"         .= (.dependencies)

-- | A package this one uses, e.g. @physics = { path = "../physics" }@.
newtype Dependency = Dependency
   { path :: FilePath  -- ^ relative to the package that depends on it
   }
   deriving (Eq, Show, Generic)

-- | An optional table of dependencies, empty when missing.
dependenciesCodec :: Toml.Key -> TomlCodec (M.Map PackageName Dependency)
dependenciesCodec key =
   let dependencyCodec = Dependency <$> Toml.string "path" .= (.path)
       tableCodec = Toml.tableMap Toml._KeyText (Toml.table dependencyCodec) key
   in Toml.dimap Just (fromMaybe M.empty) (Toml.dioptional tableCodec)

data ProjectConfig = ProjectConfig
   { name      :: Text
   , version   :: ProjectVersion
   , author    :: Maybe Text
   , modules   :: [Module]
   }
   deriving (Eq, Show, Generic)

projectConfigCodec :: TomlCodec ProjectConfig
projectConfigCodec = ProjectConfig
   <$> Toml.text                  "name"    .= (.name)
   <*> projectVersionCodec        "version" .= (.version)
   <*> Toml.dioptional (Toml.text "author") .= (.author)
   <*> projectModulesCodec        "modules" .= (.modules)

projectModulesCodec :: Toml.Key -> TomlCodec [Module]
projectModulesCodec = Toml.arrayOf (Toml._TextBy showModule parseModule) 

showModule :: Module -> Text
showModule = T.pack . show

parseModule :: Text -> Either Text Module
parseModule s = 
   let result = runParser module' "filename" s
   in Bifunctor.first (T.pack . errorBundlePretty) result

data ProjectVersion = ProjectVersion Int Int Int
   deriving Eq

instance Show ProjectVersion where
   show :: ProjectVersion -> String
   show (ProjectVersion maj minv pat) =
      show maj <> "." <> show minv <> "." <> show pat

projectVersionCodec :: Toml.Key -> TomlCodec ProjectVersion
projectVersionCodec = Toml.textBy showVersion parseVersion

showVersion :: ProjectVersion -> Text
showVersion = T.pack . show

parseVersion :: Text -> Either Text ProjectVersion
parseVersion s = 
   case split (=='.') s of
      [majT, minT, patT] -> do
         maj   <- parsePart "major" majT
         minv  <- parsePart "minor" minT
         pat   <- parsePart "patch" patT
         return $ ProjectVersion maj minv pat

      _ -> 
         Left $ "invalid version: " <> s

parsePart :: Text -> Text -> Either Text Int
parsePart name t =
   case reads (unpack t) of
      [(v, "")] -> Right v
      _ -> Left $
         "invalid " <> name <> " version: " <> t

data ApiConfig = ApiConfig
   { labels :: [Text]
   , components :: [Text]
   }
   deriving (Eq, Show, Generic)

apiConfigCodec :: TomlCodec ApiConfig
apiConfigCodec = ApiConfig
   <$> Toml.arrayOf Toml._Text "labels"     .= (.labels)
   <*> Toml.arrayOf Toml._Text "components" .= (.components)