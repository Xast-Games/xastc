module Main where

import Data.Text (pack, unpack)
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (exitWith)

import Paths_xastc (version)
import Xast.AST (Module)
import Xast.Config (parseModule)
import Xast.Pipeline

data Command
   = Check ProjectOptions
   | Build BuildOptions
   | Dump DumpOptions

main :: IO ()
main = do
   chosen <- execParser commandLine
   exitCode <- case chosen of
      Check options -> runCheck options
      Build options -> runBuild options
      Dump options  -> runDump options
   exitWith exitCode

commandLine :: ParserInfo Command
commandLine =
   let description = progDesc "The Xast compiler"
   in info (commands <**> helper <**> versionOption) (fullDesc <> description)

-- | @xastc --version@, e.g. @xastc 0.1.0.0@.
versionOption :: Parser (a -> a)
versionOption = simpleVersioner ("xastc " <> showVersion version)

commands :: Parser Command
commands = hsubparser
   (  command "check" (info (Check <$> projectOptions) (progDesc "Parse and analyse the project"))
   <> command "build" (info (Build <$> buildOptions) (progDesc "Generate C and check it with a C compiler"))
   <> command "dump"  (info (Dump <$> dumpOptions) (progDesc "Print one stage of the compiler"))
   )

-- #### OPTIONS ####

projectOptions :: Parser ProjectOptions
projectOptions = ProjectOptions
   <$> projectArgument
   <*> optional stdOption

buildOptions :: Parser BuildOptions
buildOptions = BuildOptions
   <$> projectOptions
   <*> optional outputOption
   <*> compilerOption
   <*> optional flecsOption

-- | @xastc dump STAGE [DIR]@, e.g. @xastc dump c examples/Shapes -m Main@.
dumpOptions :: Parser DumpOptions
dumpOptions = DumpOptions
   <$> stageArgument
   <*> projectOptions
   <*> optional moduleOption

projectArgument :: Parser FilePath
projectArgument = strArgument
   (  metavar "DIR"
   <> value "."
   <> showDefault
   <> help "The project directory"
   )

stdOption :: Parser FilePath
stdOption = strOption
   (  long "std"
   <> metavar "DIR"
   <> help "Std's directory (default: XAST_STD, then next to xastc)"
   )

outputOption :: Parser FilePath
outputOption = strOption
   (  long "out"
   <> short 'o'
   <> metavar "DIR"
   <> help "Where to write the C files (default: DIR/build)"
   )

-- | @--cc PROGRAM@ picks the C compiler, @--no-cc@ skips the check.
compilerOption :: Parser (Maybe String)
compilerOption =
   let chosen = strOption
         (  long "cc"
         <> metavar "PROGRAM"
         <> value "gcc"
         <> showDefault
         <> help "The C compiler that checks the output"
         )
       skipped = flag' Nothing (long "no-cc" <> help "Do not check the generated C")
   in skipped <|> (Just <$> chosen)

flecsOption :: Parser FilePath
flecsOption = strOption
   (  long "flecs"
   <> metavar "DIR"
   <> help "The directory with flecs.h (default: XAST_FLECS)"
   )

stageArgument :: Parser DumpStage
stageArgument = argument (eitherReader readStage)
   (  metavar "STAGE"
   <> help "parsed, desugared, kira or c"
   )

moduleOption :: Parser Module
moduleOption = option (eitherReader readModule)
   (  long "module"
   <> short 'm'
   <> metavar "MODULE"
   <> help "Print only this module, e.g. Main"
   )

readStage :: String -> Either String DumpStage
readStage stage = case stage of
   "parsed"    -> Right DumpParsed
   "desugared" -> Right DumpDesugared
   "kira"      -> Right DumpKira
   "c"         -> Right DumpC
   other       -> Left ("unknown stage `" <> other <> "`, expected parsed, desugared, kira or c")

-- | E.g. @Std.Option@.
readModule :: String -> Either String Module
readModule text = case parseModule (pack text) of
   Right module' -> Right module'
   Left message  -> Left (unpack message)
