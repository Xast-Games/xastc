{-# OPTIONS_GHC -Wno-orphans #-}
module Xast.Error.Pretty where

import Control.Monad (forM_, unless)
import Data.List (intercalate)
import Data.Text (Text, unpack, pack)
import Data.Void (Void)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (HasHints (hints), errorDiagnosticFromBundle)
import Text.Megaparsec
import Toml (prettyTomlDecodeError)

import Xast.Error.Types
import Xast.AST
import Xast.Utils.Pretty 
import qualified Xast.Utils.List as L

instance HasHints Void String where
   hints :: Void -> [Note String]
   hints _ = []

class PrintError a where
   printError :: a -> IO ()

instance PrintError XastError where
   printError :: XastError -> IO ()
   printError (XastSemAnalyzeError e) = printError e
   printError (XastParseError bundle) = printError bundle
   printError (XastFileNotFound file dir) = do
      let msg = "File `" <> file <> "` not found in directory: " <> dir
      let report = Err Nothing msg [] []
      let diagnostic = addReport mempty report
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic

   printError (XastModuleNotFound module_ dir) = do
      let msg = "Module `" <> show module_ <> "` not found at path: " <> dir <> "/" <> moduleToPath module_ ".xst"
      let report = Err Nothing msg [] []
      let diagnostic = addReport mempty report
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic

   printError (XastTomlDecodeError file e) = do
      let msg = pack (show (yellow (file <> ": "))) <> prettyTomlDecodeError e
      let report = Err Nothing msg [] []
      let diagnostic = addReport mempty report
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic

instance PrintError (ParseErrorBundle Text Void) where
   printError :: ParseErrorBundle Text Void -> IO ()
   printError bundle = do
      let diagnostic = errorDiagnosticFromBundle Nothing "Parsing error" Nothing bundle
          filename = sourceName . pstateSourcePos . bundlePosState $ bundle
          sourceText = unpack . pstateInput . bundlePosState $ bundle
          diagnosticWithFile = addFile diagnostic filename sourceText
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnosticWithFile

instance PrintError SemError where
   printError :: SemError -> IO ()
   printError (SESelfImportError module_ from to) =
      let Location fromPos _ fromLen = from
          Location toPos _ toLen = to
          filename = sourceName fromPos
          report = errReport
            ("Found self-referencing import in module: " <> highlightModule module_)
            [ (toPosition fromPos fromLen filename, Where "Module is defined here")
            , (toPosition toPos toLen filename, This "Module imports itself here")
            ]
            [Hint "A module cannot import itself. Remove this import statement."]

      in printReportAt filename report

   printError (SECyclicImportError modules loc) =
      let Location pos _ len = loc
          filename = sourceName pos
          cycleT = show $ red $ bold $ intercalate " ─▶ " (map show modules)
          report = errReport
            ("Found cyclical import: " <> cycleT)
            [ (toPosition pos len filename, Where "Module is defined here") ]
            []

     in printReportAt filename report

   printError (SEMissingModule module_ loc) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Trying to import a missing module: " <> highlightModule module_)
            [ (toPosition pos len filename, This "Imported module does not exist") ]
            []

      in printReportAt filename report

   printError (SEInvalidExport module_ loc ids) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ( "Invalid exported symbols in module " <> highlightModule module_ <> ": " 
               <> show (green (intercalate ", " (map show ids)))
            )
            [(toPosition pos len filename, This "This export is invalid")]
            []

      in printReportAt filename report

   printError (SEMissingImports module_ loc ids) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ( "Imported symbols are not found in module " <> highlightModule module_ <> ": " 
               <> intercalate ", " (map show ids)
            )
            [(toPosition pos len filename, This "This import contains missing symbols")]
            []

      in printReportAt filename report

   printError (SEPrivateImports module_ loc ids) =
      let Location pos _ len = loc
          filename = sourceName pos
          imports = intercalate ", " (map show ids)
          report = errReport
            ( "Imported symbols in module " <> highlightModule module_ <> " are private: " 
               <> imports
            )
            [(toPosition pos len filename, This "This import contains private symbols")]
            [Hint ("Add " <> imports <> " to the list of exported symbols of module " <> show (blue (bold (show module_))))]

      in printReportAt filename report

   printError (SEAmbiguousAlias alias locA locB) =
      let Location posA _ lenA = locA
          Location posB _ lenB = locB
          filename = sourceName posA
          report = errReport
            ("Ambiguous module import aliases found: " <> show (blue (show alias)))
            [ (toPosition posA lenA filename, Where "First module alias imported here")
            , (toPosition posB lenB filename, Where "Second module alias imported here")
            ]
            []

      in printReportAt filename report

   printError (SEAmbiguousImport ident locA locB) =
      let Location posA _ lenA = locA
          Location posB _ lenB = locB
          filename = sourceName posA
          report = errReport
            ("Ambiguous unqualified import: " <> show (blue (show ident)))
            [ (toPosition posA lenA filename, Where "First import of this symbol")
            , (toPosition posB lenB filename, Where "Second import of this symbol")
            ]
            [ Hint "Use module qualification to disambiguate" ]

      in printReportAt filename report

   printError (SEImportDeclConflict ident impLoc declLoc) =
      let Location posImp _ lenImp = impLoc
          Location posDecl _ lenDecl = declLoc
          filename = sourceName posImp
          report = errReport
            ("Imported name conflicts with local declaration: " <> show (blue (show ident)))
            [ (toPosition posImp lenImp filename, Where "Symbol imported here")
            , (toPosition posDecl lenDecl filename, Where "Symbol declared locally here")
            ]
            [ Hint "Rename the local declaration or use qualified imports" ]

      in printReportAt filename report

   printError (SECtorRedeclaration ident oldLoc newLoc) =
      let Location posOld _ lenOld = oldLoc
          Location posNew _ lenNew = newLoc
          filename = sourceName posNew
          report = errReport
            ("Constructor redeclared: " <> show (blue (show ident)))
            [ (toPosition posOld lenOld filename, Where "Previous declaration here")
            , (toPosition posNew lenNew filename, This "Redeclared here")
            ]
            []

      in printReportAt filename report

   printError (SEUndefinedVar loc ident) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Undefined variable: " <> show (blue (show ident)))
            [ (toPosition pos len filename, This "Not found in local/module/import scope") ]
            []

      in printReportAt filename report

   printError (SEUndefinedCon loc ident) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Undefined constructor: " <> show (blue (show ident)))
            [ (toPosition pos len filename, This "Constructor is not in scope") ]
            []

      in printReportAt filename report

   printError (SEUndefinedType loc ident) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Undefined type: " <> show (blue (show ident)))
            [ (toPosition pos len filename, This "Not declared in this module or brought in by an import") ]
            []

      in printReportAt filename report

   printError (SEFnArityMismatch loc ident expected actual) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ( "Function `" <> show ident <> "` expects " <> show expected
               <> " argument(s), but got " <> show actual
            )
            [ (toPosition pos len filename, This "Called/implemented with the wrong number of arguments") ]
            []

      in printReportAt filename report

   printError (SESystemArityMismatch loc ident expected actual) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ( "System `" <> show ident <> "` expects " <> show expected
               <> " pattern(s) here, but got " <> show actual
            )
            [ (toPosition pos len filename, This "Wrong number of patterns") ]
            []

      in printReportAt filename report

   printError (SEUnknownField loc con fld) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Constructor " <> show (blue (show con)) <> " has no field: " <> show (blue (show fld)))
            [ (toPosition pos len filename, This "Unknown field") ]
            []

      in printReportAt filename report

   printError (SENotARecordType loc ty) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Cannot access fields on non-record type: " <> show ty)
            [ (toPosition pos len filename, This "Not a record type") ]
            []

      in printReportAt filename report

   printError (SEAmbiguousRecordAccess loc conIdent otherCtors) =
      let Location pos _ len = loc
          filename = sourceName pos
          othersList = intercalate ", " (map show otherCtors)
          report = errReport
            ("Cannot access/update fields of " <> show (blue (show conIdent)) <> ": its type has other constructors too (" <> othersList <> ")")
            [ (toPosition pos len filename, This "Ambiguous field access on a multi-constructor type") ]
            [ Hint "Field getters and record update syntax only work when the record variant is the only constructor of its type. Pattern-match instead to pick out the fields for this variant." ]

      in printReportAt filename report

   printError (SEInvalidTupleIndex loc ty idx) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Invalid tuple index " <> show idx <> " on type: " <> show ty)
            [ (toPosition pos len filename, This "Invalid tuple index") ]
            []

      in printReportAt filename report

   printError (SENonExhaustiveMatch loc tyIdent missingCtors) =
      let Location pos _ len = loc
          filename = sourceName pos
          missingList = intercalate ", " (map show missingCtors)
          report = errReport
            ("Non-exhaustive match on " <> show (blue (show tyIdent)) <> ": missing " <> missingList)
            [ (toPosition pos len filename, This "Not every constructor is covered here") ]
            [ Hint "Add arms for the missing constructors, add a wildcard/variable arm, or mark the type @NonExhaustive." ]

      in printReportAt filename report

   printError (SEUndefinedAlias filename alias) =
      let report = errReport
            ("Undefined module alias: " <> show (blue (show alias)))
            []
            [Hint "Add `use Module as Alias` or use an existing alias"]

      in printReportAt filename report

   printError (SETypeRedeclaration ident oldLoc newLoc) =
      redeclarationError "Type" ident oldLoc newLoc

   printError (SEFnRedeclaration ident oldLoc newLoc) =
      redeclarationError "Function" ident oldLoc newLoc

   printError (SEExternFnRedeclaration ident oldLoc newLoc) =
      redeclarationError "Extern function" ident oldLoc newLoc

   printError (SEExternTypeRedeclaration ident oldLoc newLoc) =
      redeclarationError "Extern type" ident oldLoc newLoc

   printError (SESystemRedeclaration ident oldLoc newLoc) =
      redeclarationError "System" ident oldLoc newLoc

   printError (SEDuplicateBinding ident oldLoc newLoc) =
      redeclarationError "Binding" ident oldLoc newLoc

   printError (SEShadowedBinding ident outerLoc newLoc) =
      shadowingError ident outerLoc newLoc

   printError (SEMissingFnDef loc ident) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Function `" <> show ident <> "` is implemented but never declared")
            [(toPosition pos len filename, This "No matching function declaration found")]
            [Hint "Add a function declaration for this implementation"]

      in printReportAt filename report

   printError (SEExtraFnDef loc ident others) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Function `" <> show ident <> "` has multiple implementations")
            ( (toPosition pos len filename, This "Redundant implementation here")
            : [ (toPosition p l filename, Where "Also implemented here") | Location p _ l <- others ]
            )
            []

      in printReportAt filename report

   printError (SEMissingFnImpls loc ident) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Function `" <> show ident <> "` is declared but never implemented")
            [(toPosition pos len filename, This "Declared here but never implemented")]
            [Hint "Add an implementation for this function"]

      in printReportAt filename report

   printError (SEMissingSystemDef loc ident) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("System `" <> show ident <> "` is implemented but never declared")
            [(toPosition pos len filename, This "No matching system declaration found")]
            [Hint "Add a system declaration for this implementation"]

      in printReportAt filename report

   printError (SEExtraSystemDef loc ident others) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("System `" <> show ident <> "` has multiple implementations")
            ( (toPosition pos len filename, This "Redundant implementation here")
            : [ (toPosition p l filename, Where "Also implemented here") | Location p _ l <- others ]
            )
            []

      in printReportAt filename report

   printError (SEMissingSystemImpls loc ident) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("System `" <> show ident <> "` is declared but never implemented")
            [(toPosition pos len filename, This "Declared here but never implemented")]
            [Hint "Add an implementation for this system"]

      in printReportAt filename report

   printError (SETypeError loc expected actual) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ( "Type mismatch: expected `" <> show (green (typename expected))
               <> "`, but got `" <> show (red (typename actual)) <> "`"
            )
            [(toPosition pos len filename, This "This expression has the wrong type")]
            []

      in printReportAt filename report

   printError (SEListElementTypeMismatch locA tyA locB tyB) =
      let Location posA _ lenA = locA
          Location posB _ lenB = locB
          filename = sourceName posA
          report = errReport
            "List elements have mismatched types"
            [ (toPosition posA lenA filename, Where ("Previous element has type `" <> typename tyA <> "`"))
            , (toPosition posB lenB filename, This ("This element has type `" <> typename tyB <> "`, expected `" <> typename tyA <> "`"))
            ]
            []

      in printReportAt filename report

   printError (SEThenElseTypeMismatch locThen tyThen locElse tyElse) =
      let Location posThen _ lenThen = locThen
          Location posElse _ lenElse = locElse
          filename = sourceName posThen
          report = errReport
            "The branches of this `if` have different types"
            [ (toPosition posThen lenThen filename, Where ("The `then` branch has type `" <> typename tyThen <> "`"))
            , (toPosition posElse lenElse filename, This ("The `else` branch has type `" <> typename tyElse <> "`, expected `" <> typename tyThen <> "`"))
            ]
            []

      in printReportAt filename report

   printError (SEInfiniteType loc tyVar ty) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ( "Cannot construct infinite type: t" <> show tyVar <> " ~ " <> typename ty )
            [(toPosition pos len filename, This "Infinite type detected here")]
            [Hint "Check for a self-referencing definition"]

      in printReportAt filename report

   printError (SETooManyArgs loc ty) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Too many arguments applied to a value of type `" <> typename ty <> "`")
            [(toPosition pos len filename, This "Extra argument(s) here")]
            []

      in printReportAt filename report

   printError (SENotAFunction loc ty) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ("Cannot apply arguments to a non-function value of type `" <> typename ty <> "`")
            [(toPosition pos len filename, This "This value is not a function")]
            []

      in printReportAt filename report

   printError (SECtorArityMismatch loc ident expected actual) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ( "Constructor `" <> show ident <> "` expects " <> show expected
               <> " argument(s), but got " <> show actual
            )
            [(toPosition pos len filename, This "Called with the wrong number of arguments")]
            []

      in printReportAt filename report
   
   printError (SEIntegerOutOfBounds loc kind value (low, high)) =
      let Location pos _ len = loc
          filename = sourceName pos
          report = errReport
            ( "`" <> show kind <> "` integer literal value `" <> show value <> "` is out of bounds " )
            [ ( toPosition pos len filename
             , This 
               ( "This value is of type `" <> show kind <> "` and its bounds are "
               <> show low <> ".." <> show high
               )
              )
            ]
            []

      in printReportAt filename report

redeclarationError :: String -> Ident -> Location -> Location -> IO ()
redeclarationError kind ident oldLoc newLoc =
   let Location posOld _ lenOld = oldLoc
       Location posNew _ lenNew = newLoc
       filename = sourceName posNew
       report = errReport
         (kind <> " redeclared: " <> show (blue (show ident)))
         [ (toPosition posOld lenOld filename, Where "Previous declaration here")
         , (toPosition posNew lenNew filename, This "Redeclared here")
         ]
         []

   in printReportAt filename report

shadowingError :: Ident -> Location -> Location -> IO ()
shadowingError ident outerLoc newLoc =
   let Location posOld _ lenOld = outerLoc
       Location posNew _ lenNew = newLoc
       filename = sourceName posNew
       report = errReport
         ("Shadowing is not allowed: " <> show (blue (show ident)))
         [ (toPosition posOld lenOld filename, Where "Already bound here")
         , (toPosition posNew lenNew filename, This "Cannot shadow it here")
         ]
         []

   in printReportAt filename report

printWarnings :: [SemWarning] -> IO ()
printWarnings warns = forM_ warns printWarning

printWarning :: SemWarning -> IO ()
printWarning (SWRedundantImport intr) = case intr of
   InterModule (Located (Location pos _ len) module_) ->
      let filename = sourceName pos
          report = warnReport
            ("Redundant module import: " <> highlightModule module_)
            [ (toPosition pos len filename, Where "Module is imported here")
            ]
            []

      in printReportAt filename report

   InterSelect module_ xs -> unless (null xs) $
      let dat = L.forEach xs $
            \(Located loc ident) ->
               let Location pos _ len = loc
                   fname = sourceName pos
               in (ident, (toPosition pos len fname, Blank))
          filename = sourceName (head xs).location.pos
          report =
            Warn
            Nothing
            ( "Redundant imports in module " <> highlightModule module_ <> ": "
               <> intercalate ", " (map (show . fst) dat)
            )
            (map snd dat)
            [ Hint $ "Remove redundant imports: "
               <> mark q
               <> intercalate (mark q <> ", " <> mark q) (map (mark . show . fst) dat)
               <> mark q
            ]
            where
               mark = show . yellow
               q = "\""

      in printReportAt filename report

printWarning (SWFunctionGayness (Location pos _ len) ident args ) =
   let filename = sourceName pos
       report = warnReport
         ("Function `" <> show ident <> "` is too gay")
         [ (toPosition pos len filename, Where $ "This function has " <> show args <> " parameters")
         ]
         [ Hint "Consider reducing its gayness"]

   in printReportAt filename report

printWarning (SWUnusedImport module_) =
   putStrLn $ show (bold (yellow "warning: ")) <> "Unused import: " <> highlightModule module_

printWarning (SWDeadCode ident) =
   putStrLn $ show (bold (yellow "warning: ")) <> "Unused declaration: " <> show (blue (show ident))

highlightModule :: Module -> String
highlightModule = show . blue . show

errReport :: String -> [(Position, Marker String)] -> [Note String] -> Report String
errReport = Err Nothing

warnReport :: String -> [(Position, Marker String)] -> [Note String] -> Report String
warnReport = Warn Nothing

printReportAt :: FilePath -> Report String -> IO ()
printReportAt filename report = do
   file <- readFile filename
   let diagnostic = addFile mempty filename file
   printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle $ addReport diagnostic report

toPosition :: SourcePos -> Int -> FilePath -> Position
toPosition (SourcePos _ line col) len filename =
   let startLine = unPos line
       startCol = unPos col
       endLine = startLine
       endCol = startCol + len
   in Position (startLine, startCol) (endLine, endCol) filename