{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SemAnalyzer where

import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Data.Either (lefts, rights)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Test.HUnit
import Xast.Error.Types
import Xast.Parser.Program (parseProgram)
import Xast.SemAnalyzer.Pass (fullAnalysis)
import Xast.SemAnalyzer.Types (AnalysisResult)
import Xast.Utils.Compiler (Target (Target64))

tests :: Test
tests = TestList
   [ TestLabel "Accepted programs"    acceptedTests
   , TestLabel "Declarations"         declarationTests
   , TestLabel "Imports"              importTests
   , TestLabel "Name resolution"      nameResolutionTests
   , TestLabel "Type checking"        typeCheckingTests
   , TestLabel "Match exhaustiveness" exhaustivenessTests
   , TestLabel "Systems"              systemTests
   ]

acceptedTests :: Test
acceptedTests = TestList
   [ named "constant function" $ assertAnalyzes $ inModule
      [ "fn f () -> Int;"
      , "fn f = 1;"
      ]

   , named "arithmetic through an extern function" $ assertAnalyzes $ inModule
      [ "fn f (Int) -> Int;"
      , "fn f x = x + 1;"
      ]

   , named "function with several clauses" $ assertAnalyzes $ inModule
      [ "fn f (Bool) -> Int;"
      , "fn f True = 1;"
      , "fn f False = 0;"
      ]

   , named "record construction and field access" $ assertAnalyzes $ inModule
      [ "type P = P { x : Int };"
      , "fn f (P) -> Int;"
      , "fn f p = p.x;"
      , "fn g () -> P;"
      , "fn g = P { x = 1 };"
      ]

   , named "applied lambda" $ assertAnalyzes $ inModule
      [ "fn f () -> Int;"
      , "fn f = (.\\x -> x) 1;"
      ]

   , named "let bound function used at two types" $ assertAnalyzes $ inModule
      [ "fn f () -> Int;"
      , "fn f = let id = .\\x -> x, let a = id True, let b = id 12 in b;"
      ]

   , named "list literal" $ assertAnalyzes $ inModule
      [ "extern type List a;"
      , "fn f () -> Int;"
      , "fn f = match [1, 2] with _ -> 1;"
      ]
   ]

declarationTests :: Test
declarationTests = TestList
   [ named "type declared twice" $ assertRejected isTypeRedeclaration $ inModule
      [ "type Bool = A;" ]

   , named "constructor declared twice in a type" $ assertRejected isCtorRedeclaration $ inModule
      [ "type X = A | A;" ]

   , named "extern type declared twice" $ assertRejected isExternTypeRedeclaration $ inModule
      [ "extern type Int;" ]

   , named "extern function declared twice" $ assertRejected isExternFnRedeclaration $ inModule
      [ "extern fn opAdd(Int, Int) -> Int;" ]

   , named "function signature declared twice" $ assertRejected isFnRedeclaration $ inModule
      [ "fn g () -> Int;"
      , "fn g () -> Bool;"
      , "fn g = 1;"
      ]

   , named "implementation without a signature" $ assertRejected isMissingFnDef $ inModule
      [ "fn g = 1;" ]

   , named "signature without an implementation" $ assertRejected isMissingFnImpls $ inModule
      [ "fn g () -> Int;" ]
   ]

importTests :: Test
importTests = TestList
   [ named "import everything from another module" $ assertAnalyzes
      [ moduleSource
         [ "module A exports *"
         , "extern type Int;"
         , "fn inc (Int) -> Int;"
         , "fn inc x = x;"
         ]
      , moduleSource
         [ "module B exports *"
         , "use A *"
         , "fn f (Int) -> Int;"
         , "fn f x = inc x;"
         ]
      ]

   , named "import a list of names" $ assertAnalyzes
      [ moduleWithInc "A"
      , moduleSource
         [ "module B exports *"
         , "use A { Int, inc }"
         , "fn f (Int) -> Int;"
         , "fn f x = inc x;"
         ]
      ]

   , named "use a function through an alias" $ assertAnalyzes
      [ moduleWithInc "A"
      , moduleSource
         [ "module B exports *"
         , "use A { Int }"
         , "use A as M"
         , "fn f (Int) -> Int;"
         , "fn f x = M.inc x;"
         ]
      ]

   , named "module importing itself" $ assertRejected isSelfImport
      [ moduleSource
         [ "module A exports *"
         , "use A *"
         , "extern type Int;"
         ]
      ]

   , named "importing a module that does not exist" $ assertRejected isMissingModule
      [ moduleSource
         [ "module A exports *"
         , "use Nope *"
         , "extern type Int;"
         ]
      ]

   , named "modules importing each other" $ assertRejected isCyclicImport
      [ moduleSource ["module A exports *", "use B *", "extern type X;"]
      , moduleSource ["module B exports *", "use A *", "extern type Y;"]
      ]

   , named "importing a name the module does not have" $ assertRejected isMissingImports
      [ moduleWithInc "A"
      , moduleSource ["module B exports *", "use A { Nope }", "extern type Z;"]
      ]

   , named "importing a name the module does not export" $ assertRejected isPrivateImports
      [ moduleSource
         [ "module A exports { Int }"
         , "extern type Int;"
         , "extern type Hidden;"
         ]
      , moduleSource ["module B exports *", "use A { Hidden }", "extern type Z;"]
      ]

   , named "exporting a name that is not declared" $ assertRejected isInvalidExport
      [ moduleSource
         [ "module A exports { nope }"
         , "extern type Int;"
         ]
      ]

   , named "the same name imported from two modules" $ assertRejected isAmbiguousImport
      [ moduleWithInc "A"
      , moduleWithInc "A2"
      , moduleSource
         [ "module B exports *"
         , "use A *"
         , "use A2 *"
         , "fn f (Int) -> Int;"
         , "fn f x = inc x;"
         ]
      ]

   , named "an import clashing with a declaration" $ assertRejected isImportDeclConflict
      [ moduleWithInc "A"
      , moduleSource
         [ "module B exports *"
         , "use A *"
         , "extern type Int;"
         ]
      ]

   , named "using an alias that was never declared" $ assertRejected isUndefinedAlias
      [ moduleWithInc "A"
      , moduleSource
         [ "module B exports *"
         , "use A { Int }"
         , "use A as M"
         , "fn f (Int) -> Int;"
         , "fn f x = N.inc x;"
         ]
      ]

   , named "one alias for two modules" $ assertRejected isAmbiguousAlias
      [ moduleWithInc "A"
      , moduleWithInc "A2"
      , moduleSource
         [ "module B exports *"
         , "use A { Int }"
         , "use A as M"
         , "use A2 as M"
         , "fn f (Int) -> Int;"
         , "fn f x = M.inc x;"
         ]
      ]
   ]

nameResolutionTests :: Test
nameResolutionTests = TestList
   [ named "undefined variable" $ assertRejected isUndefinedVar $ inModule
      [ "fn f () -> Int;"
      , "fn f = y;"
      ]

   , named "undefined constructor" $ assertRejected isUndefinedCon $ inModule
      [ "fn f () -> Bool;"
      , "fn f = Nope;"
      ]

   , named "undefined type" $ assertRejected isUndefinedType $ inModule
      [ "fn f () -> Nope;"
      , "fn f = 1;"
      ]

   , named "the same name bound twice in one let" $ assertRejected isDuplicateBinding $ inModule
      [ "fn f (Int) -> Int;"
      , "fn f x = let y = 1, let y = 2 in y;"
      ]

   , named "a let binding shadowing a parameter" $ assertRejected isShadowedBinding $ inModule
      [ "fn f (Int) -> Int;"
      , "fn f x = let x = 1 in x;"
      ]
   ]

typeCheckingTests :: Test
typeCheckingTests = TestList
   [ named "body of the wrong type" $ assertRejected isTypeError $ inModule
      [ "fn f () -> Bool;"
      , "fn f = 1;"
      ]

   , named "constructor result of the wrong type" $ assertRejected isTypeError $ inModule
      [ "fn f () -> Int;"
      , "fn f = True;"
      ]

   , named "argument of the wrong type" $ assertRejected isTypeError $ inModule
      [ "fn f (Int) -> Int;"
      , "fn f x = x;"
      , "fn g () -> Int;"
      , "fn g = f True;"
      ]

   , named "if condition that is not a Bool" $ assertRejected isTypeError $ inModule
      [ "fn f () -> Int;"
      , "fn f = if 1 then 1 else 2;"
      ]

   , named "if branches of different types" $ assertRejected isThenElseTypeMismatch $ inModule
      [ "fn f (Bool) -> Int;"
      , "fn f b = if b then 1 else True;"
      ]

   , named "list elements of different types" $ assertRejected isListElementTypeMismatch $ inModule
      [ "fn f () -> Int;"
      , "fn f = match [1, True] with _ -> 1;"
      ]

   , named "applying something that is not a function" $ assertRejected isNotAFunction $ inModule
      [ "fn f () -> Int;"
      , "fn f = 1 2;"
      ]

   , named "too many arguments" $ assertRejected isNotAFunction $ inModule
      [ "fn f (Int) -> Int;"
      , "fn f x = x;"
      , "fn g () -> Int;"
      , "fn g = f 1 2;"
      ]

   , named "integer literal out of range" $ assertRejected isIntegerOutOfBounds $ inModule
      [ "fn f () -> Int;"
      , "fn f = 99999999999;"
      ]

   , named "a term whose type would be infinite" $ assertRejected isInfiniteType $ inModule
      [ "fn f () -> Int;"
      , "fn f = let g = .\\x -> x x in 1;"
      ]

   , named "more parameters than the signature has" $ assertRejected isFnArityMismatch $ inModule
      [ "fn f (Int) -> Int;"
      , "fn f x y = x;"
      ]

   , named "fewer parameters than the signature has" $ assertRejected isFnArityMismatch $ inModule
      [ "fn f (Int, Int) -> Int;"
      , "fn f x = x;"
      ]

   , named "constructor pattern with the wrong number of arguments" $ assertRejected isCtorArityMismatch $ inModule
      [ "type P = P Int Int;"
      , "fn f (P) -> Int;"
      , "fn f (P a) = a;"
      ]

   , named "field that does not exist" $ assertRejected isUnknownField $ inModule
      [ "type P = P { x : Int };"
      , "fn f (P) -> Int;"
      , "fn f p = p.y;"
      ]

   , named "field that does not exist in a record construction" $ assertRejected isUnknownField $ inModule
      [ "type P = P { x : Int };"
      , "fn f () -> P;"
      , "fn f = P { z = 1 };"
      ]

   , named "field access on a type that is not a record" $ assertRejected isNotARecordType $ inModule
      [ "fn f (Int) -> Int;"
      , "fn f p = p.x;"
      ]

   , named "tuple index out of range" $ assertRejected isInvalidTupleIndex $ inModule
      [ "fn f () -> Int;"
      , "fn f = (1, 2).5;"
      ]
   ]

exhaustivenessTests :: Test
exhaustivenessTests = TestList
   [ named "match covering every constructor" $ assertAnalyzes $ inModule
      [ "fn f (Bool) -> Int;"
      , "fn f b = match b with True -> 1, False -> 0;"
      ]

   , named "match missing a constructor" $ assertRejected isNonExhaustiveMatch $ inModule
      [ "fn f (Bool) -> Int;"
      , "fn f b = match b with True -> 1;"
      ]
   ]

systemTests :: Test
systemTests = TestList
   [ named "system with a definition and an implementation" $ assertAnalyzes $ inModule
      [ "type Pos = Pos Int;"
      , "system Move #(Pos) -> ();"
      , "system Move p = ();"
      ]

   , named "system definition without an implementation" $ assertRejected isMissingSystemImpls $ inModule
      [ "type Pos = Pos Int;"
      , "system Move #(Pos) -> ();"
      ]

   , named "system implementation without a definition" $ assertRejected isMissingSystemDef $ inModule
      [ "type Pos = Pos Int;"
      , "system Move p = ();"
      ]

   , named "system defined twice" $ assertRejected isSystemRedeclaration $ inModule
      [ "type Pos = Pos Int;"
      , "system Move #(Pos) -> ();"
      , "system Move #(Pos) -> ();"
      , "system Move p = ();"
      ]

   , named "system implementation with the wrong number of entities" $ assertRejected isSystemArityMismatch $ inModule
      [ "type Pos = Pos Int;"
      , "type Vel = Vel Int;"
      , "system Move #(Pos, Vel) -> ();"
      , "system Move p = ();"
      ]
   ]

-- Helpers -------------------------------------------------------------------

-- | Parses the sources and runs the whole semantic analysis on them.
--   A source that does not parse fails the test, since it is a broken test.
analyze :: [Text] -> IO (Either [SemError] AnalysisResult)
analyze sources = do
   let parsed = parseProgram "<test>" <$> sources
   unless (null (lefts parsed)) $
      assertFailure $
         "Parsing error occurred during semantic tests:\n"
            <> intercalate "\n" (map show (lefts parsed))
   runExceptT $
      fullAnalysis
         (const (pure ()))
         (const (const (pure ())))
         (rights parsed)
         Target64

-- | The analysis accepts the program.
assertAnalyzes :: [Text] -> Assertion
assertAnalyzes sources = analyze sources >>= \case
   Left errors -> assertFailure ("unexpected errors: " <> show errors)
   Right _     -> pure ()

-- | The analysis rejects the program with at least one error satisfying the predicate.
--   Errors carry source locations, so tests match on the kind of error.
assertRejected :: (SemError -> Bool) -> [Text] -> Assertion
assertRejected isExpected sources = analyze sources >>= \case
   Right _     -> assertFailure "expected an error, but the analysis succeeded"
   Left errors -> unless (any isExpected errors) $
      assertFailure ("none of the errors is the expected one: " <> show errors)

-- | A test case with a name that shows up when it fails.
named :: String -> Assertion -> Test
named label assertion = TestLabel label (TestCase assertion)

-- | The lines every test module starts with: there are no builtin types or
--   operators, so `Int`, `Bool` and the arithmetic functions are declared here.
preludeLines :: [Text]
preludeLines =
   [ "module T exports *"
   , "extern type Int;"
   , "type Bool = True | False;"
   , "extern fn opAdd(Int, Int) -> Int;"
   , "extern fn opEq(Int, Int) -> Bool;"
   ]

-- | A single module `T`: the prelude followed by the given declarations.
inModule :: [Text] -> [Text]
inModule declarations = [T.unlines (preludeLines ++ declarations)]

-- | A module with the given header and body lines, for tests with several modules.
moduleSource :: [Text] -> Text
moduleSource = T.unlines

-- | A module that declares `Int` and a function `inc` on it.
moduleWithInc :: Text -> Text
moduleWithInc moduleName = moduleSource
   [ "module " <> moduleName <> " exports *"
   , "extern type Int;"
   , "fn inc (Int) -> Int;"
   , "fn inc x = x;"
   ]

-- Error kinds ---------------------------------------------------------------

isSelfImport, isCyclicImport, isMissingModule, isMissingImports, isPrivateImports :: SemError -> Bool
isSelfImport = \case { SESelfImportError {} -> True; _ -> False }
isCyclicImport = \case { SECyclicImportError {} -> True; _ -> False }
isMissingModule = \case { SEMissingModule {} -> True; _ -> False }
isMissingImports = \case { SEMissingImports {} -> True; _ -> False }
isPrivateImports = \case { SEPrivateImports {} -> True; _ -> False }

isInvalidExport, isAmbiguousAlias, isAmbiguousImport, isImportDeclConflict :: SemError -> Bool
isInvalidExport = \case { SEInvalidExport {} -> True; _ -> False }
isAmbiguousAlias = \case { SEAmbiguousAlias {} -> True; _ -> False }
isAmbiguousImport = \case { SEAmbiguousImport {} -> True; _ -> False }
isImportDeclConflict = \case { SEImportDeclConflict {} -> True; _ -> False }

isTypeRedeclaration, isFnRedeclaration, isExternFnRedeclaration, isExternTypeRedeclaration :: SemError -> Bool
isTypeRedeclaration = \case { SETypeRedeclaration {} -> True; _ -> False }
isFnRedeclaration = \case { SEFnRedeclaration {} -> True; _ -> False }
isExternFnRedeclaration = \case { SEExternFnRedeclaration {} -> True; _ -> False }
isExternTypeRedeclaration = \case { SEExternTypeRedeclaration {} -> True; _ -> False }

isSystemRedeclaration, isCtorRedeclaration, isDuplicateBinding, isShadowedBinding :: SemError -> Bool
isSystemRedeclaration = \case { SESystemRedeclaration {} -> True; _ -> False }
isCtorRedeclaration = \case { SECtorRedeclaration {} -> True; _ -> False }
isDuplicateBinding = \case { SEDuplicateBinding {} -> True; _ -> False }
isShadowedBinding = \case { SEShadowedBinding {} -> True; _ -> False }

isUndefinedVar, isUndefinedCon, isUndefinedType, isUndefinedAlias :: SemError -> Bool
isUndefinedVar = \case { SEUndefinedVar {} -> True; _ -> False }
isUndefinedCon = \case { SEUndefinedCon {} -> True; _ -> False }
isUndefinedType = \case { SEUndefinedType {} -> True; _ -> False }
isUndefinedAlias = \case { SEUndefinedAlias {} -> True; _ -> False }

isMissingFnDef, isMissingFnImpls, isMissingSystemDef, isMissingSystemImpls :: SemError -> Bool
isMissingFnDef = \case { SEMissingFnDef {} -> True; _ -> False }
isMissingFnImpls = \case { SEMissingFnImpls {} -> True; _ -> False }
isMissingSystemDef = \case { SEMissingSystemDef {} -> True; _ -> False }
isMissingSystemImpls = \case { SEMissingSystemImpls {} -> True; _ -> False }

isIntegerOutOfBounds, isTypeError, isListElementTypeMismatch, isThenElseTypeMismatch :: SemError -> Bool
isIntegerOutOfBounds = \case { SEIntegerOutOfBounds {} -> True; _ -> False }
isTypeError = \case { SETypeError {} -> True; _ -> False }
isListElementTypeMismatch = \case { SEListElementTypeMismatch {} -> True; _ -> False }
isThenElseTypeMismatch = \case { SEThenElseTypeMismatch {} -> True; _ -> False }

isInfiniteType, isNotAFunction, isCtorArityMismatch, isFnArityMismatch, isSystemArityMismatch :: SemError -> Bool
isInfiniteType = \case { SEInfiniteType {} -> True; _ -> False }
isNotAFunction = \case { SENotAFunction {} -> True; _ -> False }
isCtorArityMismatch = \case { SECtorArityMismatch {} -> True; _ -> False }
isFnArityMismatch = \case { SEFnArityMismatch {} -> True; _ -> False }
isSystemArityMismatch = \case { SESystemArityMismatch {} -> True; _ -> False }

isUnknownField, isNotARecordType, isInvalidTupleIndex, isNonExhaustiveMatch :: SemError -> Bool
isUnknownField = \case { SEUnknownField {} -> True; _ -> False }
isNotARecordType = \case { SENotARecordType {} -> True; _ -> False }
isInvalidTupleIndex = \case { SEInvalidTupleIndex {} -> True; _ -> False }
isNonExhaustiveMatch = \case { SENonExhaustiveMatch {} -> True; _ -> False }
