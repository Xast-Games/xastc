{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Parser where

import Test.HUnit
import Xast.Parser.Common (Parser)
import Text.Megaparsec (runParser, errorBundlePretty, MonadParsec (eof), initialPos)
import Data.Text (Text)
import Control.Monad (unless)

import Xast.AST
import Xast.Parser.Ident
import Xast.Parser.Expr (pattern', expr)
import Xast.Parser.Type (type', typeDef)
import Xast.Parser.Function (func)
import Xast.Parser.Extern (extern)
import Xast.Parser.Headers (importDef, moduleDef)
import Xast.Parser.System (system)
import Xast.Parser.Program (program)
import qualified Data.Text as T

tests :: Test
tests = TestList
   [ TestLabel "Identifiers" identTests
   , TestLabel "Types"       typeTests
   , TestLabel "Patterns"    patternTests
   , TestLabel "Expressions" exprTests
   , TestLabel "Functions"   functionTests
   , TestLabel "Types"       typeDefTests
   , TestLabel "Extern"      externTests
   , TestLabel "Headers"     headerTests
   , TestLabel "Systems"     systemTests
   , TestLabel "Programs"    programTests
   ]

identTests :: Test
identTests = TestList
   [ TestCase $
      assertParses varIdent "hello"
      (Ident "hello")

   , TestCase $
      assertParses typeIdent "Hello"
      (Ident "Hello")

   , TestCase $
      assertParses genericIdent "a"
      (Ident "a")

   , TestCase $
      assertFails varIdent "Hello"

   , TestCase $
      assertFails typeIdent "hello"

   , TestCase $
      assertFails varIdent "let"

   , TestCase $
      assertFails fnIdent "match"
   ]

typeTests :: Test
typeTests = TestList
   [ TestCase $
      assertParses type' "Int"
      (TyCon (Ident "Int"))

   , TestCase $
      assertParses type' "a"
      (TyGnr (Ident "a"))

   , TestCase $
      assertParses type' "List Int"
      ( TyApp
         (TyCon (Ident "List"))
         (TyCon (Ident "Int"))
      )

   , TestCase $
      assertParses type' "()"
      (TyTuple [])

   , TestCase $
      assertParses type' "(Int)"
      (TyCon (Ident "Int"))

   , TestCase $
      assertParses type' "(Int, Bool)"
      ( TyTuple
         [ TyCon (Ident "Int")
         , TyCon (Ident "Bool")
         ]
      )

   , TestCase $
      assertParses type' "fn(Int, Bool) -> String"
      ( TyFn
         [ TyCon (Ident "Int")
         , TyCon (Ident "Bool")
         ]
         (TyCon (Ident "String"))
      )

   , TestCase $
         assertFails type' "fn("
   ]

patternTests :: Test
patternTests = TestList
   [ TestCase $
      assertParses pattern' "_" $
         (PatWildcard info)

   , TestCase $
      assertParses pattern' "x" $
         (PatVar info (Ident "x"))

   , TestCase $
      assertParses pattern' "Just x" $
         (PatCon info (Ident "Just")
            [PatVar info (Ident "x")]
         )

   , TestCase $
      assertParses pattern' "()" $
         (PatTuple info [])

   , TestCase $
      assertParses pattern' "(x)" $
         (PatVar info (Ident "x"))

   , TestCase $
      assertParses pattern' "(x, y)" $
         (PatTuple info
            [ PatVar info (Ident "x")
            , PatVar info (Ident "y")
            ]
         )

   , TestCase $
      assertParses pattern' "[x, y]" $
         (PatList info
            [ PatVar info (Ident "x")
            , PatVar info (Ident "y")
            ]
         )

   , TestCase $
      assertFails pattern' "(x,"
   ]

exprTests :: Test
exprTests = TestLabel "Expr (atoms)" $ TestList
   -- Variables / constructors
   [ TestCase $
      assertParses expr "x" $
         ExpVar info Nothing (Ident "x")

   , TestCase $
      assertParses expr "Point" $
         ExpCon info Nothing (Ident "Point")

   , TestCase $
      assertParses expr "Math.value" $
         ExpVar info (Just (Ident "Math")) (Ident "value")

   , TestCase $
      assertParses expr "Math.Point" $
         ExpCon info (Just (Ident "Math")) (Ident "Point")

   -- Integer / float
   , TestCase $
      assertParses expr "123" $
         ExpLit info (LitInt (int 123))

   , TestCase $
      assertParses expr "0" $
         ExpLit info (LitInt (int 0))

   , TestCase $
      assertParses expr "3.14" $
         ExpLit info (LitDouble 3.14)

   -- Char / string
   , TestCase $
      assertParses expr "'a'" $
         ExpLit info (LitChar 'a')

   , TestCase $
      assertParses expr "\"hello\"" $
         ExpLit info (LitString "hello")

   -- Lists
   , TestCase $
      assertParses expr "[]" $
         ExpList info []

   , TestCase $
      assertParses expr "[1]" $
         ExpList info
               [ ExpLit info (LitInt (int 1))
               ]

   , TestCase $
      assertParses expr "[1, 2, 3]" $
         ExpList info
               [ ExpLit info (LitInt (int 1))
               , ExpLit info (LitInt (int 2))
               , ExpLit info (LitInt (int 3))
               ]

   -- Empty / singleton / tuple
   , TestCase $
      assertParses expr "()" $
         ExpTuple info []

   , TestCase $
      assertParses expr "(1)" $
         ExpLit info (LitInt (int 1))

   , TestCase $
      assertParses expr "(1, 2)" $
         ExpTuple info
               [ ExpLit info (LitInt (int 1))
               , ExpLit info (LitInt (int 2))
               ]

   , TestCase $
      assertParses expr "(1, 2, 3)" $
         ExpTuple info
               [ ExpLit info (LitInt (int 1))
               , ExpLit info (LitInt (int 2))
               , ExpLit info (LitInt (int 3))
               ]

   -- Function application
   , TestCase $
      assertParses expr "f x" $
         ExpApp info
               (ExpVar info Nothing (Ident "f"))
               (ExpVar info Nothing (Ident "x"))

   , TestCase $
      assertParses expr "f x y" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "f"))
                     (ExpVar info Nothing (Ident "x")))
               (ExpVar info Nothing (Ident "y"))

   , TestCase $
      assertParses expr "f (g x)" $
         ExpApp info
               (ExpVar info Nothing (Ident "f"))
               (ExpApp info
                     (ExpVar info Nothing (Ident "g"))
                     (ExpVar info Nothing (Ident "x")))

   , TestCase $
      assertParses expr "Just 42" $
         ExpApp info
               (ExpCon info Nothing (Ident "Just"))
               (ExpLit info (LitInt (int 42)))

   , TestCase $
      assertParses expr "Maybe.Just 42" $
         ExpApp info
               (ExpCon info (Just $ Ident "Maybe") (Ident "Just"))
               (ExpLit info (LitInt (int 42)))

   -- Getters
   , TestCase $
      assertParses expr "point.x" $
         ExpVarGetter info
               (ExpVar info Nothing (Ident "point"))
               (GetField $ Ident "x")

   , TestCase $
      assertParses expr "tuple.0" $
         ExpVarGetter info
               (ExpVar info Nothing (Ident "tuple"))
               (GetTupleField 0)

   , TestCase $
      assertParses expr "tuple.15" $
         ExpVarGetter info
               (ExpVar info Nothing (Ident "tuple"))
               (GetTupleField 15)

   , TestCase $
      assertParses expr "point.pos.x" $
         ExpVarGetter info
               (ExpVarGetter info
                     (ExpVar info Nothing (Ident "point"))
                     (GetField $ Ident "pos"))
               (GetField $ Ident "x")

   , TestCase $
      assertParses expr "matrix.0.1" $
         ExpVarGetter info
               (ExpVarGetter info
                     (ExpVar info Nothing (Ident "matrix"))
                     (GetTupleField 0))
               (GetTupleField 1)

   -- Record construction
   , TestCase $
      assertParses expr "Point { x = 1 }" $
         ExpRecConstruct info $
               RecConstruct
                  Nothing
                  (Ident "Point")
                  [ RecAssign
                        (loc $ Ident "x")
                        (ExpLit info $ LitInt (int 1))
                  ]

   , TestCase $
      assertParses expr "Point { x = 1, y = 2 }" $
         ExpRecConstruct info $
               RecConstruct
                  Nothing
                  (Ident "Point")
                  [ RecAssign
                        (loc $ Ident "x")
                        (ExpLit info $ LitInt (int 1))
                  , RecAssign
                        (loc $ Ident "y")
                        (ExpLit info $ LitInt (int 2))
                  ]

   , TestCase $
      assertParses expr "Math.Point { x = 1 }" $
         ExpRecConstruct info $
               RecConstruct
                  (Just $ Ident "Math")
                  (Ident "Point")
                  [ RecAssign
                        (loc $ Ident "x")
                        (ExpLit info $ LitInt (int 1))
                  ]

   , TestCase $
      assertParses expr "Point { x = foo, y = bar }" $
         ExpRecConstruct info $
               RecConstruct
                  Nothing
                  (Ident "Point")
                  [ RecAssign
                        (loc $ Ident "x")
                        (ExpVar info Nothing $ Ident "foo")
                  , RecAssign
                        (loc $ Ident "y")
                        (ExpVar info Nothing $ Ident "bar")
                  ]

   -- Unary operators
   , TestCase $
      assertParses expr "-x" $
         ExpApp info
               (ExpVar info Nothing (Ident "opNeg"))
               (ExpVar info Nothing (Ident "x"))

   , TestCase $
      assertParses expr "!flag" $
         ExpApp info
               (ExpVar info Nothing (Ident "opNot"))
               (ExpVar info Nothing (Ident "flag"))

   -- Simple binary operators
   , TestCase $
      assertParses expr "a + b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opAdd"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a - b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opSub"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a * b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opMul"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a / b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opDiv"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a % b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opMod"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a == b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opEq"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a != b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opNeq"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a && b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opAnd"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a || b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opOr"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a |> f" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opPipe"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "f"))

   , TestCase $
      assertParses expr "a <> b" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opConcat"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpVar info Nothing (Ident "b"))

   -- Precedence
   , TestCase $
      assertParses expr "a + b * c" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opAdd"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpApp info
                     (ExpApp info
                           (ExpVar info Nothing (Ident "opMul"))
                           (ExpVar info Nothing (Ident "b")))
                     (ExpVar info Nothing (Ident "c")))

   , TestCase $
      assertParses expr "(a + b) * c" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opMul"))
                     (ExpApp info
                           (ExpApp info
                                 (ExpVar info Nothing (Ident "opAdd"))
                                 (ExpVar info Nothing (Ident "a")))
                           (ExpVar info Nothing (Ident "b"))))
               (ExpVar info Nothing (Ident "c"))

   , TestCase $
      assertParses expr "a == b && c" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opAnd"))
                     (ExpApp info
                           (ExpApp info
                                 (ExpVar info Nothing (Ident "opEq"))
                                 (ExpVar info Nothing (Ident "a")))
                           (ExpVar info Nothing (Ident "b"))))
               (ExpVar info Nothing (Ident "c"))

   , TestCase $
      assertParses expr "a && b || c" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opOr"))
                     (ExpApp info
                           (ExpApp info
                                 (ExpVar info Nothing (Ident "opAnd"))
                                 (ExpVar info Nothing (Ident "a")))
                           (ExpVar info Nothing (Ident "b"))))
               (ExpVar info Nothing (Ident "c"))

   -- Associativity
   , TestCase $
      assertParses expr "a - b - c" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opSub"))
                     (ExpApp info
                           (ExpApp info
                                 (ExpVar info Nothing (Ident "opSub"))
                                 (ExpVar info Nothing (Ident "a")))
                           (ExpVar info Nothing (Ident "b"))))
               (ExpVar info Nothing (Ident "c"))

   , TestCase $
      assertParses expr "a |> b |> c" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opPipe"))
                     (ExpApp info
                           (ExpApp info
                                 (ExpVar info Nothing (Ident "opPipe"))
                                 (ExpVar info Nothing (Ident "a")))
                           (ExpVar info Nothing (Ident "b"))))
               (ExpVar info Nothing (Ident "c"))

   , TestCase $
      assertParses expr "a ** b ** c" $
         ExpApp info
               (ExpApp info
                     (ExpVar info Nothing (Ident "opPow"))
                     (ExpVar info Nothing (Ident "a")))
               (ExpApp info
                     (ExpApp info
                           (ExpVar info Nothing (Ident "opPow"))
                           (ExpVar info Nothing (Ident "b")))
                     (ExpVar info Nothing (Ident "c")))

   -- Lambda
   , TestCase $
      assertParses expr ".\\x -> x" $
         ExpLambda info $
               Lambda
                  [PatVar info (Ident "x")]
                  (ExpVar info Nothing (Ident "x"))

   , TestCase $
      assertParses expr ".\\x y -> x" $
         ExpLambda info $
               Lambda
                  [PatVar info (Ident "x"), PatVar info (Ident "y")]
                  (ExpVar info Nothing (Ident "x"))

   , TestCase $
      assertParses expr ".\\a b c -> c" $
         ExpLambda info $
               Lambda
                  [PatVar info (Ident "a"), PatVar info (Ident "b"), PatVar info (Ident "c")]
                  (ExpVar info Nothing (Ident "c"))

   , TestCase $
      assertParses expr ".\\(x, y) -> x" $
         ExpLambda info $
               Lambda
                  [PatTuple info [PatVar info (Ident "x"), PatVar info (Ident "y")]]
                  (ExpVar info Nothing (Ident "x"))

   , TestCase $
      assertParses expr ".\\MyStruct a b -> a" $
         ExpLambda info $
               Lambda
                  [PatCon info (Ident "MyStruct") [PatVar info (Ident "a"), PatVar info (Ident "b")]]
                  (ExpVar info Nothing (Ident "a"))

   -- If
   , TestCase $
      assertParses expr "if cond then a else b" $
         ExpIfThen info $
               IfThenElse
                  (ExpVar info Nothing (Ident "cond"))
                  (ExpVar info Nothing (Ident "a"))
                  (ExpVar info Nothing (Ident "b"))

   , TestCase $
      assertParses expr "if x then 1 else 2" $
         ExpIfThen info $
               IfThenElse
                  (ExpVar info Nothing (Ident "x"))
                  (ExpLit info $ LitInt (int 1))
                  (ExpLit info $ LitInt (int 2))

   -- Let
   , TestCase $
      assertParses expr "let x = 1 in x" $
         ExpLetIn info $
               LetIn
                  [ Let
                           (PatVar info $ Ident "x")
                           (ExpLit info $ LitInt (int 1))
                  ]
                  (ExpVar info Nothing $ Ident "x")

   , TestCase $
      assertParses expr "let x = 1, let y = 2 in x" $
         ExpLetIn info $
               LetIn
                  [ Let
                           (PatVar info $ Ident "x")
                           (ExpLit info $ LitInt (int 1))
                  , Let
                           (PatVar info $ Ident "y")
                           (ExpLit info $ LitInt (int 2))
                  ]
                  (ExpVar info Nothing $ Ident "x")

   , TestCase $
      assertParses expr "let (a, b) = pair in a" $
         ExpLetIn info $
               LetIn
                  [ Let
                           (PatTuple info
                              [ PatVar info $ Ident "a"
                              , PatVar info $ Ident "b"
                              ])
                           (ExpVar info Nothing $ Ident "pair")
                  ]
                  (ExpVar info Nothing $ Ident "a")

   -- Match
   , TestCase $
      assertParses expr "match x with _ -> 0" $
         ExpMatch info $
               Match
                  (ExpVar info Nothing $ Ident "x")
                  [ MatchWing
                        (PatWildcard info)
                        (ExpLit info $ LitInt (int 0))
                  ]
                  False

   , TestCase $
      assertParses expr "match x with Just y -> y, Nothing -> 0" $
         ExpMatch info $
               Match
                  (ExpVar info Nothing $ Ident "x")
                  [ MatchWing
                        (PatCon info
                              (Ident "Just")
                              [PatVar info $ Ident "y"])
                        (ExpVar info Nothing $ Ident "y")

                  , MatchWing
                        (PatCon info
                              (Ident "Nothing")
                              [])
                        (ExpLit info $ LitInt (int 0))
                  ]
                  False

   , TestCase $
      assertParses expr "match xs with [] -> 0, _ -> 1" $
         ExpMatch info $
               Match
                  (ExpVar info Nothing $ Ident "xs")
                  [ MatchWing
                     (PatList info [])
                     (ExpLit info $ LitInt (int 0))

                  , MatchWing
                     (PatWildcard info)
                     (ExpLit info $ LitInt (int 1))
                  ]
                  False

   -- Failure cases
   , TestCase $
      assertFails expr ""

   , TestCase $
      assertFails expr "("

   , TestCase $
      assertFails expr ")"

   , TestCase $
      assertFails expr "["

   , TestCase $
      assertFails expr "]"

   , TestCase $
      assertFails expr "(1,"

   , TestCase $
      assertFails expr "[1,"

   , TestCase $
      assertFails expr "."

   , TestCase $
      assertFails expr ".\\"

   , TestCase $
      assertFails expr ".\\ -> x"

   , TestCase $
      assertFails expr ".\\x"

   , TestCase $
      assertFails expr ".\\x ->"

   , TestCase $
      assertFails expr "if"

   , TestCase $
      assertFails expr "if x"

   , TestCase $
      assertFails expr "if x then"

   , TestCase $
      assertFails expr "if x then y"

   , TestCase $
      assertFails expr "if x else y"

   , TestCase $
      assertFails expr "if then x else y"

   , TestCase $
      assertFails expr "let"

   , TestCase $
      assertFails expr "let x"

   , TestCase $
      assertFails expr "let x ="

   , TestCase $
      assertFails expr "let x = 1"

   , TestCase $
      assertFails expr "let in x"

   , TestCase $
      assertFails expr "let x = 1 in"

   , TestCase $
      assertFails expr "match"

   , TestCase $
      assertFails expr "match x"

   , TestCase $
      assertFails expr "match x with"

   , TestCase $
      assertFails expr "match x with _"

   , TestCase $
      assertFails expr "match x with _ ->"

   , TestCase $
      assertFails expr "Point {"

   , TestCase $
      assertFails expr "Point { x"

   , TestCase $
      assertFails expr "Point { x ="

   , TestCase $
      assertFails expr "point."

   , TestCase $
      assertFails expr "point..x"

   , TestCase $
      assertFails expr ".0"

   -- Binary operators
   , TestCase $
      assertFails expr "+"

   , TestCase $
      assertFails expr "*"

   , TestCase $
      assertFails expr "1 +"

   , TestCase $
      assertFails expr "+ 1"

   , TestCase $
      assertFails expr "1 *"

   , TestCase $
      assertFails expr "* 1"

   , TestCase $
      assertFails expr "1 + * 2"

   , TestCase $
      assertFails expr "1 &&"

   , TestCase $
      assertFails expr "1 ||"

   , TestCase $
      assertFails expr "1 =="

   , TestCase $
      assertFails expr "1 !="

   , TestCase $
      assertFails expr "1 |>"

   , TestCase $
      assertFails expr "1 <>"

   , TestCase $
      assertFails expr "** 2"

   , TestCase $
      assertFails expr "1 **"
   ]

functionTests :: Test
functionTests = TestLabel "Functions" $ TestList
   -- Function declarations
   [ TestCase $
      assertParsesIgnoringLocations withTestLocationFunc func
         "fn add(Int, Int) -> Int;"
         (FnDef $
            FuncDef
                  testLocation
                  []
                  (Ident "add")
                  [ (loc $ TyCon $ Ident "Int")
                  , (loc $ TyCon $ Ident "Int")
                  ]
                  (loc $ TyCon $ Ident "Int"))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationFunc func
         "fn id(a) -> a;"
         (FnDef $
            FuncDef
                  testLocation
                  []
                  (Ident "id")
                  [(loc $ TyGnr $ Ident "a")]
                  (loc $ TyGnr $ Ident "a"))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationFunc func
         "fn makePoint() -> Point;"
         (FnDef $
            FuncDef
                  testLocation
                  []
                  (Ident "makePoint")
                  []
                  (loc $ TyCon $ Ident "Point"))

   -- Function implementations
   , TestCase $
      assertParsesIgnoringLocations withTestLocationFunc func
         "fn id x = x;"
         (FnImpl $
            FuncImpl
                  testLocation
                  (Ident "id")
                  [PatVar info $ Ident "x"]
                  (ExpVar info Nothing $ Ident "x"))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationFunc func
         "fn const a b = a;"
         (FnImpl $
            FuncImpl
                  testLocation
                  (Ident "const")
                  [ PatVar info $ Ident "a"
                  , PatVar info $ Ident "b"
                  ]
                  (ExpVar info Nothing $ Ident "a"))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationFunc func
         "fn fst (a, b) = a;"
         (FnImpl $
            FuncImpl
                  testLocation
                  (Ident "fst")
                  [PatTuple info
                     [ PatVar info $ Ident "a"
                     , PatVar info $ Ident "b"
                     ]]
                  (ExpVar info Nothing $ Ident "a"))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationFunc func
         "fn isEmpty [] = True;"
         (FnImpl $
            FuncImpl
                  testLocation
                  (Ident "isEmpty")
                  [PatList info []]
                  (ExpCon info Nothing $ Ident "True"))

   -- Failures
   , TestCase $
      assertFails func "fn"

   , TestCase $
      assertFails func "fn foo"

   , TestCase $
      assertFails func "fn foo()"

   , TestCase $
      assertFails func "fn foo ="

   , TestCase $
      assertFails func "fn foo x"

   , TestCase $
      assertFails func "fn foo(Int)"

   , TestCase $
      assertFails func "fn foo(Int) ->"

   , TestCase $
      assertFails func "fn foo(Int) Int;"
   ]

typeDefTests :: Test
typeDefTests = TestLabel "Type definitions" $ TestList
   -- Unit constructor
   [ TestCase $
      assertParsesIgnoringLocations withTestLocationTypeDef typeDef
         "type Bool = True | False;"
         (TypeDef
               testLocation
               []
               (Ident "Bool")
               []
               [ Ctor testLocation (Ident "True") PUnit
               , Ctor testLocation (Ident "False") PUnit
               ])

   -- Tuple constructor
   , TestCase $
      assertParsesIgnoringLocations withTestLocationTypeDef typeDef
         "type Maybe a = Just a | Nothing;"
         (TypeDef
               testLocation
               []
               (Ident "Maybe")
               [Ident "a"]
               [ Ctor
                        testLocation
                        (Ident "Just")
                        (PTuple
                           [(loc $ TyGnr $ Ident "a")])
               , Ctor
                        testLocation
                        (Ident "Nothing")
                        PUnit
               ])

   , TestCase $
      assertParsesIgnoringLocations withTestLocationTypeDef typeDef
         "type Either a b = Left a | Right b;"
         (TypeDef
               testLocation
               []
               (Ident "Either")
               [Ident "a", Ident "b"]
               [ Ctor
                        testLocation
                        (Ident "Left")
                        (PTuple [(loc $ TyGnr $ Ident "a")])
               , Ctor
                        testLocation
                        (Ident "Right")
                        (PTuple [(loc $ TyGnr $ Ident "b")])
               ])

   -- Record constructor
   , TestCase $
      assertParsesIgnoringLocations withTestLocationTypeDef typeDef
         "type Point = Point { x : Int, y : Int };"
         (TypeDef
               testLocation
               []
               (Ident "Point")
               []
               [ Ctor
                        testLocation
                        (Ident "Point")
                        (PRecord
                           [ Field
                              (Ident "x")
                              (loc $ TyCon $ Ident "Int")
                           , Field
                              (Ident "y")
                              (loc $ TyCon $ Ident "Int")
                           ])
               ])

   -- Multiple constructors
   , TestCase $
      assertParsesIgnoringLocations withTestLocationTypeDef typeDef
         "type Shape = Circle Float | Rectangle Float Float;"
         (TypeDef
               testLocation
               []
               (Ident "Shape")
               []
               [ Ctor
                        testLocation
                        (Ident "Circle")
                        (PTuple
                           [(loc $ TyCon $ Ident "Float")])
               , Ctor
                        testLocation
                        (Ident "Rectangle")
                        (PTuple
                           [ (loc $ TyCon $ Ident "Float")
                           , (loc $ TyCon $ Ident "Float")
                           ])
               ])

   -- Failures
   , TestCase $
      assertFails typeDef "type"

   , TestCase $
      assertFails typeDef "type Foo"

   , TestCase $
      assertFails typeDef "type Foo ="

   , TestCase $
      assertFails typeDef "type Foo = |"

   , TestCase $
      assertFails typeDef "type Foo = Bar("

   , TestCase $
      assertFails typeDef "type Foo = Bar {"

   , TestCase $
      assertFails typeDef "type Foo = Bar { x : }"

   , TestCase $
      assertFails typeDef "type Foo = Bar { x Int }"
   ]

externTests :: Test
externTests = TestLabel "Extern" $ TestList
   -- Extern functions
   [ TestCase $
      assertParsesIgnoringLocations withTestLocationExtern extern
         "extern fn puts(String) -> Int;"
         (ExtFunc $
            ExternFunc
                  testLocation
                  []
                  (Ident "puts")
                  [(loc $ TyCon $ Ident "String")]
                  (loc $ TyCon $ Ident "Int"))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationExtern extern
         "extern fn malloc(Int) -> Ptr;"
         (ExtFunc $
            ExternFunc
                  testLocation
                  []
                  (Ident "malloc")
                  [(loc $ TyCon $ Ident "Int")]
                  (loc $ TyCon $ Ident "Ptr"))

   , TestCase $
        assertParsesIgnoringLocations withTestLocationExtern extern
           "extern fn panic() -> Never;"
           (ExtFunc $
              ExternFunc
                    testLocation
                    []
                    (Ident "panic")
                    []
                  (loc $ TyCon $ Ident "Never"))

   -- Extern types
   , TestCase $
      assertParsesIgnoringLocations withTestLocationExtern extern
         "extern type CString;"
         (ExtType $
            ExternType
                  testLocation
                  (Ident "CString")
                  [])

   , TestCase $
      assertParsesIgnoringLocations withTestLocationExtern extern
         "extern type Ptr a;"
         (ExtType $
            ExternType
                  testLocation
                  (Ident "Ptr")
                  [Ident "a"])

   , TestCase $
      assertParsesIgnoringLocations withTestLocationExtern extern
         "extern type Either a b;"
         (ExtType $
            ExternType
                  testLocation
                  (Ident "Either")
                  [Ident "a", Ident "b"])

   -- Failures
   , TestCase $ assertFails extern "extern"
   , TestCase $ assertFails extern "extern fn"
   , TestCase $ assertFails extern "extern fn foo"
   , TestCase $ assertFails extern "extern fn foo("
   , TestCase $ assertFails extern "extern fn foo()"
   , TestCase $ assertFails extern "extern fn foo() ->"
   , TestCase $ assertFails extern "extern type"
   , TestCase $ assertFails extern "extern type a"
   ]

headerTests :: Test
headerTests = TestLabel "Headers" $ TestList
   -- Modules
   [ TestCase $
      assertParsesIgnoringLocations withTestLocationModuleDef moduleDef
         "module Main exports *"
         (ModuleDef
               testLocation
               (Module [Ident "Main"])
               (loc ExpFull))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationModuleDef moduleDef
         "module Foo.Bar exports *"
         (ModuleDef
               testLocation
               (Module
                  [ Ident "Foo"
                  , Ident "Bar"
                  ])
               (loc ExpFull))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationModuleDef moduleDef
         "module Game.Player exports { Player, spawn }"
         (ModuleDef
               testLocation
               (Module
                  [ Ident "Game"
                  , Ident "Player"
                  ])
               (loc $ ExpSelect
                     [ Ident "Player"
                     , Ident "spawn"
                     ]))

   -- Imports
   , TestCase $
      assertParses importDef
         "use Math *"
         (loc $
            ImportDef
               (Module [Ident "Math"])
               ImpFull)

   , TestCase $
      assertParses importDef
         "use Math as M"
         (loc $
            ImportDef
               (Module [Ident "Math"])
               (ImpAlias $
                  loc $
                     Ident "M"))

   , TestCase $
      assertParses importDef
         "use Math { sin, cos }"
         (loc $
            ImportDef
               (Module [Ident "Math"])
               (ImpSelect
                  [ loc $ Ident "sin"
                  , loc $ Ident "cos"
                  ]))

   , TestCase $
      assertParses importDef
         "use Foo.Bar { Baz, qux }"
         (loc $
            ImportDef
               (Module
                  [ Ident "Foo"
                  , Ident "Bar"
                  ])
               (ImpSelect
                  [ loc $ Ident "Baz"
                  , loc $ Ident "qux"
                  ]))

   -- Failures
   , TestCase $ assertFails moduleDef "module"
   , TestCase $ assertFails moduleDef "module Foo"
   , TestCase $ assertFails moduleDef "module Foo exports"
   , TestCase $ assertFails moduleDef "module Foo exports {}"
   , TestCase $ assertFails importDef "use"
   , TestCase $ assertFails importDef "use Foo"
   , TestCase $ assertFails importDef "use Foo as"
   , TestCase $ assertFails importDef "use Foo {}"
   , TestCase $ assertFails importDef "use Foo {,}"
   ]

systemTests :: Test
systemTests = TestLabel "Systems" $ TestList
   -- Definitions
   [ TestCase $
      assertParsesIgnoringLocations withTestLocationSystem system
         "system Move -> ();"
         (SysDef $
            SystemDef
                  testLocation
                  []
                  (Ident "Move")
                  []
                  (loc $ TyTuple [])
                  Nothing)

   , TestCase $
      assertParsesIgnoringLocations withTestLocationSystem system
         "system Move #(Position) -> ();"
         (SysDef $
            SystemDef
                  testLocation
                  []
                  (Ident "Move")
                  [QueriedEntity
                     [(loc $ TyCon $ Ident "Position")]]
                  (loc $ TyTuple [])
                  Nothing)

   , TestCase $
      assertParsesIgnoringLocations withTestLocationSystem system
         "system Move #(Position, Velocity) -> ();"
         (SysDef $
            SystemDef
                  testLocation
                  []
                  (Ident "Move")
                  [QueriedEntity
                     [ (loc $ TyCon $ Ident "Position")
                     , (loc $ TyCon $ Ident "Velocity")
                     ]]
                  (loc $ TyTuple [])
                  Nothing)

   , TestCase $
      assertParsesIgnoringLocations withTestLocationSystem system
         "system Move -> () with event: Damage;"
         (SysDef $
            SystemDef
                  testLocation
                  []
                  (Ident "Move")
                  []
                  (loc $ TyTuple [])
                  (Just
                     [WithEvent $ (loc $ TyCon $ Ident "Damage")]))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationSystem system
         "system Move -> () with res: Time;"
         (SysDef $
            SystemDef
                  testLocation
                  []
                  (Ident "Move")
                  []
                  (loc $ TyTuple [])
                  (Just
                     [WithRes $ (loc $ TyCon $ Ident "Time")]))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationSystem system
         "@Label(Physics) system Move -> ();"
         (SysDef $
            SystemDef
                  testLocation
                  [SysMod $ ModLabel $ Ident "Physics"]
                  (Ident "Move")
                  []
                  (loc $ TyTuple [])
                  Nothing)

   -- Implementations
   , TestCase $
      assertParsesIgnoringLocations withTestLocationSystem system
         "system Move = pos;"
         (SysImpl $
            SystemImpl
                  testLocation
                  (Ident "Move")
                  []
                  Nothing
                  (ExpVar info Nothing $ Ident "pos"))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationSystem system
         "system Move pos vel = pos;"
         (SysImpl $
            SystemImpl
                  testLocation
                  (Ident "Move")
                  [EntityPattern
                     [ EntPatBinding (PatVar info $ Ident "pos") AccessRead
                     , EntPatBinding (PatVar info $ Ident "vel") AccessRead
                     ]]
                  Nothing
                  (ExpVar info Nothing $ Ident "pos"))

   , TestCase $
      assertParsesIgnoringLocations withTestLocationSystem system
         "system Move with dt = dt;"
         (SysImpl $
            SystemImpl
                  testLocation
                  (Ident "Move")
                  []
                  (Just
                     [PatVar info $ Ident "dt"])
                  (ExpVar info Nothing $ Ident "dt"))

   -- Failures
   , TestCase $ assertFails system "system"
   , TestCase $ assertFails system "system Move"
   , TestCase $ assertFails system "system Move ->"
   , TestCase $ assertFails system "system Move ="
   , TestCase $ assertFails system "system Move #("
   , TestCase $ assertFails system "@label ="
   ]

programTests :: Test
programTests = TestLabel "Programs" $ TestList
   [ TestCase $
      let src = T.unlines
            [ "module Main exports *"
            , "fn main () -> Int;"
            ]
      in assertParsesProgram src
         (Program
            (ModuleDef
                  testLocation
                  (Module [Ident "Main"])
                  (loc ExpFull))
            []
            [ StmtFunc $
                  FnDef $
                     FuncDef
                           testLocation
                           []
                           (Ident "main")
                           []
                           (loc $ TyCon $ Ident "Int")
            ]
            src)

   , TestCase $
      let src = T.unlines
            [ "module Main exports *"
            , "extern type CString;"
            ]
      in assertParsesProgram src
         (Program
            (ModuleDef
                  testLocation
                  (Module [Ident "Main"])
                  (loc ExpFull))
            []
            [ StmtExtern $
                  ExtType $
                     ExternType
                           testLocation
                           (Ident "CString")
                           []
            ]
            src)

   , TestCase $
      let src = T.unlines
            [ "module Game.Main exports *"
            , "use Math *"
            , "type Bool = True | False;"
            ]
      in assertParsesProgram src
         (Program
            (ModuleDef
                  testLocation
                  (Module
                     [Ident "Game", Ident "Main"])
                  (loc ExpFull))
            [loc $
               ImportDef
                  (Module [Ident "Math"])
                  ImpFull]
            [ StmtTypeDef $
                  TypeDef
                        testLocation
                        []
                        (Ident "Bool")
                        []
                        [ Ctor testLocation (Ident "True") PUnit
                        , Ctor testLocation (Ident "False") PUnit
                        ]
            ]
            src)

   -- Failures
   , TestCase $
      assertFails program ""

   , TestCase $
      assertFails program
         "fn main () -> Int;"

   , TestCase $
      assertFails program
         "module Main exports *"

   , TestCase $
      assertFails program $
         T.unlines
            [ "@mode = \"invalid\""
            , "module Main exports *"
            , "fn main () -> Int;"
            ]

   , TestCase $
      assertFails program $
         T.unlines
            [ "module Main exports *"
            , "use"
            ]

   , TestCase $
      assertFails program $
         T.unlines
            [ "module Main exports *"
            , "unknown"
            ]
   ]

testLocation :: Location
testLocation = Location (initialPos "<test>") 0 0

loc :: a -> Located a
loc = Located testLocation

-- | The info of a parsed node. Parsed values compare equal whatever their location.
info :: Parsed
info = ParsedInfo testLocation

-- | An unsuffixed integer literal.
int :: Integer -> IntLiteral
int = IntLiteral Int

assertParses 
   :: (Eq a, Show a)
   => Parser a
   -> Text
   -> a
   -> Assertion
assertParses p input expected =
   case runParser (p <* eof) "<test>" input of
      Left e ->
         assertFailure $
            "Expected success, got error:\n" <> errorBundlePretty e

      Right found ->
         unless (found == expected) $
            assertFailure $
               "Parsed value mismatch:\n" <>
               "   expected: " <> show expected <> "\n" <>
               "   found:    " <> show found

-- | Like `assertParses`, but first overwrites the `Location` fields that the
-- parser records (and that derived `Eq` compares) with `testLocation`.
assertParsesIgnoringLocations
   :: (Eq a, Show a)
   => (a -> a)
   -> Parser a
   -> Text
   -> a
   -> Assertion
assertParsesIgnoringLocations setLocations p = assertParses (setLocations <$> p)

-- The `withTestLocation...` functions below return their argument with every
-- `Location` the parser recorded replaced by `testLocation`, so that expected
-- values do not need to know exact source positions.
withTestLocationFunc :: Func Parsed -> Func Parsed
withTestLocationFunc = \case
   FnDef (FuncDef _ modifiers name args retType) ->
      FnDef (FuncDef testLocation modifiers name args retType)
   FnImpl (FuncImpl _ name args body) ->
      FnImpl (FuncImpl testLocation name args body)

withTestLocationTypeDef :: TypeDef -> TypeDef
withTestLocationTypeDef (TypeDef _ modifiers name generics ctors) =
   TypeDef testLocation modifiers name generics (map withTestLocationCtor ctors)

withTestLocationCtor :: Ctor -> Ctor
withTestLocationCtor (Ctor _ name payload) = Ctor testLocation name payload

withTestLocationExtern :: Extern -> Extern
withTestLocationExtern = \case
   ExtFunc (ExternFunc _ modifiers name args retType) ->
      ExtFunc (ExternFunc testLocation modifiers name args retType)
   ExtType (ExternType _ name generics) ->
      ExtType (ExternType testLocation name generics)

withTestLocationModuleDef :: ModuleDef -> ModuleDef
withTestLocationModuleDef (ModuleDef _ name export) = ModuleDef testLocation name export

withTestLocationSystem :: System Parsed -> System Parsed
withTestLocationSystem = \case
   SysDef (SystemDef _ modifiers name entities retType with) ->
      SysDef (SystemDef testLocation modifiers name entities retType with)
   SysImpl (SystemImpl _ name entities with body) ->
      SysImpl (SystemImpl testLocation name entities with body)

withTestLocationStmt :: Stmt Parsed -> Stmt Parsed
withTestLocationStmt = \case
   StmtTypeDef typeDef' -> StmtTypeDef (withTestLocationTypeDef typeDef')
   StmtFunc func'       -> StmtFunc (withTestLocationFunc func')
   StmtExtern extern'   -> StmtExtern (withTestLocationExtern extern')
   StmtSystem system'   -> StmtSystem (withTestLocationSystem system')

withTestLocationProgram :: Program Parsed -> Program Parsed
withTestLocationProgram (Program moduleDef' imports stmts src) =
   Program (withTestLocationModuleDef moduleDef') imports (map withTestLocationStmt stmts) src

assertFails :: Parser a -> Text -> Assertion
assertFails p input =
   case runParser (p <* eof) "<test>" input of
      Right _ -> assertFailure "Expected failure, but parsing succeeded"
      Left _  -> return ()

-- | `program` yields a `Text -> Program Parsed` waiting on the source text to
-- fill in `progSource`; feed it back the same source that's given to
-- `assertParses` so the produced `Program` can be compared directly.
assertParsesProgram :: Text -> Program Parsed -> Assertion
assertParsesProgram input =
   assertParses (fmap (withTestLocationProgram . ($ input)) program) input