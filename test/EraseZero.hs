{-# LANGUAGE OverloadedStrings #-}
module EraseZero (tests) where

import qualified Data.Set as S
import Data.Text (Text)
import Test.HUnit
import Xast.AST
import Xast.Lowerer.Types
import Xast.Lowerer.EraseZero

tests :: Test
tests = TestList
   [ TestLabel "zero-sized types"         zeroSizedTypesTest
   , TestLabel "type definitions"         typeDefinitionsTest
   , TestLabel "function header"          functionHeaderTest
   , TestLabel "calls"                    callsTest
   , TestLabel "stores and locals"        storesAndLocalsTest
   , TestLabel "match, erased result"     matchErasedResultTest
   , TestLabel "match, erased scrutinee"  matchErasedScrutineeTest
   , TestLabel "system bindings"          systemBindingsTest
   ]

-- Fixtures ------------------------------------------------------------------

unitType, intType, erasedType :: Type
unitType   = TyCon (Ident "Unit")
intType    = TyCon (Ident "Int")
erasedType = TyTuple []

unitCtor :: CtorName
unitCtor = Ident "Unit"

unitValue :: KirValue
unitValue = KirConst (TagCtor unitCtor)

fieldOf :: Text -> Type -> KirFieldDef
fieldOf fieldName fieldType = KirFieldDef { name = Ident fieldName, ty = fieldType }

-- type Unit = Unit
unitDef :: KirTypeDef
unitDef = KirTypeDef
   { name  = KirName "Unit"
   , ctors = [KirCtor { name = unitCtor, fields = [] }]
   }

-- type Wrapper = Wrapper { u : Unit, n : Int }
wrapperDef :: KirTypeDef
wrapperDef = KirTypeDef
   { name  = KirName "Wrapper"
   , ctors = [KirCtor { name = Ident "Wrapper", fields = [fieldOf "u" unitType, fieldOf "n" intType] }]
   }

-- type Only = Only { u : Unit }, zero-sized because its only field is
onlyDef :: KirTypeDef
onlyDef = KirTypeDef
   { name  = KirName "Only"
   , ctors = [KirCtor { name = Ident "Only", fields = [fieldOf "u" unitType] }]
   }

typeDefs :: [KirTypeDef]
typeDefs = [unitDef, wrapperDef, onlyDef]

paramOf :: Text -> Type -> KirParam
paramOf paramName paramType = KirParam { ty = paramType, name = KirName paramName }

functionOf :: Text -> [KirParam] -> Type -> [KirInstruct] -> KirTerm -> KirFunction
functionOf functionName params returnType instructions terminator = KirFunction
   { name   = KirName functionName
   , params = params
   , retTy  = returnType
   , body   = KirBlock { instructs = instructions, term = terminator }
   }

programOf :: [KirFunction] -> [KirSystem] -> Kira
programOf functions systems = Kira
   { moduleName = Module [Ident "Test"]
   , systems    = systems
   , functions  = functions
   }

callOf :: Type -> Text -> [KirValue] -> Maybe Text -> KirInstruct
callOf resultType callee args out = KirCall
   { ty     = resultType
   , callee = KirName callee
   , args   = args
   , out    = fmap KirName out
   }

var :: Text -> KirValue
var = KirVar . KirName

-- | Erases one program and returns it with the erased type definitions.
eraseOne :: Kira -> (Kira, [KirTypeDef])
eraseOne program =
   let (erasedPrograms, erasedDefs) = eraseZeroSized [program] typeDefs
   in (head erasedPrograms, erasedDefs)

erasedFunctionNamed :: Text -> Kira -> KirFunction
erasedFunctionNamed functionName program =
   head [function | function <- program.functions, function.name == KirName functionName]

-- | Kira has no Eq, so structures are compared by their Show output.
assertSame :: Show a => String -> a -> a -> Assertion
assertSame label expected actual = assertEqual label (show expected) (show actual)

-- Tests ---------------------------------------------------------------------

zeroSizedTypesTest :: Test
zeroSizedTypesTest = TestCase $ do
   let context = newErasureContext [] typeDefs
   assertEqual "Unit and Only are zero-sized, Wrapper is not"
      (S.fromList [KirName "Unit", KirName "Only"])
      context.zstTypes
   assertEqual "constructors of zero-sized types"
      (S.fromList [unitCtor, Ident "Only"])
      context.zstCtors

typeDefinitionsTest :: Test
typeDefinitionsTest = TestCase $ do
   let (_, erasedDefs) = eraseOne (programOf [] [])
   assertSame "zero-sized types are removed" [KirName "Wrapper"] (map (.name) erasedDefs)
   let wrapperFields = [field.name | def <- erasedDefs, ctor <- def.ctors, field <- ctor.fields]
   assertSame "zero-sized fields are dropped" [Ident "n"] wrapperFields

functionHeaderTest :: Test
functionHeaderTest = TestCase $ do
   let original = functionOf "f" [paramOf "u" unitType, paramOf "n" intType] unitType
         [KirStore { ty = unitType, dest = DestLocal (KirLocalId 0), value = unitValue }]
         (KirReturn (Just (KirLocalRef (KirLocalId 0))))
   let (erased, _) = eraseOne (programOf [original] [])
   let expected = functionOf "f" [paramOf "n" intType] erasedType [] (KirReturn Nothing)
   assertSame "parameter dropped, return erased, store dropped" expected (erasedFunctionNamed "f" erased)

callsTest :: Test
callsTest = TestCase $ do
   let calleeG = functionOf "g" [paramOf "x" unitType, paramOf "y" intType] intType [] (KirReturn Nothing)
   let calleeH = functionOf "h" [paramOf "n" intType] unitType [] (KirReturn Nothing)
   let caller = functionOf "caller" [paramOf "n" intType] intType
         [ callOf intType "g" [unitValue, var "n"] (Just "r1")
         , callOf unitType "h" [var "n"] (Just "u1")
         , callOf intType "externAdd" [var "r1", var "u1"] (Just "r2")
         ]
         (KirReturn (Just (var "r2")))
   let (erased, _) = eraseOne (programOf [calleeG, calleeH, caller] [])
   let expected = functionOf "caller" [paramOf "n" intType] intType
         [ callOf intType "g" [var "n"] (Just "r1")
         , callOf erasedType "h" [var "n"] Nothing
         , callOf intType "externAdd" [var "r1"] (Just "r2")
         ]
         (KirReturn (Just (var "r2")))
   assertSame "known callee drops erased params, erased result has no name, extern drops erased values"
      expected (erasedFunctionNamed "caller" erased)

storesAndLocalsTest :: Test
storesAndLocalsTest = TestCase $ do
   let original = functionOf "p" [paramOf "n" intType] intType
         [ KirStore { ty = unitType, dest = DestLocal (KirLocalId 0), value = unitValue }
         , callOf intType "ext" [KirLocalRef (KirLocalId 0), var "n"] (Just "r")
         ]
         (KirReturn (Just (var "r")))
   let (erased, _) = eraseOne (programOf [original] [])
   let expected = functionOf "p" [paramOf "n" intType] intType
         [ callOf intType "ext" [var "n"] (Just "r") ]
         (KirReturn (Just (var "r")))
   assertSame "a local that held a zero-sized value disappears from later uses"
      expected (erasedFunctionNamed "p" erased)

matchErasedResultTest :: Test
matchErasedResultTest = TestCase $ do
   let branchOf = KirBranch
         { instructs = [callOf intType "side" [var "n"] (Just "s")]
         , value     = var "s"
         }
   let matchOn resultType branch = KirMatch
         { ty         = resultType
         , scrutinee  = var "n"
         , arms       = [KirMatchArm { tag = TagCtor (Ident "A"), branch = branch }]
         , fallback   = Nothing
         , dest       = DestLocal (KirLocalId 3)
         , exhaustive = True
         }
   let original = functionOf "m" [paramOf "n" intType] unitType
         [matchOn unitType branchOf]
         (KirReturn (Just (KirLocalRef (KirLocalId 3))))
   let (erased, _) = eraseOne (programOf [original] [])
   let expected = functionOf "m" [paramOf "n" intType] erasedType
         [matchOn erasedType (KirBranch { instructs = branchOf.instructs, value = KirUnit })]
         (KirReturn Nothing)
   assertSame "the branches stay, the match stops producing a value"
      expected (erasedFunctionNamed "m" erased)

matchErasedScrutineeTest :: Test
matchErasedScrutineeTest = TestCase $ do
   let sideCall = callOf intType "side" [var "n"] (Just "s")
   let original = functionOf "q" [paramOf "n" intType] intType
         [ KirMatch
            { ty         = intType
            , scrutinee  = unitValue
            , arms       = [KirMatchArm
                  { tag    = TagCtor unitCtor
                  , branch = KirBranch { instructs = [sideCall], value = var "s" }
                  }]
            , fallback   = Nothing
            , dest       = DestLocal (KirLocalId 4)
            , exhaustive = True
            }
         ]
         (KirReturn (Just (KirLocalRef (KirLocalId 4))))
   let (erased, _) = eraseOne (programOf [original] [])
   let expected = functionOf "q" [paramOf "n" intType] intType
         [ sideCall
         , KirStore { ty = intType, dest = DestLocal (KirLocalId 4), value = var "s" }
         ]
         (KirReturn (Just (KirLocalRef (KirLocalId 4))))
   assertSame "a match on a zero-sized value runs its only branch directly"
      expected (erasedFunctionNamed "q" erased)

systemBindingsTest :: Test
systemBindingsTest = TestCase $ do
   let bindingOf bindingType access = KirBinding
         { bindType = bindingType, bindSrc = SrcEntity, bindAccess = access }
   let systemWith bindings instructions = KirSystem
         { name     = KirName "sys"
         , bindings = bindings
         , body     = KirBlock { instructs = instructions, term = KirReturn Nothing }
         }
   let original = systemWith
         [bindingOf unitType AccessRead, bindingOf intType AccessWrite]
         [callOf intType "ext" [KirBindingRef (KirBindingId 0), KirBindingRef (KirBindingId 1)] (Just "r")]
   let (erased, _) = eraseOne (programOf [] [original])
   let expected = systemWith
         [bindingOf erasedType AccessRead, bindingOf intType AccessWrite]
         [callOf intType "ext" [KirBindingRef (KirBindingId 1)] (Just "r")]
   assertSame "a tag binding keeps its slot but has no data, and its uses disappear"
      expected (head erased.systems)
