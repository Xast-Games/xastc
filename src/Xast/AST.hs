{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.AST where

import Data.List (intercalate)
import GHC.Generics (Generic)
import Data.Text (Text, unpack, pack)
import Text.Megaparsec (SourcePos)

allowedIntrinsics :: [Ident]
allowedIntrinsics =
   -- Basic operations
   [ Ident "opAdd",        Ident "opSub",          Ident "opMul"
   , Ident "opDiv",        Ident "opMod",          Ident "opPow"
   , Ident "opEq",         Ident "opNeq",          Ident "opLt"
   , Ident "opGt",         Ident "opLe",           Ident "opGe"
   , Ident "opAnd",        Ident "opOr",           Ident "opNot"
   , Ident "opPipe",       Ident "opApply",        Ident "opConcat"
   , Ident "opNeg",        Ident "opBitwiseAnd",   Ident "opBitwiseOr"
   , Ident "opBitwiseXor", Ident "opShiftLeft",    Ident "opShiftRight"
   -- 
   ]

data Located a = Located
   { location :: Location
   , node     :: a
   }
   deriving (Show, Ord, Functor, Foldable, Traversable)

instance Eq a => Eq (Located a) where
   (==) :: Eq a => Located a -> Located a -> Bool
   a == b = a.node == b.node

data Location = Location
   { pos    :: SourcePos
   , offset :: Int
   , length :: Int
   }
   deriving (Eq, Show, Ord)

sortLocByPos :: Location -> Location -> Ordering
sortLocByPos a b = compare a.pos b.pos

displayModifier :: Modifier -> String
displayModifier = \case
   FnMod (ModSharedVariant _)       -> "@SharedVariant"
   FnMod ModMemoize                 -> "@Memoize"
   FnMod ModInline                  -> "@Inline"
   FnMod (ModDeprecated _)          -> "@Deprecated"
   FnMod ModSupUnreachable          -> "@SuppressUnreachable"
   FnMod ModCompileTime             -> "@CompileTime"
   SysMod (ModCompDispatchMode _)   -> "@Mode"
   SysMod (ModLabel _)              -> "@Label"
   SysMod ModParallel               -> "@Parallel"
   TypeMod ModSingleton             -> "@Singleton"
   TypeMod ModCopyable              -> "@Copyable"
   TypeMod ModTag                   -> "@Tag"
   TypeMod ModNonExhaustive         -> "@NonExhaustive"
   ExtFnMod ModIntrinsic            -> "@Intrinsic"
data Modifier 
   = FnMod FnModifier
   | SysMod SysModifier
   | TypeMod TypeModifier
   | ExtFnMod ExtFnModifier
   deriving (Eq, Show)

data ExtFnModifier
   = ModIntrinsic
   deriving (Eq, Show)

data FnModifier
   = ModSharedVariant Ident
   | ModMemoize
   | ModInline
   | ModDeprecated Text
   | ModSupUnreachable
   | ModCompileTime
   deriving (Eq, Show)

data SysModifier
   = ModCompDispatchMode ComponentDispatchMode
   | ModLabel Ident
   | ModParallel
   deriving (Eq, Show)

data TypeModifier
   = ModSingleton
   | ModCopyable
   | ModTag
   | ModNonExhaustive
   deriving (Eq, Show)

data ComponentDispatchMode
   = CDMStrict
   | CDMSafe
   | CDMDynamic
   deriving (Eq, Show)

data Program a = Program
   { moduleDef :: ModuleDef
   , imports   :: [Located ImportDef]
   , stmts     :: [Stmt a]
   , src       :: Text
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

type ModBind = Maybe Ident

newtype Parsed = ParsedInfo
   { location :: Location
   }
   deriving (Show)

instance Eq Parsed where
   (==) :: Parsed -> Parsed -> Bool
   _ == _ = True

data Resolved = ResolvedInfo
   { location :: Location
   , res      :: Maybe Resolution
   }
   deriving (Show)

instance Eq Resolved where
   (==) :: Resolved -> Resolved -> Bool
   a == b = a.res == b.res

data Typed = TypedInfo
   { location :: Location
   , ty       :: Type
   , res      :: Maybe Resolution
   }
   deriving (Show)

instance Eq Typed where
   (==) :: Typed -> Typed -> Bool
   a == b = a.ty == b.ty && a.res == b.res

data Desugared = DesugaredInfo
   { ty  :: Type
   , res :: Maybe Resolution
   }
   deriving (Eq, Show)

desugaredAnn :: Typed -> Desugared
desugaredAnn ti = DesugaredInfo ti.ty ti.res

newtype LocalId = LocalId Int
   deriving (Eq, Ord, Show)
newtype FunctionId = FunctionId Int
   deriving (Eq, Show)
newtype ConstructorId = ConstructorId Int
   deriving (Eq, Ord, Show)
newtype ExternId = ExternId Int
   deriving (Eq, Show)

data Resolution
   = ResLocal LocalId
   | ResFunction FunctionId
   | ResConstructor ConstructorId
   | ResExternFunction ExternId
   deriving (Eq, Show)

data Expr a
   = ExpVar a ModBind Ident                     -- add, a
   | ExpCon a ModBind Ident                     -- Nothing, Just
   | ExpTuple a [Expr a]                        -- (pos, Event (p, pos));
   | ExpList a [Expr a]                         -- [a, 12, b, c]
   | ExpLit a Literal                           -- "abc", 12, ()
   | ExpLambda a (Lambda a)                     -- .\x y -> x + y
   | ExpApp a (Expr a) (Expr a)                 -- Just 12, func a b
   | ExpLetIn a (LetIn a)                       -- let a = 1 and let b = 2 in ...
   | ExpMatch a (Match a)                       -- match EXPR of
   | ExpIfThen a (IfThenElse a)                 -- if ... then ... else ...
   | ExpRecConstruct a (RecConstruct a)         -- Point { x = 12, y = 34 }
   | ExpRecUpdate a (RecUpdate a)               -- value { field = 12, field2 = True }
   | ExpVarGetter a (Expr a) Getter             -- var.x, tuple.0
   deriving (Eq, Show, Functor, Foldable, Traversable)

data Getter
   = GetField Ident
   | GetTupleField Int
   deriving (Eq, Show)

data RecConstruct a = RecConstruct
   { bind      :: ModBind
   , con       :: Ident
   , assigns   :: [RecAssign a]
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data RecUpdate a = RecUpdate
   { base      :: Expr a
   , assigns   :: [RecAssign a]
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data RecAssign a = RecAssign (Located Ident) (Expr a)
   deriving (Eq, Show, Functor, Foldable, Traversable)

data BuiltinOp 
   -- Math
   = OpPlus    -- +
   | OpNeg     -- -
   | OpMinus   -- -
   | OpMul     -- *
   | OpDiv     -- /
   | OpMod     -- %
   | OpPow     -- **
   -- Logical
   | OpEq      -- ==
   | OpNeq     -- !=
   | OpLt      -- <
   | OpGt      -- >
   | OpLe      -- <=
   | OpGe      -- >=
   | OpAnd     -- &&
   | OpOr      -- ||
   | OpNot     -- !
   | OpPipe    -- |>
   | OpApply   -- <|
   | OpConcat  -- <>
   -- Bitwise
   | OpBitAnd  -- &
   | OpBitOr   -- |
   | OpBitXor  -- ^
   | OpShl     -- <<
   | OpShr     -- >>
   deriving (Eq, Show)

data Match a = Match
   { baseExpr   :: Expr a
   , matches    :: [MatchWing a]
   , exhaustive :: Bool
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data MatchWing a = MatchWing (Pattern a) (Expr a)
   deriving (Eq, Show, Functor, Foldable, Traversable)

data IfThenElse a = IfThenElse
   { ifExpr    :: Expr a
   , thenExpr  :: Expr a
   , elseExpr  :: Expr a
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data Lambda a = Lambda
   { args :: [Pattern a]
   , body :: Expr a
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data LetIn a = LetIn
   { bindings  :: [Let a]
   , bindExpr  :: Expr a
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data Let a = Let
   { pat    :: Pattern a
   , value  :: Expr a
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data Literal
   = LitInt IntLiteral
   | LitFloat Float
   | LitDouble Double
   | LitString Text
   | LitChar Char
   | LitList [Located Literal]
   | LitTuple [Located Literal]
   deriving (Eq, Show)

data IntLiteral = IntLiteral
   { kind :: IntKind
   , value :: Integer
   }
   deriving (Eq, Show)

data IntKind
   = Size
   | Long
   | Int
   | Short
   | Byte
   | USize
   | ULong
   | UInt
   | UShort
   | UByte
   deriving (Eq, Show)

data Extern = ExtFunc ExternFunc | ExtType ExternType
   deriving (Eq, Show)

data ExternFunc = ExternFunc
   { location  :: Location
   , modifiers :: [Modifier]
   , name      :: Ident
   , args      :: [Located Type]
   , retType   :: Located Type
   }
   deriving (Eq, Show)

data ExternType = ExternType
   { location  :: Location
   , name      :: Ident
   , generics  :: [Ident]
   }
   deriving (Eq, Show)

data Func a = FnDef FuncDef | FnImpl (FuncImpl a)
   deriving (Eq, Show, Functor, Foldable, Traversable)

-- fn myFunc (Type1, Type2) -> TypeReturn
data FuncDef = FuncDef
   { location  :: Location
   , modifiers :: [Modifier]
   , name      :: Ident
   , args      :: [Located Type]
   , retType   :: Located Type
   }
   deriving (Eq, Show)

-- fn IDENT arg1 arg2 ... argN = <IMPL>
data FuncImpl a = FuncImpl
   { location :: Location
   , name     :: Ident
   , args     :: [Pattern a]
   , body     :: Expr a
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data Pattern a
   = PatVar a Ident              -- a
   | PatWildcard a               -- _
   | PatLit a Literal            -- "abc"
   | PatList a [Pattern a]       -- [a, 2, 3]
   | PatTuple a [Pattern a]      -- (a, _, 12)
   | PatCon a Ident [Pattern a]  -- Either a b
   deriving (Eq, Show, Functor, Foldable, Traversable)

patAnnotation :: Pattern a -> a
patAnnotation = \case
   PatVar a _ -> a
   PatWildcard a -> a
   PatLit a _ -> a
   PatList a _ -> a
   PatTuple a _ -> a
   PatCon a _ _ -> a

patType :: Pattern Typed -> Type
patType = (.ty) . patAnnotation

exprAnnotation :: Expr a -> a
exprAnnotation = \case
   ExpVar a _ _         -> a
   ExpCon a _ _         -> a
   ExpTuple a _         -> a
   ExpList a _          -> a
   ExpLit a _           -> a
   ExpLambda a _        -> a
   ExpApp a _ _         -> a
   ExpLetIn a _         -> a
   ExpMatch a _         -> a
   ExpIfThen a _        -> a
   ExpRecConstruct a _  -> a
   ExpRecUpdate a _     -> a
   ExpVarGetter a _ _   -> a

exprLoc :: Expr Parsed -> Location
exprLoc = (.location) . exprAnnotation

patLoc :: Pattern Parsed -> Location
patLoc = (.location) . patAnnotation

newtype Module = Module [Ident]
   deriving (Eq, Ord)

moduleToPath :: Module -> String -> String
moduleToPath (Module ids) extension = "src/" ++ concatMap (\(Ident t) -> unpack t ++ "/") (init ids) ++ unpack (let Ident t = last ids in t) ++ extension

namespacedName :: Module -> Ident -> Text
namespacedName (Module ids) name = pack $ concatMap (\(Ident t) -> unpack t ++ "_") ids ++ unpack name.inner

instance Show Module where
   show :: Module -> String
   show (Module []) = undefined
   show (Module [x]) = show x
   show (Module (x:xs)) = show x ++ "." ++ show (Module xs)

data ModuleDef = ModuleDef
   { location :: Location
   , name     :: Module
   , export   :: Located ExportPayload
   }
   deriving (Eq, Show)

data ExportPayload
   = ExpFull
   | ExpSelect [Ident]
   deriving (Eq, Show)

data ImportDef = ImportDef
   { importModule :: Module
   , payload      :: ImportPayload
   }
   deriving (Eq, Show, Ord)

data ImportPayload
   = ImpAlias (Located Ident)
   | ImpSelect [Located Ident]
   | ImpFull
   deriving (Eq, Show, Ord)

data ImportIntersection
   = InterModule (Located Module)
   | InterSelect Module [Located Ident]
   deriving (Eq, Show, Ord)

intersectIdents :: [Located Ident] -> [Located Ident] -> [Located Ident]
intersectIdents as bs = [b | b@(Located _ bi) <- bs, any (\(Located _ ai) -> ai == bi) as]

intersectImport
   :: Located ImportDef
   -> Located ImportDef
   -> Maybe ImportIntersection
intersectImport
   (Located locA (ImportDef moduleA impA))
   (Located locB (ImportDef moduleB impB)) =
      if moduleA == moduleB then
         case (impA, impB) of
            (ImpFull, _) ->
               Just (InterModule (Located locB moduleB))

            (_, ImpFull) ->
               Just (InterModule (Located locA moduleA))

            (ImpSelect as, ImpSelect bs) ->
               case intersectIdents as bs of
                  [] ->
                     Nothing
                  others ->
                     Just (InterSelect moduleB others)

            _ -> Nothing
      else
         Nothing

newtype Ident = Ident { inner :: Text }
   deriving (Eq, Ord, Generic)

instance Show Ident where
   show :: Ident -> String
   show = unpack . (.inner)

data Stmt a
   = StmtTypeDef TypeDef
   | StmtFunc (Func a)
   | StmtExtern Extern
   | StmtSystem (System a)
   deriving (Eq, Show, Functor, Foldable, Traversable)

data System a = SysDef SystemDef | SysImpl (SystemImpl a)
   deriving (Eq, Show, Functor, Foldable, Traversable)

data SystemDef = SystemDef
   { location  :: Location
   , modifiers :: [Modifier]
   , name      :: Ident
   , entities  :: [QueriedEntity]
   , retType   :: Located Type
   , with      :: Maybe [WithType]
   }
   deriving (Eq, Show)

newtype QueriedEntity = QueriedEntity [Located Type]
   deriving (Eq, Show)

data WithType
   = WithEvent (Located Type)
   | WithRes (Located Type)
   deriving (Eq, Show)

data SystemImpl a = SystemImpl
   { location  :: Location
   , name      :: Ident
   , entities  :: [EntityPattern a]
   , with      :: Maybe [Pattern a]
   , body      :: Expr a
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

newtype EntityPattern a = EntityPattern
   { bindings :: [EntPatBinding a]
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data EntPatBinding a = EntPatBinding
   { pat       :: Pattern a
   , access    :: BindingAccess
   }
   deriving (Eq, Show, Functor, Foldable, Traversable)

data BindingAccess
   = AccessRead
   | AccessWrite
   deriving (Eq, Show)

data TypeDef = TypeDef
   { location  :: Location
   , modifiers :: [Modifier]
   , name      :: Ident
   , generics  :: [Ident]
   , ctors     :: [Ctor]
   }
   deriving (Eq, Show)

data Ctor = Ctor
   { location  :: Location
   , name      :: Ident
   , payload   :: Payload
   }
   deriving (Eq, Show)

data Payload
   = PUnit
   | PTuple [Located Type]
   | PRecord [Field]
   deriving (Eq, Show)

data Field = Field      -- fieldOne : Int
   { name   :: Ident   -- field2 : Maybe Bool
   , ty     :: Located Type
   }
   deriving (Eq, Show)

data Type
   = TyGnr Ident        -- a, b, c...
   | TyCon Ident        -- Bool, Int, String
   | TyApp Type Type    -- Maybe a, Either a Int...
   | TyTuple [Type]     -- (Bool, a, Maybe String)
   | TyFn [Type] Type   -- fn(Type1, Type2 ... TypeN) -> TypeRet
   | TyVar Int          -- t0, t3
   | TyInt TyIntrinsic  -- number, concatenative
   | TyInvalid          -- <invalid>
   deriving (Eq, Ord, Show)

data TyIntrinsic
   = TyNumber
   | TyConcat
   deriving (Eq, Ord, Show)

typename :: Type -> String
typename (TyGnr ident) = show ident
typename (TyCon ident) = show ident
typename (TyTuple xs) = "(" ++ intercalate ", " (map typename xs) ++ ")"
typename (TyFn args ret) = "fn(" ++ intercalate ", " (map typename args) ++ ") -> " ++ typename ret 
typename (TyApp applicant operand) =
   let (headTy, args) = tyAppSpine applicant operand
   in unwords (typename headTy : map typenameArg args)
typename (TyVar n) = "t" ++ show (n `mod` 10)
typename (TyInt TyNumber) = "number"
typename (TyInt TyConcat) = "concat"
typename TyInvalid = "<invalid>"

tyAppSpine :: Type -> Type -> (Type, [Type])
tyAppSpine (TyApp applicant' operand') operand =
   let (headTy, args) = tyAppSpine applicant' operand'
   in (headTy, args ++ [operand])
tyAppSpine applicant operand = (applicant, [operand])

typenameArg :: Type -> String
typenameArg ty
   | needsParens ty = "(" ++ typename ty ++ ")"
   | otherwise      = typename ty
   where
      needsParens (TyApp _ _) = True
      needsParens (TyFn _ _)  = True
      needsParens _           = False