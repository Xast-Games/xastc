# XastC development code style

1. Records over positional constructors for anything with 2+ fields, with named fields everywhere:

    **BAD**:
    ```haskell
    data KirInstruct
        = KirCall Type KirName [KirValue] KirName
    ```

    **GOOD**:
    ```haskell
    data KirInstruct
        = KirCall
            { retTy  :: Type
            , callee :: KirName
            , args   :: [KirValue]
            , out    :: KirName
            }
    ```

2. Newtypes get a named field (`KirName { text :: Text }`), so you read it with name.text:

    **BAD**:
    ```haskell
    newtype KirName = KirName Text

    identFromKirName :: KirName -> Ident
    identFromKirName (KirName text) = Ident text
    ```

    **GOOD**:
    ```haskell
    newtype KirName = KirName { text :: Text }

    -- no helper needed, at the use site:
    Ident name.text
    ```

3. Type aliases for meaning (type CtorName = Ident), and a Haddock -- ^ comment on each parameter whenever a signature isn't self-explanatory:

    **BAD**:
    ```haskell
    typeTmpls :: M.Map Ident TypeDef

    mangleCtor :: Type -> Ident -> Ident
    ```

    **GOOD**:
    ```haskell
    type TypeName = Ident
    type CtorName = Ident

    typeTmpls :: M.Map TypeName TypeDef

    mangleCtor
        :: Type      -- ^ the constructor's result type, e.g. @A Int@
        -> CtorName  -- ^ the constructor's own name, e.g. @B@
        -> CtorName
    ```

4. Do not shorten names of variables, which are short themselves, and when it makes their meaning unclear (e.g. name -> nm):

    **BAD**:
    ```haskell
    nm <- requestType inst
    ctors <- forM tpl.ctors $ \c -> do
        tys <- mapM (monoType . subst sub) (payloadTypes c.payload)
        modify $ \s -> s { typeQueue = rest }
    ```

    **GOOD**:
    ```haskell
    name <- requestType instance'
    ctors <- forM template.ctors $ \ctor -> do
        fieldTypes <- mapM (monoType . subst substitution) (payloadTypes ctor.payload)
        modify $ \state -> state { typeQueue = rest }
    ```

5. Do not use `where` at the end of a function, because in such case smaller helpers are still defined in a function body, which remains too small and unclear, while helpers are written in the end and often without signature:

    **BAD**:
    ```haskell
    mangleArg :: Type -> Text
    mangleArg type' = case splitApp type' of
        (TyCon name, []) -> name.inner
        (TyCon name, args) -> wrap (mangle (TypeInst name args))
        _ -> error "unexpected type"
        where
            wrap text = "_" <> text <> "_"
    ```

    **GOOD**:
    ```haskell
    mangleArg :: Type -> Text
    mangleArg type' = case splitApp type' of
        (TyCon name, []) -> name.inner
        (TyCon name, args) -> wrapMangled (mangle (TypeInst name args))
        _ -> error "unexpected type"

    -- | Surrounds a composite argument so its boundaries are unambiguous.
    wrapMangled
        :: Text  -- ^ mangled composite argument, e.g. @Maybe_Int@
        -> Text
    wrapMangled text = "_" <> text <> "_"
    ```

6. Do not write one-liner functions with a lot of parentheses and function calls in one expression; split them into let bindings instead:

    **BAD**:
    ```haskell
    mangleCtor :: Type -> Ident -> Ident
    mangleCtor resTy c = Ident (mangle (TypeInst c (snd (splitApp resTy))))
    ```

    **GOOD**:
    ```haskell
    -- | Name of a constructor once its type has been instantiated.
    mangleCtor
        :: Type    -- ^ the constructor's type, e.g. @List Int@
        -> Ident   -- ^ the constructor's own name, e.g. @Nil@
        -> Ident
    mangleCtor resultType ctorName =
        let (_, typeArgs) = splitApp resultType
            ctorInstance = TypeInst { name = ctorName, args = typeArgs }
            mangledText = mangle ctorInstance
        in
            Ident mangledText

    ```

7. For nested loops or loops with big lambdas use `forEach` instead of `map`, forMaybe instead of `mabMaybe` and `forFlat` instead of `concatMap`:

    **BAD:**
    ```haskell
    concatMap (\container -> 
        mapMaybe (\field -> 
            fmap (, container.name) (fieldTypeName field)
        ) 
        (head container.ctors).fields
    ) 
    candidates
    ```

    **GOOD:**
    ```haskell
    L.forFlat candidates $ \container ->
        L.forMaybe (head container.ctors).fields $ \field ->
            fmap (, container.name) (fieldTypeName field)
    ```

8. Write chains of transformations left to right with `|>` instead of nesting calls or composing with `.`:

    **BAD:**
    ```haskell
    isEnum td = null (filter (not . null . (.fields)) td.ctors)
    ```

    **GOOD:**
    ```haskell
    isEnum td = td.ctors
        |> filter (\ctor -> not $ null ctor.fields)
        |> null
    ```

9. Give every condition or step with a meaning its own small top-level function with an intention-revealing name (and a short comment if the name does not say everything), instead of an inline lambda or a long expression:

    **BAD:**
    ```haskell
    filter (\ty -> case ty of { TyTuple [] -> False; _ -> True }) fields
    ```

    **GOOD:**
    ```haskell
    notUnit :: Type -> Bool
    notUnit (TyTuple []) = False
    notUnit _ = True

    fields |> filter notUnit
    ```

10. Import utility modules `qualified` (`import qualified Xast.Utils.List as L`), never `import Xast.Utils.List as L`: without `qualified` every name of the module is still imported unqualified and silently clashes with your own bindings (e.g. a local `pairs`):

    **BAD:**
    ```haskell
    import Xast.Utils.List as L
    ```

    **GOOD:**
    ```haskell
    import qualified Xast.Utils.List as L
    ```
