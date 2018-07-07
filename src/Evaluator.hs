module Evaluator where

import Context
import Utility
import qualified Eval as Eval
import Tree
import Value
import Data.Map as Map
import Data.IORef
import Data.List as List
import Debug.Trace
import Data.Char
import System.IO

type Eval a = Eval.Eval Value a
type EvalContext = Context Value

bind :: Tree -> Eval (EvalContext, Value)
bind tree = fmap fst (f Map.empty tree) where
    f :: Map String Value -> Tree -> Eval ((EvalContext, Value), Map String Value)
    f bound (Tree.Var name) | isConstructor name = do
        context <- Eval.context
        if Context.contains name context
            then return ((context, Gen name []), bound)
            else fail ("unknown data destructor '" ++ name ++ "'")
    f bound (Tree.Var name) | Map.member name bound = do
        context <- Eval.context
        return $ ((context, bound Map.! name), bound)
    f bound (Tree.Var name) = do
        context <- Eval.context
        mvar <- Value.newMetaVar
        return $ ((Context.insert name mvar context, mvar), Map.insert name mvar bound)
    f bound Tree.Ign = do
        context <- Eval.context
        mvar <- Value.newMetaVar
        return $ ((context, mvar), bound)
    f bound (Tree.App left right) = do
        ((context', lvalue), bound') <- f bound left
        ((context'', rvalue), bound'') <- Eval.with context' $ f bound' right
        case lvalue of
            Gen name values -> return $ ((context'', Gen name (values ++ [rvalue])), bound'')
            _ -> fail "illegal data destruction"
    f bound (Tree.Tuple []) = do
        context <- Eval.context
        return $ ((context, Value.Tuple []), bound)
    f bound (Tree.Tuple (hd : tl)) = do
        ((context', hd'), bound') <- f bound hd
        ((context'', Value.Tuple tl'), bound'') <- Eval.with context' $ f bound' (Tree.Tuple tl)
        return $ ((context'', Value.Tuple (hd' : tl')), bound'')
    f bound (Tree.Num value) = do
        context <- Eval.context
        return $ ((context, Value.Num value), bound)
    f bound (Tree.Chr value) = do
        context <- Eval.context
        return $ ((context, Value.Chr value), bound)
    f _ _ = fail "illegal pattern"

fill :: Value -> Value -> Eval ()
fill (Value.MetaVar id aref) bvalue = do
    avalue <- Eval.fromIO $ readIORef aref
    case avalue of
        Just avalue -> fill avalue bvalue
        Nothing -> Eval.fromIO $ writeIORef aref (Just bvalue)
fill avalue (Value.MetaVar _ bref) = do
    bvalue <- Eval.fromIO $ readIORef bref
    case bvalue of
        Just bvalue -> fill avalue bvalue
        Nothing -> fail "non exhaustive pattern"
fill (Value.Lzy af aref) bvalue = do
    avalue <- Eval.fromIO $ readIORef aref
    case avalue of
        Just avalue -> fill avalue bvalue
        Nothing -> do
            avalue <- af
            Eval.fromIO $ writeIORef aref (Just avalue)
            fill avalue bvalue
fill avalue (Value.Lzy bf bref) = do
    bvalue <- Eval.fromIO $ readIORef bref
    case bvalue of
        Just bvalue -> fill avalue bvalue
        Nothing -> do
            bvalue <- bf
            Eval.fromIO $ writeIORef bref (Just bvalue)
            fill avalue bvalue
fill (Value.Gen aname avalues) (Value.Gen bname bvalues) | aname == bname && length avalues == length bvalues =
    fmap (\_ -> ()) $ mapM (uncurry fill) (zip avalues bvalues)
fill (Value.Tuple [avalue]) bvalue = fill avalue bvalue
fill avalue (Value.Tuple [bvalue]) = fill avalue bvalue
fill (Value.Tuple avalues) (Value.Tuple bvalues) | length avalues == length bvalues =
    fmap (\_ -> ()) $ mapM (uncurry fill) (zip avalues bvalues)
fill (Value.Num anum) (Value.Num bnum) | anum == bnum = return ()
fill (Value.Chr achr) (Value.Chr bchr) | achr == bchr = return ()
fill pattern value = do
    string <- Value.pretty value
    fail ("non exhaustive pattern for value '" ++ string ++ "'")

apply :: Value -> Value -> Eval Value
apply function value = do
    function' <- Value.zonk function
    apply' function' value
    where
        apply' :: Value -> Value -> Eval Value
        apply' (Value.Abs (Tree.Abs left right) context) argument = Eval.with context $ do
            -- Eval.fromIO $ putStrLn $ "applying " ++ show (Tree.Abs left right) ++ " to " ++ show argument
            (context', pattern) <- bind left
            Eval.with context' (fill pattern argument)
            Eval.with context' (eval right)
        apply' (Value.Ntv name function) argument = function argument
        apply' function _ = do
            string <- Value.pretty function
            fail ("can not apply value '" ++ string ++ "'")

enterConstructors :: [Tree] -> Eval EvalContext
enterConstructors constructors = do
    zero <- Eval.context
    constructors' <- mapM helper constructors
    constructors'' <- mapM mkCons constructors'
    return $ List.foldl enter zero constructors''
    where
        enter :: EvalContext -> (String, Value) -> EvalContext
        enter = flip (uncurry Context.insert)

        mkCons :: (String, [String]) -> Eval (String, Value)
        mkCons (name, arguments) = fmap ((,) name) $ (List.foldr f zero arguments) []
            where
                zero :: [Value] -> Eval Value
                zero values = return $ Value.Gen name values

                f :: String -> ([Value] -> Eval Value) -> [Value] -> Eval Value
                f argumentName f' values = return $ Value.Ntv name (\value -> f' (values ++ [value]))

        helper :: Tree -> Eval (String, [String])
        helper (App left (Var head)) = do
            (name, tail) <- helper left
            return (name, head : tail)
        helper (Var name) =
            if isConstructor name
                then return (name, [])
                else fail ("'" ++ name ++ "' is not a valid constructor name")
        helper _ = fail "illegal constructor pattern"

eval :: Tree -> Eval Value
eval (Tree.Var name) = do
    context <- Eval.context
    if Context.contains name context
        then Value.newLazy $ return $ Context.get name context
        else if isConstructor name
            then fail ("unknown constructor '" ++ name ++ "'")
            else fail ("unknown variable '" ++ name ++ "'")
eval Tree.Ign = fail "undefined"
eval (Tree.App function argument) = Value.newLazy $ do
    fvalue <- eval function
    avalue <- Value.newLazy (eval argument)
    Value.newLazy $ apply fvalue avalue
eval function @ (Tree.Abs left right) = do
    context <- Eval.context
    return $ Value.Abs function context
eval (Tree.Tuple values) = Value.newLazy $ do
    values' <- mapM (Value.newLazy . eval) values
    return $ Value.Tuple values'

eval (Tree.Num value) = Value.newLazy $ return $ Value.Num value
eval (Tree.Chr value) = Value.newLazy $ return $ Value.Chr value

eval (Tree.Match value cases) = Value.newLazy $ do
    argument <- Value.newLazy (eval value)

    let caseToFunction cs = do
        function <- eval cs
        apply function argument

    let zero = do
        string <- Value.pretty argument
        fail ("non exhaustive pattern for value '" ++ string ++ "'")

    List.foldl1 Eval.orElse (fmap caseToFunction cases ++ [zero])

eval (Tree.Let bindings tree) = Value.newLazy $ do
    binders <- mapM bindLeft bindings
    ncontext <- Eval.fromIO $ List.foldl combine Context.empty $ fmap fst (fmap fst binders)
    rhs <- mapM (Value.newLazy . (Eval.with ncontext) . eval) (fmap snd binders)
    mapM fill (zip (fmap fst binders) rhs)
    Value.newLazy $ Eval.with ncontext (eval tree)
    where
        bindLeft :: (Tree, Tree) -> Eval ((EvalContext, Value), Tree)
        bindLeft (left, right) = fmap (flip (,) right) $ bind left

        combine :: IO EvalContext -> EvalContext -> IO EvalContext
        combine acc next = acc >>= (flip Context.combine next)

        fill :: ((EvalContext, Value), Value) -> Eval ()
        fill ((context, pattern), value) = Eval.with context (Evaluator.fill pattern value)

eval (Tree.Data constructors tree) = Value.newLazy $ do
    context <- enterConstructors constructors
    Eval.with context (eval tree)

eval (Tree.Macro left right content) = do
    (context, pattern) <- bind left
    rhs <- Eval.with context (eval right)
    Eval.with context $ fill pattern rhs
    value <- apply pattern (Value.fromString content) >>= Value.zonk
    -- value should be an 'IO Tree'
    case value of
        Value.Ntv _ action -> do
            result <- action $ Value.Tuple []
            tree <- dataToTree result
            Eval.with context (eval tree)
        other -> do
            fpreview <- Value.pretty pattern
            preview <- Value.pretty other
            Eval.fail $ "illegal result value '" ++ preview ++ "' for macro implementation '" ++ fpreview ++ "'"

{- meta conversion functions -}
treeToData :: Tree -> Eval Value
treeToData (Tree.Var name) = undefined
treeToData Tree.Ign = undefined
treeToData (Tree.App left right) = undefined
treeToData (Tree.Abs left right) = undefined
treeToData (Tree.Num value) = undefined
treeToData (Tree.Chr value) = undefined
treeToData (Tree.Tuple values) = undefined
treeToData (Tree.Match value cases) = undefined
treeToData (Tree.Let bindings tree) = undefined
treeToData (Tree.Data constructors tree) = undefined
treeToData (Tree.Macro left right tree) = undefined

listToTree :: Value -> Eval [Tree]
listToTree (Value.Gen "Nil" []) = return []
listToTree (Value.Gen "Cons" [hd, tl]) = do
    hd' <- dataToTree hd
    tl' <- listToTree tl
    return $ hd' : tl'
listToTree value = do
    preview <- Value.pretty value
    Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'listToTree'"

listToTree2 :: Value -> Eval [(Tree, Tree)]
listToTree2 (Value.Gen "Nil" []) = return []
listToTree2 (Value.Gen "Cons" [Value.Tuple [left, right], tl]) = do
    left' <- dataToTree left
    right' <- dataToTree right
    tl' <- listToTree2 tl
    return $ (left', right') : tl'
listToTree2 value = do
    preview <- Value.pretty value
    Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'listToTree2'"

dataToTree :: Value -> Eval Tree
dataToTree value = (Value.zonk value) >>= dataToTree' where
    dataToTree' :: Value -> Eval Tree
    dataToTree' value @ (Value.Gen "Var" [name]) =
        case asString True name of
            Just name -> return $ Tree.Var name
            Nothing -> do
                preview <- Value.pretty value
                Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'dataToTree'"
    dataToTree' (Value.Gen "Ign" []) = return Tree.Ign
    dataToTree' (Value.Gen "App" [left, right]) = do
        left' <- dataToTree' left
        right' <- dataToTree' right
        return $ Tree.App left' right'
    dataToTree' (Value.Gen "Abs" [left, right]) = do
        left' <- dataToTree' left
        right' <- dataToTree' right
        return $ Tree.Abs left' right'
    dataToTree' (Value.Gen "Num" [Value.Num value]) = return $ Tree.Num value
    dataToTree' (Value.Gen "Chr" [Value.Chr value]) = return $ Tree.Chr value
    dataToTree' (Value.Gen "Tuple" [list]) = do
        values <- listToTree list
        return $ Tree.Tuple values
    dataToTree' (Value.Gen "Match" [value, cases]) = do
        value' <- dataToTree' value
        cases' <- listToTree cases
        return $ Tree.Match value' cases'
    dataToTree' (Value.Gen "Let" [bindings, value]) = do
        bindings' <- listToTree2 bindings
        value' <- dataToTree' value
        return $ Tree.Let bindings' value'
    dataToTree' (Value.Gen "Data" [constructors, value]) = do
        constructors' <- listToTree constructors
        value' <- dataToTree' value
        return $ Tree.Data constructors' value'
    dataToTree' value @ (Value.Gen "Macro" [left, right, content]) = do
        left' <- dataToTree' left
        right' <- dataToTree' right
        case asString True content of
            Just content -> return $ Tree.Macro left' right' content
            Nothing -> do
                preview <- Value.pretty value
                Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'dataToTree'"
    dataToTree' value = do
        preview <- Value.pretty value
        Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'dataToTree'"

{- native functions -}
insertUnary :: String -> (Value -> Eval Value) -> Context Value -> Eval (Context Value)
insertUnary name op context = do
    let native = Value.Ntv name op
    return $ Context.insert name native context

insertBinary :: String -> (Value -> Value -> Eval Value) -> Context Value -> Eval (Context Value)
insertBinary name op context = do
    let native = Value.Ntv name (\a -> do {
        preview <- Value.prettyLazy 100 a;
        return $ Value.Ntv (name ++ " " ++ preview) (op a);
    })
    return $ Context.insert name native context

liftNaturalBinary :: String -> (Integer -> Integer -> Integer) -> Value -> Value -> Eval Value
liftNaturalBinary name op a b = do
    a' <- Value.zonk a
    b' <- Value.zonk b
    case (a', b') of
        (Value.Num a, Value.Num b) -> return $ Value.Num (op a b)
        (Value.Num _, b) -> do
            preview <- Value.prettyLazy 100 b
            fail $ "illegal argument '" ++ preview ++ "' for function '" ++ name ++ "'"
        (a, _) -> do
            preview <- Value.prettyLazy 100 a
            fail $ "illegal argument '" ++ preview ++ "' for function '" ++ name ++ "'"

insertNaturalBinary :: String -> (Integer -> Integer -> Integer) -> Context Value -> Eval (Context Value)
insertNaturalBinary name op context = insertBinary name (liftNaturalBinary name op) context

liftNaturalRelation :: String -> (Integer -> Integer -> Bool) -> Value -> Value -> Eval Value
liftNaturalRelation name op a b =  do
    a' <- Value.zonk a
    b' <- Value.zonk b
    case (a', b') of
        (Value.Num a, Value.Num b) ->
            if op a b
                then return $ Value.Gen "True" []
                else return $ Value.Gen "False" []
        (Value.Num _, b) -> do
            preview <- Value.prettyLazy 100 b
            fail $ "illegal argument '" ++ preview ++ "' for function '" ++ name ++ "'"
        (a, _) -> do
            preview <- Value.prettyLazy 100 a
            fail $ "illegal argument '" ++ preview ++ "' for function '" ++ name ++ "'"

insertNaturalRelation :: String -> (Integer -> Integer -> Bool) -> Context Value -> Eval (Context Value)
insertNaturalRelation name op context = insertBinary name (liftNaturalRelation name op) context

toInt :: Value -> Eval Value
toInt value = do
    value' <- Value.zonk value
    case value' of
        Value.Num n -> return $ Value.Num n
        Value.Chr a -> return $ Value.Num $ fromIntegral $ ord a
        value -> case Value.asString True value of
            Just string -> return $ Value.Num (read string)
            Nothing -> do
                preview <- Value.prettyLazy 100 value'
                fail $ "illegal argument '" ++ preview ++ "' for function 'toInt'"

toChar :: Value -> Eval Value
toChar value = do
    value' <- Value.zonk value
    case value' of
        Value.Num n -> return $ Value.Chr $ chr $ fromIntegral n
        Value.Chr a -> return $ Value.Chr a
        _ -> do
            preview <- Value.prettyLazy 100 value'
            fail $ "illegal argument '" ++ preview ++ "' for function 'toChar'"

debug :: Value -> Value -> Eval Value
debug a b = do
     a' <- Value.zonk a
     case Value.asString True a' of
        Just message -> do
            Eval.fromIO $ hPutStrLn stderr message
            Eval.fromIO $ hFlush stderr
            return b
        Nothing -> do
            preview <- Value.prettyLazy 100 a'
            fail $ "illegal argument '" ++ preview ++ "' for function 'debug'"

insertUnaryIO :: String -> (Value -> Eval Value) -> Context Value -> Eval (Context Value)
insertUnaryIO name action context = insertUnary name fn context where
    fn :: Value -> Eval Value
    fn argument = do
        preview <- Value.prettyLazy 100 argument
        return $ Value.Ntv (name ++ " " ++ preview) (\_ -> action argument)

insertBinaryIO :: String -> (Value -> Value -> Eval Value) -> Context Value -> Eval (Context Value)
insertBinaryIO name action context = insertBinary name fn context where
    fn :: Value -> Value -> Eval Value
    fn a b = do
        a' <- Value.prettyLazy 100 a
        b' <- Value.prettyLazy 100 b
        return $ Value.Ntv (name ++ " " ++ a' ++ " " ++ b') (\_ -> action a b)

print :: Value -> Eval Value
print value = do
    value' <- Value.zonk value
    case Value.asString True value' of
        Just message -> do
            Eval.fromIO $ putStr message
            Eval.fromIO $ hFlush stdout
            return $ Value.Tuple []
        Nothing -> do
            preview <- Value.prettyLazy 100 value
            fail $ "illegal argument '" ++ preview ++ "' for function 'print'"

returnIO :: Value -> Eval Value
returnIO = return

failIO :: Value -> Eval Value
failIO value = do
    value' <- Value.zonk value
    case Value.asString True value' of
        Just message -> Eval.fail message
        Nothing -> do
            preview <- Value.pretty value
            Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'failIO'"

mapIO :: Value -> Value -> Eval Value
mapIO fn io = do
    io' <- Value.zonk io
    case io' of
        Value.Ntv _ action -> do
            argument <- action $ Value.Tuple []
            apply fn argument
        other -> do
            preview <- Value.pretty io'
            Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'mapIO'"

flatMapIO :: Value -> Value -> Eval Value
flatMapIO fn io = do
    io' <- Value.zonk io
    case io' of
        Value.Ntv _ action -> do
            argument <- action $ Value.Tuple []
            result <- apply fn argument
            result' <- Value.zonk result
            case result' of
                Value.Ntv _ action -> action $ Value.Tuple []
                other -> do
                    preview <- Value.pretty result'
                    Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'mapIO'"
        other -> do
            preview <- Value.pretty io'
            Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'mapIO'"

writeRef :: Value -> Value -> Eval Value
writeRef ref value = do
    ref' <- Value.zonk ref
    case ref' of
        Ref ref -> do
            Eval.fromIO $ writeIORef ref value
            return $ Value.Tuple []
        value -> do
            preview <- Value.pretty value
            Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'writeRef'"

readRef :: Value -> Eval Value
readRef value = do
    value' <- Value.zonk value
    case value' of
        Ref ref -> Eval.fromIO $ readIORef ref
        value -> do
            preview <- Value.pretty value
            Eval.fail $ "illegal argument '" ++ preview ++ "' for function 'readRef'"

showValue :: Value -> Eval Value
showValue value = do
    repr <- Value.pretty value
    return $ Value.fromString repr

readFileContent :: Value -> Eval Value
readFileContent path = do
    path' <- Value.zonk path
    case Value.asString True path' of
        Just path -> do
            content <- Eval.fromIO $ readFile path
            return $ fromString content
        Nothing -> do
            preview <- Value.pretty path'
            fail $ "illegal argument '" ++ preview ++ "' for function 'readFile'"


initial :: Eval (Context Value)
initial = do
    c0 <- Eval.fromIO $ Context.empty
    c1 <- insertNaturalBinary "+" (+) c0
    c2 <- insertNaturalBinary "-" (-) c1
    c3 <- insertNaturalBinary "*" (*) c2
    c4 <- insertNaturalBinary "/" div c3
    c5 <- insertNaturalBinary "%" mod c4
    c6 <- insertNaturalRelation "<" (<) c5
    c7 <- insertNaturalRelation "<=" (<=) c6
    c8 <- insertNaturalRelation ">" (>) c7
    c9 <- insertNaturalRelation ">=" (>=) c8
    c10 <- insertBinary "==" liftedEquals c9
    c11 <- insertUnary "toInt" toInt c10
    c12 <- insertBinary "debug" debug c11
    c13 <- insertUnaryIO "print" Evaluator.print c12
    c14 <- insertBinaryIO "mapIO" mapIO c13
    c15 <- insertBinaryIO "flatMapIO" flatMapIO c14
    c16 <- insertUnaryIO "returnIO" returnIO c15
    c17 <- insertUnaryIO "failIO" failIO c16
    c18 <- insertUnaryIO "newRef" Value.newRef c17
    c19 <- insertUnaryIO "readRef" readRef c18
    c20 <- insertBinaryIO "writeRef" writeRef c19
    c21 <- insertUnary "show" showValue c20
    c22 <- insertUnaryIO "readFile" readFileContent c21
    c23 <- insertUnary "toChar" toChar c22
    return c23
    where
        liftedEquals :: Value -> Value -> Eval Value
        liftedEquals a b = fmap (\b -> if b then Value.Gen "True" [] else Value.Gen "False" []) $ equals a b
