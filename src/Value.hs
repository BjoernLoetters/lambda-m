module Value(Value(..), newMetaVar, zonk, equals, newLazy, newRef, pretty, zonkLazy, prettyLazy, zonkStep, asString, fromString) where

import Utility
import Context
import Eval
import qualified Tree as Tree
import Data.List as List
import Data.IORef
import Data.Char
import Data.Maybe
import System.IO

data Value = MetaVar Integer (IORef (Maybe Value))
           | Lzy (Eval Value Value) (IORef (Maybe Value))
           | Ref (IORef Value)
           | Abs Tree.Tree (Context Value)
           | Ntv String (Value -> Eval Value Value)
           | Gen String [Value]
           | Tuple [Value]
           | Num Integer
           | Chr Char

instance Show Value where
    show (MetaVar id _) = "MetaVar(" ++ show id ++ ")"
    show (Lzy _ _) = "Lazy()"
    show (Abs tree _) = "Abs(" ++ show tree ++ ")"
    show (Ntv name _) = "Ntv(" ++ name ++ ")"
    show (Gen name values) = "Gen(" ++ name ++ ", " ++ (intercalate ", " (fmap show values)) ++ ")"
    show (Tuple values) = "Tuple(" ++ (intercalate ", " (fmap show values)) ++ ")"
    show (Num value) = "Num(" ++ show value ++ ")"
    show (Chr value) = "Chr(" ++ show value ++ ")"
    show (Ref _) = "Ref()"

asString :: Bool -> Value -> Maybe String
asString False (Gen "Nil" []) = Nothing
asString _ value = helper value where
    helper :: Value -> Maybe String
    helper (Gen "Cons" ((Chr hd) : tl : [])) = do
        tl' <- helper tl
        return (hd : tl')
    helper (Gen "Nil" []) = return ""
    helper _ = Nothing

fromString :: String -> Value
fromString [] = Gen "Nil" []
fromString (hd : tl) = Gen "Cons" [Chr hd, fromString tl]

inBraces :: String -> String
inBraces string = "(" ++ string ++ ")"

withBraces :: (Value -> Eval Value String) -> Value -> Eval Value String
withBraces continuation value @ (Gen _ list) | not (null list) && isNothing (asString False value) = fmap inBraces (continuation value)
withBraces continuation value @ (Ntv _ _) = fmap inBraces (continuation value)
withBraces continuation value @ (Abs _ _) = fmap inBraces (continuation value)
withBraces continuation value @ (Ref _) = fmap inBraces (continuation value)
withBraces continuation value = continuation value

pretty :: Value -> Eval Value String
pretty value = do
    value' <- zonk value
    f value'
    where
        f :: Value -> Eval Value String
        f (MetaVar id _) = return $ (['a' .. 'z'] !! (fromIntegral $ id `mod` 26)) : (show (id `div` 26))
        f (Ntv repr _) = return repr
        f (Abs tree _) = return $ Tree.pretty tree
        f (Gen name []) = return name
        f value @ (Gen name values) =
            case asString False value of
                Just string -> return $ "\"" ++ string ++ "\""
                Nothing -> do
                    values' <- mapM (withBraces f) values
                    return $ name ++ " " ++ (List.intercalate " " values')
        f (Tuple values) = do
            values' <- mapM f values
            return $ "(" ++ (List.intercalate ", " values') ++ ")"
        f (Num value) = return $ show value
        f (Chr value) = return $ show value
        f (Ref ref) = do
            value <- Eval.fromIO $ readIORef ref
            string <- withBraces f value
            return $ "Ref " ++ string

prettyLazy :: Integer -> Value -> Eval Value String
prettyLazy upto value = do
    value' <- zonkLazy upto value
    f value'
    where
        f :: Value -> Eval Value String
        f (MetaVar id _) = return $ (['a' .. 'z'] !! (fromIntegral $ id `mod` 26)) : (show (id `div` 26))
        f (Ntv repr _) = return repr
        f (Abs tree _) = return $ Tree.pretty tree
        f (Gen name []) = return name
        f value @ (Gen name values) =
            case asString False value of
                Just string -> return $ "\"" ++ string ++ "\""
                Nothing -> do
                    values' <- mapM (withBraces f) values
                    return $ name ++ " " ++ (List.intercalate " " values')
        f (Tuple values) = do
            values' <- mapM f values
            return $ "(" ++ (List.intercalate ", " values') ++ ")"
        f (Num value) = return $ show value
        f (Chr value) = return $ show value
        f (Lzy _ _) = return "..."
        f (Ref ref) = do
            value <- Eval.fromIO $ readIORef ref
            string <- withBraces f value
            return $ "Ref " ++ string

newMetaVar :: Eval Value Value
newMetaVar = do
    ref <- Eval.fromIO $ newIORef Nothing
    context <- Eval.context
    id <- Eval.fromIO $ Context.next context
    return $ MetaVar id ref

newRef :: Value -> Eval Value Value
newRef value = do
    ref <- Eval.fromIO $ newIORef value
    return $ Ref ref

newLazy :: Eval Value Value -> Eval Value Value
newLazy f = do
    context <- Eval.context
    ref <- Eval.fromIO $ newIORef Nothing
    return $ Lzy (Eval.with context f) ref

zonkStep :: Value -> Eval Value Value
zonkStep mvar @ (MetaVar _ ref) = do
    value <- Eval.fromIO $ readIORef ref
    case value of
        Just value -> return value
        Nothing -> return mvar
zonkStep (Lzy f ref) = do
    value <- Eval.fromIO $ readIORef ref
    case value of
        Just value -> return value
        Nothing -> do
            value' <- f
            Eval.fromIO $ writeIORef ref (Just value')
            return value'
zonkStep (Gen name values) = do
    values' <- mapM zonkStep values
    return $ Gen name values'
zonkStep (Ref ref) = do
    value <- (Eval.fromIO $ readIORef ref) >>= zonkStep
    Eval.fromIO $ writeIORef ref value
    return $ Ref ref
zonkStep (Tuple [value]) = zonkStep value
zonkStep (Tuple values) = do
    values' <- mapM zonkStep values
    return $ Tuple values'
zonkStep value = return value

zonk :: Value -> Eval Value Value
zonk mvar @ (MetaVar _ ref) = do
    value <- Eval.fromIO $ readIORef ref
    case value of
        Just value -> zonk value
        Nothing -> return mvar
zonk (Lzy f ref) = do
    value <- Eval.fromIO $ readIORef ref
    case value of
        Just value -> do
            value' <- zonk value
            Eval.fromIO $ writeIORef ref (Just value')
            return value'
        Nothing -> do
            value' <- f >>= zonk
            Eval.fromIO $ writeIORef ref (Just value')
            return value'
zonk (Ref ref) = do
    value <- (Eval.fromIO $ readIORef ref) >>= zonk
    Eval.fromIO $ writeIORef ref value
    return $ Ref ref
zonk (Gen name values) = do
    values' <- mapM zonk values
    return $ Gen name values'
zonk (Tuple [value]) = zonk value
zonk (Tuple values) = do
    values' <- mapM zonk values
    return $ Tuple values'
zonk value = return value

zonkLazy :: Integer -> Value -> Eval Value Value
zonkLazy upto mvar @ (MetaVar _ ref) | upto > 0 = do
    value <- Eval.fromIO $ readIORef ref
    case value of
        Just value -> zonkLazy (upto - 1) value
        Nothing -> return mvar
zonkLazy upto (Lzy f ref) | upto > 0 = do
    value <- Eval.fromIO $ readIORef ref
    case value of
        Just value -> do
            value' <- zonkLazy (upto - 1) value
            Eval.fromIO $ writeIORef ref (Just value')
            return value'
        Nothing -> do
            value' <- f >>= (zonkLazy (upto - 1))
            Eval.fromIO $ writeIORef ref (Just value')
            return value'
zonkLazy upto (Ref ref) | upto > 0 = do
    value <- (Eval.fromIO $ readIORef ref) >>= (zonkLazy (upto - 1))
    Eval.fromIO $ writeIORef ref value
    return $ Ref ref
zonkLazy upto (Gen name values) | upto > 0 = do
    values' <- mapM (zonkLazy (upto - 1)) values
    return $ Gen name values'
zonkLazy upto (Tuple [value]) | upto > 0 = zonkLazy upto value
zonkLazy upto (Tuple values) | upto > 0 = do
    values' <- mapM (zonkLazy (upto - 1)) values
    return $ Tuple values'
zonkLazy upto value = return value

equals :: Value -> Value -> Eval Value Bool
equals a b = do
    a' <- zonk a
    b' <- zonk b
    f a' b'
    where
        f :: Value -> Value -> Eval Value Bool
        f (MetaVar aid _) (MetaVar bid _) = return $ aid == bid
        f (Gen aname avalues) (Gen bname bvalues) | aname == bname && length avalues == length bvalues =
            fmap (all id) (mapM (uncurry f) (zip avalues bvalues))
        f (Tuple avalues) (Tuple bvalues) | length avalues == length bvalues =
            fmap (all id) (mapM (uncurry f) (zip avalues bvalues))

        f (Abs atree _) (Abs btree _) = return $ atree == btree
        f (Ntv arepr _) (Ntv brepr _) = return $ arepr == brepr
        f (Num avalue) (Num bvalue) = return $ avalue == bvalue
        f (Chr avalue) (Chr bvalue) = return $ avalue == bvalue
        f (Ref aref) (Ref bref) = do
            a <- Eval.fromIO $ readIORef aref
            b <- Eval.fromIO $ readIORef bref
            f a b
        f _ _ = return False
