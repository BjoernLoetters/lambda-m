module Eval(Eval(..), fromIO, fromTry, context, with, run, orElse, fail) where

import Utility
import Context

data Eval a b = Eval (Context a -> IO (Try b))

instance Functor (Eval a) where
    fmap f (Eval g) = Eval $ \context -> do
        value <- g context
        return $ fmap f value

instance Applicative (Eval a) where
    pure value = Eval $ \context -> return $ Right value
    (Eval f) <*> (Eval a) = Eval $ \context -> do
        f' <- f context
        a' <- a context
        case (f', a') of
            (Right f, Right a) -> return $ Right $ f a
            (Left error, _) -> return $ Left error
            (_, Left error) -> return $ Left error

instance Monad (Eval a) where
    return value = Eval $ \_ -> return $ Right value
    fail message = Eval $ \_ -> return $ Left message
    (Eval g) >>= f = Eval $ \context -> do
        value <- g context
        case value of
            Left error -> return $ Left error
            Right value ->
                case f value of
                    Eval g -> g context

fromIO :: IO b -> Eval a b
fromIO io = Eval $ \_ -> fmap Right io

fromTry :: Try b -> Eval a b
fromTry try = Eval $ \_ -> return try

with :: Context a -> Eval a b -> Eval a b
with context (Eval f) = Eval $ \_ -> f context

context :: Eval a (Context a)
context = Eval $ \context -> return $ Right context

orElse :: Eval a b -> Eval a b -> Eval a b
orElse (Eval a) (Eval b) = Eval $ \context -> do
    a' <- a context
    case a' of
        Left error -> b context
        other -> return other

run :: Context a -> Eval a b -> IO (Try b)
run initial (Eval f) = f initial
