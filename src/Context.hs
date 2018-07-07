module Context(Context(..), empty, combine, contains, get, next, insert, keys) where

import qualified Data.Map as Map
import Data.IORef

data Context a = Context (IORef Integer) (Map.Map String a)

instance Show a => Show (Context a) where
    show (Context _ map) = show map

keys :: Context a -> [String]
keys (Context _ map) = Map.keys map

empty :: IO (Context a)
empty = do
    ref <- newIORef 0
    return $ Context ref Map.empty

combine :: Context a -> Context a -> IO (Context a)
combine (Context aref amap) (Context bref bmap) = do
    acount <- readIORef aref
    bcount <- readIORef bref
    if acount < bcount
        then return $ Context bref (Map.union bmap amap)
        else return $ Context aref (Map.union bmap amap)

insert :: String -> a -> Context a -> Context a
insert key value (Context ref map) = Context ref (Map.insert key value map)

contains :: String -> Context a -> Bool
contains key (Context _ map) = Map.member key map

get :: String -> Context a -> a
get key (Context _ map) = map Map.! key

next :: Context a -> IO Integer
next (Context counter _) = do
    value <- readIORef counter
    writeIORef counter (value + 1)
    return value
