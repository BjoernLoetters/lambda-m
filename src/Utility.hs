module Utility(Try, isConstructor) where

import Data.Char

type Try a = Either String a

isConstructor :: String -> Bool
isConstructor (hd : _) = isUpper hd
isConstructor _ = False
