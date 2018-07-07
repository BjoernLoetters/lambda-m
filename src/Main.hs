module Main where

import qualified Tree as Tree
import System.IO
import Utility
import Data.Char as Char
import qualified Value as Value
import Evaluator
import Context
import Eval
import Value
import Data.Time.Clock

prelude :: IO String
prelude = readFile "bootstrap/prelude.lm"

strip :: String -> String
strip = f . f where f = reverse . dropWhile Char.isSpace

run :: String -> (Value -> Eval.Eval Value ()) -> IO ()
run code action = do
    predef <- prelude
    case Tree.parse (predef ++ " " ++ code) of
        Left error -> putStrLn error
        Right tree -> do
            empty <- Context.empty
            initial <- Eval.run empty Evaluator.initial
            case initial of
                Left error -> putStrLn error
                Right initial -> do
                    value <- Eval.run initial (Evaluator.eval tree >>= action)
                    case value of
                        Left error -> putStrLn error
                        Right value -> return ()

preview :: Value -> Eval.Eval Value ()
preview value = do
    value' <- Value.prettyLazy 1000 value
    Eval.fromIO $ putStrLn value'

execute :: Value -> Eval.Eval Value ()
execute value = do
    value' <- Value.zonk value
    case value' of
        Value.Ntv name action -> do
            result <- action $ Value.Tuple []
            preview result
        other -> do
            preview <- Value.pretty value'
            Eval.fail $ "can not execute value '" ++ preview ++ "'"

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    line <- getLine
    case strip line of
        [] -> return ()
        ':' : 't' : rest -> do
            start <- getCurrentTime
            Main.run rest preview
            end <- getCurrentTime
            putStrLn $ "took " ++ show (diffUTCTime end start) ++ " to execute"
        ':' : 'x' : rest -> Main.run rest execute
        rest -> Main.run rest preview
    main
