module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [program]   -> do
            result <- interpret program
            case result of
                Left err    -> putStrLn ("Error: " ++ err)
                Right _     -> return ()
        _           -> putStrLn "Usage: brainfuck-interpreter-exe <program>"
