module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [program]   -> interpret program
        _           -> putStrLn "Usage: brainfuck-interpreter-exe <program>"
