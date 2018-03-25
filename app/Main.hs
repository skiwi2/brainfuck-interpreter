module Main where

import Control.Monad
import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [program]   -> void . interpret $ program
        _           -> putStrLn "Usage: brainfuck-interpreter-exe <program>"
