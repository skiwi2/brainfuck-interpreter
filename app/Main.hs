module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [program]       -> interpret' program
        ["-f", file]    -> do
            contents <- readFile file
            interpret' contents
        _               -> putStrLn "Usage: brainfuck-interpreter-exe <program> or brainfuck-interpret-exe -f <file>"

interpret' :: String -> IO ()
interpret' program = do
    memory <- interpret program
    putChar '\n'
    putStrLn ("Memory: " ++ show memory)