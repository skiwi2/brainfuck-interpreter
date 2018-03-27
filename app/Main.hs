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
    result <- interpret program
    case result of
        Left err    -> putStrLn ("Error: " ++ err)
        Right _     -> return ()