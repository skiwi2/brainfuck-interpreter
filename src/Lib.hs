module Lib
    ( interpret
    ) where

import Data.Char
import Data.List

interpret :: [Char] -> IO ()
interpret program = do
    step ([], program, [0], 0)
    return ()

-- TODO: memory should be of type Byte
-- TODO: error handling
-- TODO: introduce types
-- TODO: fix matching brackets bug
step :: ([Char], [Char], [Int], Int) -> IO ([Char], [Char], [Int], Int)
step (previousProgram, [], memory, pointer) = return (previousProgram, [], memory, pointer)
step (previousProgram, nextProgram, memory, pointer) = do
    let instruction = head nextProgram
    let (previousMemory, currentMemory) = splitAt pointer memory
    let currentMemoryCell = head currentMemory
    let nextMemory = tail currentMemory
    case instruction of
        '>' -> if pointer == length memory - 1
            then step (previousProgram ++ [instruction], tail nextProgram, memory ++ [0], pointer + 1)
            else step (previousProgram ++ [instruction], tail nextProgram, memory, pointer + 1)
        '<' -> if pointer == 0
            then step (previousProgram ++ [instruction], tail nextProgram, [0] ++ memory, 0)
            else step (previousProgram ++ [instruction], tail nextProgram, memory, pointer - 1)
        '+' -> step (previousProgram ++ [instruction], tail nextProgram, previousMemory ++ [currentMemoryCell + 1] ++ nextMemory, pointer)
        '-' -> step (previousProgram ++ [instruction], tail nextProgram, previousMemory ++ [currentMemoryCell - 1] ++ nextMemory, pointer)
        '.' -> do
            putChar . chr $ currentMemoryCell
            step (previousProgram ++ [instruction], tail nextProgram, memory, pointer)
        ',' -> do
            newCurrentChar <- getChar
            let newCurrent = ord newCurrentChar
            step (previousProgram ++ [instruction], tail nextProgram, previousMemory ++ [newCurrent] ++ nextMemory, pointer)
        '[' -> case currentMemoryCell of
            0   -> do
                let Just advancement = elemIndex ']' (tail nextProgram)
                step (previousProgram ++ [instruction] ++ (take advancement nextProgram), drop (advancement + 1) nextProgram, memory, pointer)
            _   -> step (previousProgram ++ [instruction], tail nextProgram, memory, pointer)
        ']' -> case currentMemoryCell of
            0   -> step (previousProgram ++ [instruction], tail nextProgram, memory, pointer)
            _   -> do
                let newStart = last $ elemIndices '[' previousProgram
                step (take newStart previousProgram, (drop newStart previousProgram) ++ [instruction] ++ nextProgram, memory, pointer)