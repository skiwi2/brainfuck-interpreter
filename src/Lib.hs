module Lib
    ( interpret
    ) where

import Data.Char
import Data.List
import System.IO

interpret :: [Char] -> IO (Either String [Int])
interpret program = step [] program [0] 0

-- TODO: memory should be of type Byte
-- TODO: introduce types
step :: [Char] -> [Char] -> [Int] -> Int -> IO (Either String [Int])
step _ [] memory _ = return . Right $ memory
step previousProgram currentProgram memory pointer = do
    let Just (instruction, nextProgram) = uncons currentProgram
    let (previousMemory, currentMemory) = splitAt pointer memory
    let Just (currentMemoryCell, nextMemory) = uncons currentMemory
    case instruction of
        '>' -> if pointer == length memory - 1
            then step (previousProgram ++ [instruction]) nextProgram (memory ++ [0]) (length memory)
            else step (previousProgram ++ [instruction]) nextProgram memory (pointer + 1)
        '<' -> if pointer == 0
            then step (previousProgram ++ [instruction]) nextProgram ([0] ++ memory) 0
            else step (previousProgram ++ [instruction]) nextProgram memory (pointer - 1)
        '+' -> step (previousProgram ++ [instruction]) nextProgram (previousMemory ++ [wrap $ currentMemoryCell + 1] ++ nextMemory) pointer
        '-' -> step (previousProgram ++ [instruction]) nextProgram (previousMemory ++ [wrap $ currentMemoryCell - 1] ++ nextMemory) pointer
        '.' -> do
            putChar . chr $ currentMemoryCell
            hFlush stdout
            step (previousProgram ++ [instruction]) nextProgram memory pointer
        ',' -> do
            newCurrentChar <- getChar
            let newCurrent = ord newCurrentChar
            step (previousProgram ++ [instruction]) nextProgram (previousMemory ++ [newCurrent] ++ nextMemory) pointer
        '[' -> case currentMemoryCell of
            0   -> case findMatchingLoopClose nextProgram 0 0 of
                Left err            -> return . Left $ err
                Right advancement   -> step (previousProgram ++ [instruction] ++ (take advancement nextProgram)) (drop advancement nextProgram) memory pointer
            _   -> step (previousProgram ++ [instruction]) nextProgram memory pointer
        ']' -> case currentMemoryCell of
            0   -> step (previousProgram ++ [instruction]) nextProgram memory pointer
            _   -> case findMatchingLoopOpen previousProgram 0 0 of
                Left err        -> return . Left $ err
                Right decrease  -> do
                    let newStart = (length previousProgram) - decrease
                    step (take newStart previousProgram) ((drop newStart previousProgram) ++ currentProgram) memory pointer
        _   -> step (previousProgram ++ [instruction]) nextProgram memory pointer

wrap :: Int -> Int
wrap input = mod input 256

findMatchingLoopClose :: [Char] -> Int -> Int -> Either String Int
findMatchingLoopClose [] _ _ = Left "findMatchingLoopClose: No matching ] found"
findMatchingLoopClose (instruction:remainingProgram) index nestingCounter = case instruction of
    '[' -> findMatchingLoopClose remainingProgram (index + 1) (nestingCounter + 1)
    ']' -> if nestingCounter == 0
        then Right index
        else findMatchingLoopClose remainingProgram (index + 1) (nestingCounter - 1)
    _   -> findMatchingLoopClose remainingProgram (index + 1) nestingCounter

findMatchingLoopOpen :: [Char] -> Int -> Int -> Either String Int
findMatchingLoopOpen = findMatchingLoopOpen' . reverse

findMatchingLoopOpen' :: [Char] -> Int -> Int -> Either String Int
findMatchingLoopOpen' [] _ _ = Left "findMatchingLoopOpen: No matching [ found"
findMatchingLoopOpen' (instruction:remainingProgram) index nestingCounter = case instruction of
    ']' -> findMatchingLoopOpen' remainingProgram (index + 1) (nestingCounter + 1)
    '[' -> if nestingCounter == 0
        then Right index
        else findMatchingLoopOpen' remainingProgram (index + 1) (nestingCounter - 1)
    _   -> findMatchingLoopOpen' remainingProgram (index + 1) nestingCounter