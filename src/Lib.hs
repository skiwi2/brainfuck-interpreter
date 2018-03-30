module Lib
    ( interpret
    ) where

import Data.Char
import Data.Maybe
import System.IO

data BFInstruction = MemoryRight | MemoryLeft | Increment | Decrement | Output | Input | LoopBegin | LoopEnd | Stop deriving (Enum, Eq, Show)
data BFProgram = BFProgram [BFInstruction] BFInstruction [BFInstruction] deriving Show

newtype BFMemoryCell = BFMemoryCell Int deriving Show
data BFMemory = BFMemory [BFMemoryCell] BFMemoryCell [BFMemoryCell] deriving Show

startProgram :: [BFInstruction] -> BFProgram
startProgram instructions = BFProgram [] (head instructions) (tail instructions ++ [Stop])

advance :: BFProgram -> BFProgram
advance (BFProgram past current next) = BFProgram (current:past) (head next) (tail next)

decrease :: BFProgram -> BFProgram
decrease (BFProgram past current next) = BFProgram (tail past) (head past) (current:next)

jumpAfterMatchingLoopEnd :: BFProgram -> BFProgram
jumpAfterMatchingLoopEnd program = jumpAfterMatchingLoopEnd' 0 (advance program)

jumpAfterMatchingLoopEnd' :: Int -> BFProgram -> BFProgram
jumpAfterMatchingLoopEnd' 0 program@(BFProgram _ LoopEnd _) = advance program
jumpAfterMatchingLoopEnd' nesting program@(BFProgram _ instruction _) = case instruction of
    LoopEnd     -> jumpAfterMatchingLoopEnd' (nesting - 1) (advance program)
    LoopBegin   -> jumpAfterMatchingLoopEnd' (nesting + 1) (advance program)
    _           -> jumpAfterMatchingLoopEnd' nesting (advance program)

jumpToMatchingLoopBegin :: BFProgram -> BFProgram
jumpToMatchingLoopBegin program = jumpToMatchingLoopBegin' 0 (decrease program)

jumpToMatchingLoopBegin' :: Int -> BFProgram -> BFProgram
jumpToMatchingLoopBegin' 0 program@(BFProgram _ LoopBegin _) = program
jumpToMatchingLoopBegin' nesting program@(BFProgram _ instruction _) = case instruction of
    LoopBegin   -> jumpToMatchingLoopBegin' (nesting - 1) (decrease program)
    LoopEnd     -> jumpToMatchingLoopBegin' (nesting + 1) (decrease program)
    _           -> jumpToMatchingLoopBegin' nesting (decrease program)

makeCell :: Int -> BFMemoryCell
makeCell = BFMemoryCell . wrap

incrementCell :: BFMemoryCell -> BFMemoryCell
incrementCell = makeCell . (+1) . getCell

decrementCell :: BFMemoryCell -> BFMemoryCell
decrementCell = makeCell . subtract 1 . getCell

getCell :: BFMemoryCell -> Int
getCell (BFMemoryCell value) = value

wrap :: Int -> Int
wrap input = mod input 256

moveMemoryRight :: BFMemory -> BFMemory
moveMemoryRight (BFMemory previous current []) = BFMemory (current:previous) (makeCell 0) []
moveMemoryRight (BFMemory previous current next) = BFMemory (current:previous) (head next) (tail next)

moveMemoryLeft :: BFMemory -> BFMemory
moveMemoryLeft (BFMemory [] current next) = BFMemory [] (makeCell 0) (current:next)
moveMemoryLeft (BFMemory previous current next) = BFMemory (tail previous) (head previous) (current:next)

onCurrentCell :: (BFMemoryCell -> BFMemoryCell) -> BFMemory -> BFMemory
onCurrentCell func (BFMemory previous current next) = BFMemory previous (func current) next

setCurrentCell :: BFMemoryCell -> BFMemory -> BFMemory
setCurrentCell cell (BFMemory previous _ next) = BFMemory previous cell next

toInstructions :: String -> [BFInstruction]
toInstructions = mapMaybe toInstruction

toInstruction :: Char -> Maybe BFInstruction
toInstruction instruction = case instruction of
    '>' -> Just MemoryRight
    '<' -> Just MemoryLeft
    '+' -> Just Increment
    '-' -> Just Decrement
    '.' -> Just Output
    ',' -> Just Input
    '[' -> Just LoopBegin
    ']' -> Just LoopEnd
    _   -> Nothing

interpret :: String -> IO BFMemory
interpret program = step (startProgram $ toInstructions program) (BFMemory [] (makeCell 0) [])

step :: BFProgram -> BFMemory -> IO BFMemory
step (BFProgram _ Stop []) memory = return memory
step program@(BFProgram _ instruction _) memory@(BFMemory _ currentMemory _) = case instruction of
    MemoryRight -> continue (moveMemoryRight memory)
    MemoryLeft  -> continue (moveMemoryLeft memory)
    Increment   -> continue (onCurrentCell incrementCell memory)
    Decrement   -> continue (onCurrentCell decrementCell memory)
    Output      -> do
        putChar . chr . getCell $ currentMemory
        hFlush stdout
        continue memory
    Input       -> do
        newCurrentChar <- getChar
        let newCurrent = makeCell . ord $ newCurrentChar
        continue (setCurrentCell newCurrent memory)
    LoopBegin   -> case getCell currentMemory of
        0   -> step (jumpAfterMatchingLoopEnd program) memory
        _   -> continue memory
    LoopEnd     -> case getCell currentMemory of
        0   -> continue memory
        _   -> step (jumpToMatchingLoopBegin program) memory
    where
        continue = step . advance $ program