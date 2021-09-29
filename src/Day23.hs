{-|
-}
module Day23 where

import Puzzle (Puzzle)
import Data.Map

part1 :: Puzzle
part1 = show . runInstructions regs 0 . Prelude.map parseInstr . lines where
    regs = fromList [(A, 0), (B, 0)]

part2 :: Puzzle
part2 = show . runInstructions regs 0 . Prelude.map parseInstr . lines where
    regs = fromList [(A, 1), (B, 0)]

data Register = A | B deriving (Show, Ord, Eq)

type Registers = Map Register Int

type Offset = Int

data Instr = Hlf Register
           | Tpl Register
           | Inc Register
           | Jmp Offset
           | Jie Register Offset
           | Jio Register Offset
           deriving Show

type State = (Int, Registers)

runInstructions :: Registers -> Int -> [Instr] -> Registers
runInstructions regs ip is
  | ip < 0 || ip >= length is = regs
  | otherwise = let (ip', regs') = applyInstr (is !! ip) (ip, regs)
                in runInstructions regs' ip' is

applyInstr :: Instr -> State -> State
applyInstr (Hlf r) (ip, regs) = (ip + 1, adjust (`div` 2) r regs)
applyInstr (Tpl r) (ip, regs) = (ip + 1, adjust (*3) r regs)
applyInstr (Inc r) (ip, regs) = (ip + 1, adjust (+1) r regs)
applyInstr (Jmp o) (ip, regs) = (ip + o, regs)
applyInstr (Jie r o) (ip, regs)
    | even (regs ! r) = (ip + o, regs)
    | otherwise = (ip + 1, regs)
applyInstr (Jio r o) (ip, regs)
    | 1 == (regs ! r) = (ip + o, regs)
    | otherwise = (ip + 1, regs)

parseInstr :: String -> Instr
parseInstr = go . words where
    go ["hlf", reg] = Hlf (readRegister reg)
    go ["tpl", reg] = Tpl (readRegister reg)
    go ["inc", reg] = Inc (readRegister reg)
    go ["jmp", offset] = Jmp (readOffset offset)
    go ["jie", reg, offset] = Jie (readRegister $ init reg) (readOffset offset)
    go ["jio", reg, offset] = Jio (readRegister $ init reg) (readOffset offset)
    go xs = error $ "Bad instrcution: " <> unwords xs
    readRegister "a" = A
    readRegister "b" = B
    readRegister s = error $ "Bad register: " <> s
    readOffset ('+':xs) = read xs
    readOffset ('-':xs) = negate $ read xs
    readOffset s = error $ "Bad offset: " <> s

