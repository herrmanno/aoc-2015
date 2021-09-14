{-| Given a set of 'wiring' via logic gates find
a) the final value of wire 'a'
b) the final value of wire 'a' after resetting wire 'b' to the value from a)
-}
module Day7 where

import Puzzle (Puzzle)
import Data.Map (Map, (!), empty, insert, singleton)
import qualified Data.Map as M
import Data.Bits ( Bits(xor, (.&.), (.|.), shiftL, shiftR) )
import Data.Foldable (foldl')
import Data.Either (fromRight)
import Data.Char (isAlpha, isNumber)
import Text.Parsec (parse,string, many1, satisfy, (<|>), choice, space, skipMany, try)
import Text.Parsec.Error (ParseError)
import Control.Monad.State (evalState, modify, get)
import Data.Functor ((<&>))

part1 :: Puzzle
part1 = show . eval . buildContext . map (fromRight undefined . parseWiring) . lines
    where eval ctx = evalWiring ctx "a" empty

part2 :: Puzzle
part2 s = (show . eval . buildContext . map (fromRight undefined . parseWiring) . lines) s
    where eval ctx = let b = read (part1 s) in evalWiring ctx "a" (singleton "b" b)

type Id = String

data Value = Number Int
           | Wire Id
           deriving (Show)

data BitOp = Value Value
           | And BitOp BitOp
           | Or BitOp BitOp
           | LShift BitOp BitOp
           | RShift BitOp BitOp
           | Not BitOp
           deriving (Show)

data Wiring = Wiring Id BitOp deriving (Show)

type Context = Map Id BitOp

evalWiring :: Context -> Id -> Map Id Int -> Int
evalWiring ctx ident = evalState (go ctx ident (ctx ! ident)) where
    go ctx ident op =
        case op of
            (Value (Number n)) -> return n
            (Value (Wire w)) -> do
                s <- get
                case w `M.lookup` s of
                    (Just value) -> return value
                    _ -> do
                        a <- go ctx w (ctx ! w)
                        modify (insert w a)
                        return a
            (And a b) -> mapM (go ctx "") [a,b] <&> curra (.&.)
            (Or a b) -> mapM (go ctx "") [a,b] <&> curra (.|.)
            (LShift a b) -> mapM (go ctx "") [a,b] <&> curra shiftL <&> (65535 .&.)
            (RShift a b) -> mapM (go ctx "") [a,b] <&> curra shiftR
            (Not a) -> go ctx "" a <&> xor 65535
    curra f [a,b] = f a b
    curra f _ = error "function must be applied to list of length 2"

buildContext :: [Wiring] -> Context
buildContext = foldl' f empty where
    f m (Wiring ident op) = insert ident op m

parseWiring :: String -> Either ParseError Wiring
parseWiring = parse parser "" where
    parser = flip Wiring <$> parseBitOp <*> (string "-> " *> parseId)
    parseBitOp = choice $ map try [parseAnd, parseOr, parseLShift, parseRShift, parseNot, parseValue]
    parseAnd = And <$> parseValue <*> (string "AND " *> parseValue)
    parseOr = Or <$> parseValue <*> (string "OR " *> parseValue)
    parseLShift = LShift <$> parseValue <*> (string "LSHIFT " *> parseValue)
    parseRShift = RShift <$> parseValue <*> (string "RSHIFT " *> parseValue)
    parseNot = Not <$> (string "NOT " *> parseValue)
    parseValue = Value <$> ((Wire <$> parseId) <|> (Number <$> parseNum))
    parseId = many1 (satisfy isAlpha) <* skipMany space
    parseNum = read <$> many1 (satisfy isNumber) <* skipMany space
