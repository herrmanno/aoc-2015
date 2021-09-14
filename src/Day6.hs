{-| Run sum 'toggle / turn [on,off] commands on a grid of
a) booleans
b) integers
-}
module Day6 where

import Puzzle (Puzzle)
import Data.IntMap (IntMap, fromList, alter, elems)

part1 :: Puzzle
part1 = show . length . filter id . elems . runCommands actionForCommand1 False . map parse . lines

part2 :: Puzzle
part2 = show . sum . elems . runCommands actionForCommand2 0 . map parse . lines

type Coord = (Int, Int)
type Range = (Coord, Coord)
data CommandType = Toggle | TurnOn | TurnOff
data Command = Command CommandType Range

runCommands :: ActionMap a  -- ^ the mapping function per commandtype
            -> a            -- ^ the initial light value
            -> [Command]    -- ^ the commands to apply
            -> IntMap a     -- ^ the resulting light map
runCommands actionMap initialValue = go initialMap where
    initialMap = fromList (zip [0..999999] (repeat initialValue))
    go m [] = m
    go m ((Command c r):cs) = let m' = alterMap m (rangeToIndices r) (actionMap c) in go m' cs
    alterMap m is f = foldr (alter f) m is

-- | Mapping from a CommandType to a mapping function
type ActionMap a = (forall f. Applicative f => CommandType -> (f a -> f a))

actionForCommand1 :: Applicative f => CommandType -> f Bool -> f Bool
actionForCommand1 Toggle = fmap not
actionForCommand1 TurnOn = pure . const True
actionForCommand1 TurnOff = pure . const False

actionForCommand2 :: (Functor f, Num b, Ord b) => CommandType -> f b -> f b
actionForCommand2 Toggle = fmap (+2)
actionForCommand2 TurnOn = fmap (+1)
actionForCommand2 TurnOff = fmap (max 0 . subtract 1)

coordToIndex :: Num a => (a, a) -> a
coordToIndex (a,b) = 1000 * a + b

rangeToIndices :: (Enum a, Num a) => ((a, a), (a, a)) -> [a]
rangeToIndices ((a,b),(c,d)) = [ coordToIndex (x,y) | x <- [a..c], y <- [b..d]]

parse :: String -> Command
parse s = go (words s) where
    go :: [String] -> Command
    go ("toggle":xs) = Command Toggle (parseRange xs)
    go ("turn":"on":xs) = Command TurnOn (parseRange xs)
    go ("turn":"off":xs) = Command TurnOff (parseRange xs)
    go s = error $ "Bad command '" <> concat s <> "'"
    parseRange :: [String] -> Range
    parseRange [a,_,b] = (readCoord a, readCoord b)
    parseRange s = error $ "Bad range definition '" <> concat s <> "'"
    readCoord s = let (a,_:b) = span (/=',') s in (read a, read b)
