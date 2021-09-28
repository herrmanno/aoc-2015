{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeFamilies #-}
{-| Playing a classic RPG game
    a) what is the cheapest (by terms of 'mana spend') spell sequence to win
    b) if the player loses one hit point at each (player's) turn
-}
module Day22 where

import Puzzle (Puzzle)
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad ((>=>))
import Data.Heap (Heap, HeapItem, Prio, Val)
import qualified Data.Heap as H

-- Spell
data Spell = Missile
           | Drain
           | Shield
           | Poison
           | Recharge
           deriving (Eq,Ord,Show,Bounded,Enum)

price :: Spell -> Int
price Missile = 53
price Drain = 73
price Shield = 113
price Poison = 173
price Recharge = 229

type Effects = Map Spell Int

-- Character
data Character =
    Character { _hitPoints :: Int
              , _damage :: Int
              , _mana :: Int
              } deriving (Show)

$(makeLenses ''Character)

-- State
data State =
    State { _player :: Character
          , _enemy :: Character
          , _effects :: Effects
          , _spend :: [Spell]
          } deriving (Show)

$(makeLenses ''State)

defaultState =
    State { _player = Character 50 0 500
          , _enemy = Character 0 0 0
          , _effects = M.empty
          , _spend = []
          }

spendSum :: State -> Int

-- Result
data Result a = Pending a | Won State | Lost State

instance Functor Result where
    fmap f (Pending a) = Pending (f a)
    fmap f (Won s) = Won s
    fmap f (Lost s) = Lost s

instance Applicative Result where
    pure a = Pending a
    (Pending f) <*> (Pending a) = Pending (f a)
    (Won s) <*> _ = Won s
    (Lost s) <*> _ = Lost s
    _ <*> (Won s) = Won s
    _ <*> (Lost s) = Lost s

instance Monad Result where
    return = pure
    (Pending a) >>= f = f a
    (Won s) >>= _ = Won s
    (Lost s) >>= _ = Lost s

returnResult s = go (view (player . mana) s) (view (player . hitPoints) s) (view (enemy . hitPoints) s) where
    go m p e
        | e <= 0 = Won s
        | m <= 0 = Lost s
        | p <= 0 = Lost s
        | otherwise = Pending s


spendSum = sum . map price . view spend

-- Game flow
applySpell :: Spell -> State -> Result State
applySpell spell = returnResult
                 . over (player . mana) (subtract (price spell))
                 . over spend (spell :)
                 . go spell
    where
        go Missile  = over (enemy . hitPoints) (max 0 . subtract 4)
        go Drain    = over (player . hitPoints ) (+2)
                    . over (enemy . hitPoints) (max 0 . subtract 2)
        go Shield   = over effects (M.insert Shield 6)
        go Poison   = over effects (M.insert Poison 6)
        go Recharge = over effects (M.insert Recharge 5)

dealEffects :: State -> Result State
dealEffects = returnResult
            . dropEffects
            . (\s -> M.foldrWithKey dealEffect s (view effects s)) where
    dealEffect Poison _   = over (enemy . hitPoints) (max 0 . subtract 3)
    dealEffect Recharge _ = over (player . mana) (+101)
    dealEffect _ _        = id
    dropEffects = over effects (M.filter (>0) . M.map (subtract 1)) where
        dropEffect sp t
            | t > 1 = Just (sp, t - 1)
            | otherwise = Nothing

enemyHit :: State -> Result State
enemyHit s = returnResult $ over (player . hitPoints) (max 0 . subtract finalDamage) s where
    finalDamage = max 1 (enemyDamage - armor)
    enemyDamage = view (enemy . damage) s
    armor = 7 * M.findWithDefault 0 Shield (view effects s)

turn :: (State -> Result State) -> Spell -> State -> Result State
turn f spell = f >=> dealEffects >=> applySpell spell >=> dealEffects >=> enemyHit


-- Min Heap instance
data MinSpend

instance HeapItem MinSpend (State,Spell) where
    newtype Prio MinSpend (State,Spell) = P { prio :: Int } deriving (Eq, Ord)
    type Val MinSpend (State, Spell) = (State,Spell)

    split val@(state, spell) = let sum' = price spell + spendSum state
                               in (P sum', val)
    merge (_,val) = val


-- solution
solve1 f initState = go (H.unions $ map H.singleton initialStates) where
    initialStates = zip (repeat initState) (enumFrom Missile)
    go :: Heap MinSpend (State,Spell) -> State
    go h = let Just ((st,sp), h') = H.view h
               st' = turn f sp st
           in case st' of
               (Won a) -> a
               (Lost _) -> go h'
               (Pending a) ->
                   let ss = zip (repeat a) (enumFrom Missile)
                       h'' = foldr H.insert h' ss
                   in go h''

part1 :: Puzzle
part1 s = let [hp, dmg] = map (read . last . words) (lines s)
              result = solve1 return $ set enemy (Character hp dmg 0) defaultState
          in unlines [show result, "Mana spend: " <> show (spendSum result)]

part2 :: Puzzle
part2 s = let [hp, dmg] = map (read . last . words) (lines s)
              f = returnResult . over (player . hitPoints) (subtract 1)
              result = solve1 f $ set enemy (Character hp dmg 0) defaultState
          in unlines [show result, "Mana spend: " <> show (spendSum result)]


