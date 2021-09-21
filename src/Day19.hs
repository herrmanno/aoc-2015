{-| 
-}
module Day19 where

import Prelude hiding (lookup)
import Data.Map (Map, fromListWith, lookup)
import qualified Data.Set as S
import Puzzle (Puzzle)
import Data.Char (isLower)
import Data.Bifunctor (second)
import Debug.Trace
import Data.Maybe (catMaybes)

part1 :: Puzzle
part1 = show . length . uncurry produce . parse

part2 :: Puzzle
part2 s = let atoms = (toAtoms . snd . parse) s
              rns = length (filter (=="Rn") atoms)
              ars = length (filter (=="Ar") atoms)
              ys = length (filter (=="Y") atoms)
          in show $ length atoms - rns - ars - 2*ys - 1
-- part2 s = let (rs,mol) = parse s
              -- result = develop rs "e" mol
          -- in show result

type Atom = String
type Molecule = String
type Rules = Map Molecule [Molecule]

produce :: Rules -> Molecule -> [Molecule]
produce rs s = S.toList $ S.fromList $ go s where
    go [] = []
    go s = let (a,rest) = nextAtom s
           in map (a++) (go rest) ++ maybe [] (map (++rest)) (lookup a rs)

produceIterate :: Rules -> Molecule -> [[String]]
produceIterate rs = iterate produce' . (:[]) where
    produce' = (S.toList . S.fromList) . concatMap (produce rs)

develop :: Rules -> Molecule -> Molecule -> Maybe Int
develop rs start target = go 0 (toAtoms start) (toAtoms target) [] where
    go :: Int -> [Atom] -> [Atom] -> [Atom] -> Maybe Int
    go n [] [] _ = Just n
    go n s [] _ = trace ("'" <> unwords s <> "' remaning") Nothing
    go n [] t _ = trace ("No more atoms to develop! Remaining target: '" <> unwords t <> "'") Nothing
    go n (s:ss) (t:ts) acc =
        let options = filter ((==t) . fst . snd) (produces rs s)
            self = if s == t then go n ss ts (s:acc) else Nothing
            paths = map (\(m,(a,rest)) -> (n +) <$> go m (toAtoms rest ++ ss) ts (a:acc))  options
            info = unlines
                [ "Mol: " <> concat (reverse acc)
                , "Stack: " <> show (s:ss)
                , "Next: " <> t
                , "Options: " <> show (maybe [] (const [(0,(s,s))]) self ++ options)
                , "Matches (" <> s <> " == " <> t <> "): " <> show (s == t)
                ]
        in trace info $ least $ catMaybes (self : paths)
    least [] = Nothing
    least xs = Just (minimum xs)

produces :: Rules -> Molecule -> [(Int, (Atom,Molecule))]
produces rs = go 1  [] "" where
    go :: Int -> [Atom] -> Molecule -> Molecule -> [(Int, (Atom,Molecule))]
    go n acc prefix mol =
        -- let results = maybe [] (filter ((`notElem` acc) . fst) . map nextAtom) (lookup mol rs)
        let results = maybe [] (map nextAtom) (lookup mol rs)
            acc' = acc ++ map fst results
            curr = map (second (++ prefix)) results
            recur = concatMap (\(a,rest) -> go (n+1) acc' rest a) (filter ((`notElem` acc) . fst) curr)
        in zip (repeat n) curr ++ recur

toAtoms :: Molecule -> [Atom]
toAtoms [] = []
toAtoms mol = let (a,b) = nextAtom mol in a : toAtoms b

nextAtom :: Molecule -> (Atom,Molecule)
nextAtom [] = ("","")
nextAtom (x:xs) = let (rest,other) = span isLower xs in (x:rest, other)

parse :: String -> (Rules,Molecule)
parse s = (parseRules rulesS, moleculeS) where
    rulesS = init $ init (lines s)
    moleculeS = last (lines s)
    parseRules = fromListWith (++) . map parseRule . filter ((/='#') . head)
    parseRule xs = let [from,_,to] = words xs in (from,[to])

-- getInput = readFile "input/19.txt" >>= return . parse
-- getInput' = readFile "input/19_test2.txt" >>= return . parse
