module Min
( Numeration
, Workspace
, SyntaxObj(..)
, Feature(..)
, derive
, spellout
, move
) where

import Data.List
import Control.Arrow
import Data.Tuple


--non-deterministically, do:
--1. "Merge" two lexitems from the numeration into a syntactic object and bring that object into the workspace
--2. bring a lexitem from the numeration into the workspace to "Merge" with an existing syntactic object
--3. "Merge" two existing syntactic objects that are in the workspace
--if none of the above work:
--4. "Move" some sub-component within an existing syntactic object
--if even move doesn't work, then declare the derivation "Crash"ed

--converge if:
--the numeration is empty
--there is only one syntactic object in the Workspace
--all features of that object have been fulfilled

{-data Parameters = Parameters {
                             ,
                             -}

type Numeration = [SyntaxObj]

type Workspace = [SyntaxObj]

data SyntaxObj = Crash
               | Trace
               | LexItem { feats :: [Feature]
                         , str :: String
                         }
               | Branch { feats :: [Feature]
                        , left :: SyntaxObj
                        , right :: SyntaxObj
                        } deriving (Show, Eq)

data Feature = D | N | V | P | C | T | LV
             | WH | Nom | Acc
             | Pl | Sing
             | Mas | Fem
             | First | Second | Third
             | Minus Feature deriving (Show, Eq)

data Direct = L | R deriving (Show, Eq)

categoryFeats = [D, N, V, P, C, T, LV]
selectorFeats = [Minus D, Minus N, Minus V, Minus P, Minus C, Minus T, Minus LV]
licenseeFeats = [Minus WH, Minus Nom, Minus Acc, Minus Pl, Minus Sing]
licensorFeats = [WH, Nom, Acc, Pl, Sing]
phiFeats = [Pl, Sing, Mas, Fem, First, Second, Third]

-- given a numeration, return all convergent syntactic objects
derive :: Numeration -> Workspace -> [SyntaxObj]
derive num ws = nub out where
  r | length num < 2 = []
    | otherwise = mergeNum num ws
  s | null ws || null num = []
    | otherwise = mergeNew num ws
  t | length ws < 2 = []
    | otherwise = mergeWS num ws
  u | all null [r, s, t] = move num ws
    | otherwise = []
  out | (not.null) (r ++ s ++ t ++ u) = concatMap (uncurry derive) (r ++ s ++ t ++ u)
      | feats (head ws) == [C] = ws
      | otherwise = []

-- "Merge" two lexitems from the numeration into a syntactic object and bring that object into the workspace
mergeNum :: Numeration -> Workspace -> [(Numeration, Workspace)]
mergeNum num ws = filter (\(_,y) -> Crash `notElem` y) states where
  states = combo (\y z -> (deleteTwo y z num, merge y z : ws)) num num

-- bring a lexitem from the numeration into the workspace to "Merge" with an existing syntactic object
mergeNew :: Numeration -> Workspace -> [(Numeration, Workspace)]
mergeNew num ws = filter (\(_,y) -> Crash `notElem` y) (states1 ++ states2) where
  states1 = combo (\y z -> (delete y num, merge y z : delete z ws)) num ws
  states2 = combo (\y z -> (delete y num, merge y z : delete y ws)) ws num

-- "Merge" two existing syntactic objects that are in the workspace
mergeWS :: Numeration -> Workspace -> [(Numeration, Workspace)]
mergeWS num ws = filter (\(_,y) -> Crash `notElem` y) states where
  states = combo (\y z -> (num, merge y z : deleteTwo y z ws)) ws ws

-- deletes two things from a list
deleteTwo :: Eq a => a -> a -> [a] -> [a]
deleteTwo x y zs = delete y (delete x zs)

-- combos without repeats
combo :: (SyntaxObj -> SyntaxObj -> ([SyntaxObj], [SyntaxObj])) -> [SyntaxObj] -> [SyntaxObj] -> [([SyntaxObj], [SyntaxObj])]
combo f b c = concatMap (\x -> map (f x) (delete x c)) b

-- Merge
-- always merges towards the first argument
-- set up so the branch with the -Feat is always on the right (Head initial?)
merge :: SyntaxObj -> SyntaxObj -> SyntaxObj
merge Trace _ = Crash
merge _ Trace = Crash
merge Crash _ = Crash
merge _ Crash = Crash
merge sobj1 sobj2 = smc out where
  fs1 = feats sobj1
  fs2 = feats sobj2
  out | null fs1 || null fs2 = Crash
      | Minus (head fs1) == head fs2 = Branch (tail fs1) sobj2{feats=tail fs2} sobj1{feats=[]}
      | head fs1 == Minus (head fs2) = Branch (tail fs1) sobj1{feats=[]} sobj2{feats=tail fs2}
      | otherwise = Crash

-- Shortest Move Constraint
smc :: SyntaxObj -> SyntaxObj
smc sobj = out where
  fss = map feats (subcomps sobj)
  nn = filter (not.null) fss
  out | length nn < 2 = sobj
      | any (> 1) (map (count (map head nn)) licenseeFeats) = Crash
      | otherwise = sobj

-- get all syntactic objects within a syntactic object
subcomps :: SyntaxObj -> [SyntaxObj]
subcomps Crash = []
subcomps Trace = []
subcomps (Branch fs sobj1 sobj2) = Branch fs sobj1 sobj2 : subcomps sobj1 ++ subcomps sobj2
subcomps li = [li]

count :: Eq a => [a] -> a -> Int
count xs x =  length $ filter (==x) xs

-- Move
move :: Numeration -> Workspace -> [(Numeration, Workspace)]
move num ws = filter (\(_,y) -> Crash `notElem` y) (states1 ++ states2) where
  states1 = concatMap (\x -> map ((,) num . (: delete x ws) . uncurry merge) (foobar x)) ws
  states2 = concatMap (\x -> map ((,) num . (: delete x ws) . uncurry merge . swap) (foobar x)) ws

-- go through a syntax object, replace a subcomponent with Trace, return the (new) syntax object and the subcomponent
foobar :: SyntaxObj -> [(SyntaxObj, SyntaxObj)]
foobar (Branch fs sobj1 sobj2) = (Branch fs Trace sobj2, sobj1) : (Branch fs sobj1 Trace, sobj2) : f1 ++ f2 where
  f1 = map (first (\x -> Branch fs x sobj2)) (foobar sobj1)
  f2 = map (first (Branch fs sobj1)) (foobar sobj2)
foobar _ = []

-- Spellout
spellout :: SyntaxObj -> String
spellout Crash = "<CRASH>"
spellout (Branch [C] sobj1 sobj2) = spellout sobj1 ++ spellout sobj2
spellout (Branch [] sobj1 sobj2) = spellout sobj1 ++ spellout sobj2
spellout Branch{} = "<ERROR?>"
spellout Trace = "t "
spellout (LexItem _ "") = ""
spellout (LexItem _ str) = str ++ " "
