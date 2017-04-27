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


type Numeration = [SyntaxObj]

type Workspace = [SyntaxObj]

data SyntaxObj = Crash
               | LexItem { feats :: [Feature]
                         , str :: String
                         }
               | Branch { feats :: [Feature]
                        , left :: SyntaxObj
                        , right :: SyntaxObj
                        } deriving (Show, Eq)

data Feature = D | N | V | P | C | T | LV
             | WH | Nom | Acc
             | Minus Feature deriving (Show, Eq)

data Direct = L | R deriving (Show, Eq)

categoryFeats = [D, N, V, P, C, T, LV]
selectorFeats = [Minus D, Minus N, Minus V, Minus P, Minus C, Minus T, Minus LV]
licenseeFeats = [Minus WH, Minus Nom, Minus Acc]
licensorFeats = [WH, Nom, Acc]

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
      | null num && length ws == 1 && (feats (head ws) == [C]) = ws
      | otherwise = []

-- "Merge" two lexitems from the numeration into a syntactic object and bring that object into the workspace
mergeNum :: Numeration -> Workspace -> [(Numeration, Workspace)]
mergeNum num ws = filter (\(_,y) -> Crash `notElem` y) states where
  states = combo (\x y z -> (deleteTwo y z num, merge x y z : ws)) [L,R] num num

-- bring a lexitem from the numeration into the workspace to "Merge" with an existing syntactic object
mergeNew :: Numeration -> Workspace -> [(Numeration, Workspace)]
mergeNew num ws = filter (\(_,y) -> Crash `notElem` y) states where
  states = combo (\x y z -> (delete y num, merge x y z : delete z ws)) [L,R] num ws

-- "Merge" two existing syntactic objects that are in the workspace
mergeWS :: Numeration -> Workspace -> [(Numeration, Workspace)]
mergeWS num ws = filter (\(_,y) -> Crash `notElem` y) states where
  states = combo (\x y z -> (num, merge x y z : deleteTwo y z ws)) [L,R] ws ws

-- deletes two things from a list
deleteTwo :: Eq a => a -> a -> [a] -> [a]
deleteTwo x y zs = delete y (delete x zs)

-- combos without repeats
combo :: (Direct -> SyntaxObj -> SyntaxObj -> ([SyntaxObj], [SyntaxObj])) -> [Direct] -> [SyntaxObj] -> [SyntaxObj] -> [([SyntaxObj], [SyntaxObj])]
combo f a b c = concatMap (\y -> concatMap (\x -> map (f y x) (delete x c)) b ) a

-- Merge
merge :: Direct -> SyntaxObj -> SyntaxObj -> SyntaxObj
merge _ Crash _ = Crash
merge _ _ Crash = Crash
merge d sobj1 sobj2 = smc out where
  fs1 = feats sobj1
  fs2 = feats sobj2
  out | null fs1 || null fs2 = Crash
      | (d == R) && head fs1 == Minus (head fs2) = Branch (tail fs2) sobj1{feats=tail fs1} sobj2{feats=[]}
      | (d == R) && Minus (head fs1) == head fs2 = Branch (tail fs1) sobj2{feats=tail fs2} sobj1{feats=[]}
      | (d == L) && head fs1 == Minus (head fs2) = Branch (tail fs1) sobj1{feats=[]} sobj2{feats=tail fs2}
      | (d == L) && Minus (head fs1) == head fs2 = Branch (tail fs2) sobj2{feats=[]} sobj1{feats=tail fs1}
      | otherwise = Crash

-- Shortest Move Constraint
smc :: SyntaxObj -> SyntaxObj
smc sobj = out where
  fss = map feats (subcomps sobj)
  nn = filter (not.null) fss
  out | length nn < 2 = sobj
      | any (> 1) (map (count (map head nn)) licenseeFeats) = Crash
      | otherwise = sobj

count :: Eq a => [a] -> a -> Int
count xs x =  length $ filter (==x) xs

-- Move
move :: Numeration -> Workspace -> [(Numeration, Workspace)]
move num ws = filter (\(_,y) -> Crash `notElem` y) states where
  states = concatMap (\x -> map ((,) num . (: delete x ws)) (merge <$> [L,R] <*> subcomps x <*> [x]) ) ws

-- get all syntactic objects within a syntactic object
subcomps :: SyntaxObj -> [SyntaxObj]
subcomps Crash = []
subcomps (Branch fs sobj1 sobj2) = Branch fs sobj1 sobj2 : subcomps sobj1 ++ subcomps sobj2
subcomps li = [li]

-- Spellout
spellout :: SyntaxObj -> String
spellout Crash = "<CRASH>"
spellout (Branch [C] sobj1 sobj2) = spellout sobj1 ++ spellout sobj2
spellout (Branch [] sobj1 sobj2) = spellout sobj1 ++ spellout sobj2
spellout Branch{} = "t "
spellout (LexItem _ "") = ""
spellout (LexItem _ str) = str ++ " "
