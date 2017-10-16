module Min
( derive
, linearize
, move
, move2
, merge
, checkFeature
) where

import Data.List
import Data.Ord
import Control.Arrow hiding (left, right)
import Data.Tuple
import Debug.Trace

import MinData

-- Non-deterministically, do:
-- "Merge" two LexItems from the Numeration into a SyntaxObj Branch and bring that object into the Workspace
-- If the above work doesn't work:
-- "Move" some sub-SyntaxObj within an existing SyntaxObj
-- If even move doesn't work, then declare the derivation "Crash"ed

-- Converge if:
-- There is only one SyntaxObj in the Numeration
-- No Features of that SyntaxObj are Uninterp

-- Given a Numeration, return all convergent SyntaxObj
derive :: Numeration -> Workspace -> [SyntaxObj]
derive num ws = nub out where
  r | length num < 2 = []
    | otherwise = mergeNum num ws
  s | null ws || null num = []
    | otherwise = mergeNew num ws
  t | length ws < 2 = []
    | otherwise = mergeWS num ws
  u | null (r ++ s ++ t) = move num ws
    | otherwise = []
  out | (not.null) (r ++ s ++ t ++ u) = concatMap (uncurry derive) (r ++ s ++ t ++ u)
      | length ws == 1 && null num = filter converge ws
      | otherwise = []

-- True if a given SyntaxObj has no Uninterp Features within
converge :: SyntaxObj -> Bool
converge (Branch fs left right) = all isInterp fs &&
                                  converge left &&
                                  converge right
converge (LexItem fs _) = all isInterp fs
converge Trace{} = True
converge Crash = False


-- False if a given SyntaxObj has a Strong Feature within
{-checkStrong :: SyntaxObj -> Bool
checkStrong (Branch fs _ left right) = all ((Strong /= ) . strength) fs &&
                                       checkStrong left &&
                                       checkStrong right
checkStrong (LexItem fs _ _) = all ((Strong /= ) . strength) fs
checkStrong _ = True-}

-- "Merge" two LexItems from the Numeration into a SyntaxObj Branch and bring that SyntaxObj into the Workspace
mergeNum :: Numeration -> Workspace -> [(Numeration, Workspace)]
mergeNum num ws = filter (\(_,y) -> Crash `notElem` y) states where
  states = combo (\y z -> (deleteTwo y z num, merge y z : ws)) num num

-- Bring a LexItem from the Numeration into the Workspace to "Merge" with an existing SyntaxObj Branch
mergeNew :: Numeration -> Workspace -> [(Numeration, Workspace)]
mergeNew num ws = filter (\(_,y) -> Crash `notElem` y) states1 where
  states1 = combo (\y z -> (delete y num, merge y z : delete z ws)) num ws

-- "Merge" two existing SyntaxObjs that are in the Workspace
mergeWS :: Numeration -> Workspace -> [(Numeration, Workspace)]
mergeWS num ws = filter (\(_,y) -> Crash `notElem` y) states where
  states = combo (\y z -> (num, merge y z : deleteTwo y z ws)) ws ws

-- Deletes two things from a list
deleteTwo :: Eq a => a -> a -> [a] -> [a]
deleteTwo x y zs = delete y (delete x zs)

-- combos without repeats
combo :: (SyntaxObj -> SyntaxObj -> ([SyntaxObj], [SyntaxObj])) -> [SyntaxObj] -> [SyntaxObj] -> [([SyntaxObj], [SyntaxObj])]
combo f b c = concatMap (\x -> map (f x) (delete x c)) b


-- Merge
merge :: SyntaxObj -> SyntaxObj -> SyntaxObj
merge Trace{} _ = Crash
merge _ Trace{} = Crash
merge Crash _ = Crash
merge _ Crash = Crash
merge sobj1 sobj2 = out where
  bund1 = feats sobj1
  bund2 = feats sobj2

  b1 = or (checkFeature <$> bund1 <*> bund2)
  b2 = or (checkFeature <$> bund2 <*> bund1)
  out
    | b1 = Branch bund1N sobj1{feats=filter isInterp bund1} sobj2
    | b2 = Branch bund2N sobj2{feats=filter isInterp bund2} sobj1
    | otherwise = Crash
  bund1N = filter (\x -> all (not.checkFeature x) bund2) bund1
  bund2N = filter (\y -> all (not.checkFeature y) bund1) bund2

-- Check if feat1 is Uninterp and feat2 is Interp (and they are the same category)
checkFeature :: Feature -> Feature -> Bool
checkFeature (UFeature cat1) (IFeature cat2) = cat1 == cat2
checkFeature _ _ = False

-- Move
move :: Numeration -> Workspace -> [(Numeration, Workspace)]
move _ [] = []
move num ws = filter (\(_,y) -> Crash `notElem` y) j where
  j = map ((,) num) h
  h = concatMap (\x -> map (: delete x ws) (smc (move2 x))) ws

-- Shortest move constraint, sorta
smc :: [(SyntaxObj, SyntaxObj, Int)] -> [SyntaxObj]
smc blah = map (\(x,_,_) -> x) (a ++ out) where
  (a,b) = partition (\(u,v,_) -> v == Crash) blah
  foo = map (\(x,y,z) -> (merge x y, Crash, z)) b
  filt = filter (\(y, _, _) -> y /= Crash) foo
  out
    | length filt > 1 = [minimumBy (comparing (\(_,_,x) -> x)) filt]
    | otherwise = filt

-- Go through a SyntaxObj and for each subcomponent, try and merge that component with all subcomponents that c-command it
-- if it "Crash"es each time, then don't add it to the list
move2 :: SyntaxObj -> [(SyntaxObj, SyntaxObj, Int)]
move2 (Branch fs sobj1 sobj2) = f1 ++ f2 ++ up ++ pass1 ++ pass2 where
  -- pass upstream
  up = [(Branch fs (Trace (feats sobj1) (linearize sobj1)) sobj2, sobj1, 1), (Branch fs sobj1 (Trace (feats sobj2) (linearize sobj2)), sobj2, 1)]

  t1 = move2 sobj1
  t2 = move2 sobj2

  -- pass through
  pass1 = map (\(u,v,n)-> (Branch fs u sobj2,v,n+1)) t1
  pass2 = map (\(u,v,n)-> (Branch fs sobj1 u,v,n+1)) t2

  -- merge from downstream
  -- with shortest move constraint - if multiple things can merge without crashing, only let the closest one merge

  -- right branch
  foo1 = map (\(x,y,z) -> (Branch fs x (merge y sobj2), Crash, z)) t1
  -- try and merge daughters
  bar1 = (Branch fs (Trace (feats sobj1) (linearize sobj1)) (merge sobj1 sobj2), Crash, 0) : foo1
  filt1 = filter (\(y, _, _) -> right y /= Crash) bar1
  f1
    | length filt1 > 1 = [minimumBy (comparing (\(_,_,x) -> x)) filt1]
    | otherwise = filt1

  -- left branch
  foo2 = map (\(x,y,z) -> (Branch fs (merge y sobj1) x, Crash, z)) t2
  -- try and merge daughters
  bar2 = (Branch fs (merge sobj2 sobj1) (Trace (feats sobj2) (linearize sobj2)), Crash, 0) : foo2
  filt2 = filter (\(y, _, _) -> left y /= Crash) bar2
  f2
    | length filt2 > 1 = [minimumBy (comparing (\(_,_,x) -> x)) filt2]
    | otherwise = filt2

move2 _ = []


-- Agree
-- For each SyntaxObj in the Workspace, look at each subcomponent
-- And for each subcomponent, look down into it's c-command domain for a goal
agree :: Numeration -> Workspace -> [(Numeration, Workspace)]
agree _ [] = []
agree num ws = filter (\(_,y) -> Crash `notElem` y) j where
  j = map ((,) num) h
  h = concatMap (\x -> map (: delete x ws) (agree2 x)) ws

agree2 :: SyntaxObj -> [SyntaxObj]
agree2 (Branch fs sobj1 sobj2) = filter (\x -> left x /= Crash && right x /= Crash) (l ++ r ++ a ++ b) where
  l
    | agree3 sobj1 == Crash = []
    | otherwise = [Branch fs (agree3 sobj1) sobj2]
  r
    | agree3 sobj2 == Crash = []
    | otherwise = [Branch fs sobj1 (agree3 sobj2)]
  a = map (\x -> Branch fs x sobj2) (agree2 sobj1)
  b = map (Branch fs sobj1) (agree2 sobj2)

-- go through a SyntaxObj and try to find something with Interpretable Phi Features
agree3 :: SyntaxObj -> SyntaxObj
agree3 (Branch fs sobj1 sobj2) = out where
  out
    | null (searchThrough sobj2) = Crash
    | otherwise = Branch fs sobj1{feats=newFeats} sobj2
  newFeats = filter (isUPhi.category) fs ++ [IFeature (head (searchThrough sobj2))]
agree3 _ = Crash

-- returns an interpretable Phi
searchThrough :: SyntaxObj -> [Category]
searchThrough (Branch fs sobj1 sobj2)
  | not (any isPhi (map category fs)) = filter isPhi (map category fs)
  | otherwise = searchThrough sobj1 ++ searchThrough sobj2
searchThrough (LexItem fs _) = filter isPhi (map category fs)

isPhi :: Category -> Bool
isPhi (Phi UNum UPer UGen) = False
isPhi Phi{} = True
isPhi _ = False

isUPhi :: Category -> Bool
isUPhi (Phi UNum UPer UGen) = True
isUPhi _ = False


-- Linearize a SyntaxObj into a String
linearize :: SyntaxObj -> String
linearize Crash = "<CRASH>"
linearize (Branch fs sobj1 sobj2)
  | any isUninterp fs = "<CRASH>"
  | otherwise = l1 ++ space ++ l2 where
    l1 = linearize sobj1
    l2 = linearize sobj2
    space | l1 == "" || l2 == "" = ""
          | otherwise = " "
linearize Trace{} = "t"
linearize (LexItem _ "") = ""
linearize (LexItem _ str) = str


-- Helper functions
isUninterp :: Feature -> Bool
isUninterp (UFeature _) = True
isUninterp _ = False

isInterp :: Feature -> Bool
isInterp (IFeature _) = True
isInterp _ = False
