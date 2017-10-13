module Min
( derive
, spellout
, move
, move2
, merge
, checkFeature
) where

import Data.List
import Data.Ord
import Control.Arrow
import Data.Tuple
import Debug.Trace

import MinData

-- Non-deterministically, do:
-- 1. "Merge" two LexItems from the Numeration into a SyntaxObj Branch and bring that object into the Workspace
-- 2. Bring a LexItem from the Numeration into the Workspace to "Merge" with an existing SyntaxObj Branch
-- 3. "Merge" two existing SyntaxObjs that are in the Workspace
-- If none of the above work:
-- 4. "Move" some sub-SyntaxObj within an existing SyntaxObj
-- If even move doesn't work, then declare the derivation "Crash"ed

-- Converge if:
-- The Numeration is empty
-- There is only one SyntaxObj in the Workspace
-- All features of that SyntaxObj have been fulfilled

categoryFeats = [D, N, V, P, C, T, LV]

-- given a Numeration, return all convergent SyntaxObj
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
      | null num && length ws == 1 = ws
      | otherwise = []

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
merge Trace _ = Crash
merge _ Trace = Crash
merge Crash _ = Crash
merge _ Crash = Crash
merge sobj1 sobj2 = out where
  bund1 = feats sobj1
  bund2 = feats sobj2
  ph1 = phis sobj1
  ph2 = phis sobj2

  b1 = or (checkFeature <$> bund1 <*> bund2)
  b2 = or (checkFeature <$> bund2 <*> bund1)
  out
    | b1 = Branch bund1N ph2 sobj1{feats=filter (\x -> interp x == Interp) bund1} sobj2
    | b2 = Branch bund2N ph1 sobj2{feats=filter (\y -> interp y == Interp) bund2} sobj1
    | otherwise = Crash
  bund1N = filter (\x -> all (not.checkFeature x) bund2) bund1
  bund2N = filter (\y -> all (not.checkFeature y) bund1) bund2

-- checks if feat1 is Uninterp and feat2 is Interp (and they are the same category)
checkFeature :: Feature -> Feature -> Bool
checkFeature feat1 feat2 = interp feat1 == Uninterp &&
                           interp feat2 == Interp &&
                           category feat1 == category feat2

-- Move
move :: Numeration -> Workspace -> [(Numeration, Workspace)]
move _ [] = []
move num ws = filter (\(_,y) -> Crash `notElem` y) j where
  j = map ((,) num) h
  h = concatMap (\x -> map ((: delete x ws) . (\(u,v,_) -> if v /= Crash then merge u v else u)) (move2 x)) ws

-- Go through a SyntaxObj and for each subcomponent, try and merge that component with all subcomponents that c-command it
-- if it "Crash"es each time, then don't add it to the list
move2 :: SyntaxObj -> [(SyntaxObj, SyntaxObj, Int)]
move2 (Branch fs ph sobj1 sobj2) = f1 ++ f2 ++ up ++ pass1 ++ pass2 where
  -- pass upstream
  up = [(Branch fs ph Trace sobj2, sobj1, 1), (Branch fs ph sobj1 Trace, sobj2, 1)]

  t1 = move2 sobj1
  t2 = move2 sobj2

  -- pass through
  pass1 = map (\(u,v,n)-> (Branch fs ph u sobj2,v,n+1)) t1
  pass2 = map (\(u,v,n)-> (Branch fs ph sobj1 u,v,n+1)) t2

  -- merge from downstream
  -- this needs a shortest move constraint
  -- if multiple things can merge without crashing, only let the closest one merge
  -- need to keep track of distance

  -- right branch
  foo1 = map (\(x,y,z) -> (Branch fs ph x (merge y sobj2), Crash, z)) t1
  -- try and merge daughters
  bar1 = (Branch fs ph Trace (merge sobj1 sobj2), Crash, 0) : foo1
  filt1 = filter (\(Branch _ _ _ y, _, _) -> y /= Crash) bar1
  f1
    | length filt1 > 1 = [minimumBy (comparing (\(_,_,x) -> x)) filt1]
    | otherwise = filt1

  -- left branch
  foo2 = map (\(x,y,z) -> (Branch fs ph (merge y sobj1) x, Crash, z)) t2
  -- try and merge daughters
  bar2 = (Branch fs ph (merge sobj2 sobj1) Trace, Crash, 0) : foo2
  filt2 = filter (\(Branch _ _ y _, _, _) -> y /= Crash) bar2
  f2
    | length filt2 > 1 = [minimumBy (comparing (\(_,_,x) -> x)) filt2]
    | otherwise = filt2

move2 _ = []

-- Agree
-- Might not need?
{-
agree :: Numeration -> Workspace -> [(Numeration, Workspace)]
agree num ws = filter (\(_,y) -> Crash `notElem` y) states1 where
  states1 = concatMap (\x -> map ((,) num . (: delete x ws) . uncurry merge) (foobar x)) ws
-}


-- Spellout
spellout :: SyntaxObj -> String
spellout Crash = "<CRASH>"
spellout (Branch fs _ sobj1 sobj2)
  | any ((Uninterp == ) . interp) fs = "<CRASH>"
  | otherwise = spellout sobj1 ++ spellout sobj2
spellout Trace = "t "
spellout (LexItem _ _ "") = ""
spellout (LexItem _ _ str) = str ++ " "
