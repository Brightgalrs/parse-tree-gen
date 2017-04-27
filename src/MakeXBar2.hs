module MakeXBar2
( makeXP
) where

import           Control.Monad
import           XBarType2
import           LoadData
import           Data.RVar
import           Data.Random.Extras hiding (shuffle)
import           Data.Random hiding (sample)

-- Given an XP, what specifiers are allowed?
allowedSpec :: LexCat -> [LexCat]
allowedSpec Null = [Null]
allowedSpec Comp = [Null]
allowedSpec Infl = [Det]
allowedSpec Verb = [Null]
allowedSpec Det  = [Null]
allowedSpec Noun = [Null]
allowedSpec Prep = [Null]
allowedSpec Adj  = [Null]
allowedSpec Adv  = [Null, Adv]
allowedSpec Neg  = [Null]
allowedSpec Quan = [Det]
allowedSpec Agr  = [Null]

-- Given an XBar, what adjuncts are allowed?
allowedAdjunt :: LexCat -> [LexCat]
allowedAdjunt Null = [Null]
allowedAdjunt Comp = [Null]
allowedAdjunt Infl = [Null]
allowedAdjunt Verb = [Null, Adv, Prep]
allowedAdjunt Det  = [Null]
allowedAdjunt Noun = [Null, Adj, Prep]
allowedAdjunt Prep = [Null, Prep]
allowedAdjunt Adj  = [Null, Adj, Prep]
allowedAdjunt Adv  = [Null]
allowedAdjunt Neg  = [Null]
allowedAdjunt Quan = [Null]
allowedAdjunt Agr  = [Null] -- ??

-- Given an XBar, what complements are allowed?
allowedComp :: LexCat -> [LexCat]
allowedComp Null = [Null]
allowedComp Comp = [Infl]
allowedComp Infl = [Neg, Verb]
allowedComp Verb = [Comp, Det]
allowedComp Det  = [Noun]
allowedComp Noun = [Null, Prep]
allowedComp Prep = [Det]
allowedComp Adj  = [Null, Prep]
allowedComp Adv  = [Null]
allowedComp Neg  = [Verb]
allowedComp Quan = [Null]
allowedComp Agr  = [Null] -- ??

choiceFoo :: CatLimit -> [LexCat] -> RVar (CatLimit, LexCat)
choiceFoo lims cats = do
  -- filter
  let foo x | x == Null && nullL lims > 0 = True
            | x == Comp && compL lims > 0 = True
            | x == Infl && inflL lims > 0 = True
            | x == Verb && verbL lims > 0 = True
            | x == Det && detL lims > 0 = True
            | x == Noun && nounL lims > 0 = True
            | x == Prep && prepL lims > 0 = True
            | x == Adj && adjL lims > 0 = True
            | x == Adv && advL lims > 0 = True
            | x == Neg && negL lims > 0 = True
            | x == Quan && quanL lims > 0 = True
            | x == Agr && agrL lims > 0 = True
            | otherwise = False

  -- choose
  cat <- choice (filter foo cats)

  -- update
  let x = case cat of Null -> lims{nullL = nullL lims}
                      Comp -> lims{compL = compL lims - 1}
                      Infl -> lims{inflL = inflL lims - 1}
                      Verb -> lims{verbL = verbL lims - 1}
                      Det -> lims{detL = detL lims - 1}
                      Noun -> lims{nounL = nounL lims - 1}
                      Prep -> lims{prepL = prepL lims - 1}
                      Adj -> lims{adjL = adjL lims - 1}
                      Adv -> lims{advL = advL lims - 1}
                      Neg -> lims{negL = negL lims - 1}
                      Quan -> lims{quanL = quanL lims - 1}
                      Agr -> lims{agrL = agrL lims - 1}
  return (x, cat)

-- Make the XBar tree
makeXP :: InputData -> CatLimit -> LexCat -> RVar (Phrase LexCat)
makeXP _ _ Null = return XPNull
makeXP idata lims cat = do
  (newLims, newCat) <- choiceFoo lims (allowedSpec cat)
  join $ choice [ XP cat Ini <$> makeXP idata newLims newCat <*> makeXBar idata newLims cat
                ]

makeXBar :: InputData -> CatLimit -> LexCat -> RVar (Bar LexCat)
makeXBar idata lims cat = do
  (newLims1, newCat1) <- choiceFoo lims (allowedAdjunt cat)
  (newLims2, newCat2) <- choiceFoo lims (allowedComp cat)
  let foo | newCat1 == Null = [XBar2 cat Ini <$> makeX idata cat <*> makeXP idata newLims2 newCat2]
          | newCat1 == Adj = [ XBar1 cat Ini <$> makeXP idata newLims1 newCat1 <*> makeXBar idata newLims1 cat
                             , XBar2 cat Ini <$> makeX idata cat <*> makeXP idata newLims2 newCat2
                             ]
          | otherwise = [ XBar1 cat Fin <$> makeXP idata newLims1 newCat1 <*> makeXBar idata newLims1 cat
                        , XBar2 cat Ini <$> makeX idata cat <*> makeXP idata newLims2 newCat2
                        ]
  join $ choice foo

makeX :: InputData -> LexCat -> RVar (Head LexCat)
makeX idata Null = Head Null <$> return "" -- should never get to this, probably
makeX idata Comp = Head Comp <$> choice (iComp idata)
makeX idata Infl = HInfl Infl <$> choice (iInfl idata)
makeX idata Verb = Head Verb <$> choice (iVerb idata)
makeX idata Det  = Head Det <$> choice (iDet idata)
makeX idata Noun = Head Noun <$> choice (iNoun idata)
makeX idata Prep = Head Prep <$> choice (iPrep idata)
makeX idata Adj  = Head Adj <$> choice (iAdj  idata)
makeX idata Adv  = Head Adv <$> choice (iAdv  idata)
makeX idata Neg  = Head Neg <$> return "not" --temp
makeX idata Quan = Head Quan <$> return "both" --temp
