module MakeXBarXP
( makeSentence
) where

import           Control.Monad
import           Data.Random.Extras hiding (shuffle)
import           Data.RVar
import           LoadData

import           MakeX
import           Prelude
import           XBarType

--Top level
makeSentence :: InputData -> (Int,Int,Int,Int,Int) -> RVar Sentence
makeSentence idata limits = join $ choice pickFrom where
  focj | cjl > 0 = [ makeSentence2 idata (cpl, ppl, ajl, avl, cjl-1)
                   ]
       | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickFrom = focj ++ [makeSentence1 idata limits]

makeSentence1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar Sentence
makeSentence1 idata limits =
  Sentence1 <$> makeTenseP idata limits

makeSentence2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar Sentence
makeSentence2 idata limits =
  Sentence2 <$> makeConjP8 idata limits

--Make X'

--NounBar main
makeNounBar :: InputData -> (Int,Int,Int,Int,Int) -> RVar NounBar
makeNounBar idata limits = join $ choice pickfrom where
  foop | ppl > 0 = [ makeNounBar3 idata (cpl, ppl-1, ajl, avl, cjl)
                   , makeNounBar4 idata (cpl, ppl-1, ajl, avl, cjl)
                   ]
       | otherwise = []
  fooj | ajl > 0 = [ makeNounBar1 idata (cpl, ppl, ajl-1, avl, cjl)
                   ]
       | otherwise = []
  fooc | cjl > 0 = [ makeNounBar5 idata (cpl, ppl, ajl, avl, cjl-1)
                   ]
       | otherwise = []
  fojc | ajl > 0 && cjl > 0 = [ makeNounBar2 idata (cpl, ppl, ajl-1, avl, cjl-1)
                              ]
       | otherwise = []
  fopc | ppl > 0 && cjl > 0 = [ makeNounBar5 idata (cpl, ppl-1, ajl, avl, cjl-1)
                              ]
       | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickfrom = foop ++ fooj ++ fooc ++ fojc ++ fopc ++ [makeNounBar6 idata limits]

--AdjP + NounBar
makeNounBar1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar NounBar
makeNounBar1 idata limits =
  NounBar1 <$> makeAdjP idata limits <*> makeNounBar idata limits

makeNounBar2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar NounBar
makeNounBar2 idata limits =
  NounBar2 <$> makeConjP4 idata limits <*> makeNounBar idata limits

--NounBar + PrepP
makeNounBar3 :: InputData -> (Int,Int,Int,Int,Int) -> RVar NounBar
makeNounBar3 idata limits =
  NounBar3 <$> makeNounBar idata limits <*> makePrepP idata limits

--Noun with PrepP
makeNounBar4 :: InputData -> (Int,Int,Int,Int,Int) -> RVar NounBar
makeNounBar4 idata limits =
  NounBar4 <$> makeNoun idata <*> makePrepP idata limits

--Noun with two PrepP
makeNounBar5 :: InputData -> (Int,Int,Int,Int,Int) -> RVar NounBar
makeNounBar5 idata limits =
  NounBar5 <$> makeNoun idata <*> makeConjP5 idata limits

--Just Noun
makeNounBar6 :: InputData -> (Int,Int,Int,Int,Int) -> RVar NounBar
makeNounBar6 idata limits =
  NounBar6 <$> makeNoun idata

--VerbBar main
makeVerbBar :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar idata limits = join $ choice pickfrom where
  fooc | cpl > 0 = [ makeVerbBar7 idata (cpl-1, ppl, ajl, avl, cjl)
                   ]
       | otherwise = []
  foop | ppl > 0 = [ makeVerbBar5 idata (cpl, ppl-1, ajl, avl, cjl)
                   ]
       | otherwise = []
  foov | avl > 0 = [ makeVerbBar1 idata (cpl, ppl, ajl, avl-1, cjl)
                   , makeVerbBar3 idata (cpl, ppl, ajl, avl-1, cjl)
                   ]
       | otherwise = []
  focj | cjl > 0 = [ makeVerbBar0 idata (cpl, ppl, ajl, avl, cjl-1)
                   ]
       | otherwise = []
  focc | cjl > 0 && cpl > 0 = [ makeVerbBar8 idata (cpl-1, ppl, ajl, avl, cjl-1)
                              ]
       | otherwise = []
  fovc | cjl > 0 && avl > 0 = [ makeVerbBar2 idata (cpl, ppl, ajl, avl-1, cjl-1)
                              , makeVerbBar4 idata (cpl, ppl, ajl, avl-1, cjl-1)
                              ]
       | otherwise = []
  fopc | cjl > 0 && ppl > 0 = [ makeVerbBar6 idata (cpl, ppl-1, ajl, avl, cjl-1)
                              ]
       | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits

  pickfrom = fooc ++ foop ++ foov ++ focj ++ focc ++ fovc ++ fopc ++ [makeVerbBar9 idata limits]

--AdvP + VerbBar
makeVerbBar1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar1 idata limits =
  VerbBar1 <$> makeAdvP idata limits <*> makeVerbBar idata limits

--ConjP AdvP + VerbBar
makeVerbBar2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar2 idata limits =
  VerbBar2 <$> makeConjP2 idata limits <*> makeVerbBar idata limits

--VerbBar + AdvP
makeVerbBar3 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar3 idata limits =
  VerbBar3 <$> makeVerbBar idata limits <*> makeAdvP idata limits

--VerbBar + ConjP AdvP
makeVerbBar4 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar4 idata limits =
  VerbBar4 <$> makeVerbBar idata limits <*> makeConjP2 idata limits

--VerbBar + PrepP
makeVerbBar5 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar5 idata limits =
  VerbBar5 <$> makeVerbBar idata limits <*> makePrepP idata limits

--VerbBar + ConjP PrepP
makeVerbBar6 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar6 idata limits =
  VerbBar6 <$> makeVerbBar idata limits <*> makeConjP5 idata limits

--Verb + CompP
makeVerbBar7 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar7 idata limits =
  VerbBar7 <$> makeVerb idata <*> makeCompP idata limits

--Verb + ConjP CompP
makeVerbBar8 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar8 idata limits =
  VerbBar8 <$> makeVerb idata <*> makeConjP3 idata limits

--Verb + DetP
makeVerbBar9 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar9 idata limits =
  VerbBar9 <$> makeVerb idata <*> makeDetP idata limits

--Verb + ConjP DetP
makeVerbBar0 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbBar
makeVerbBar0 idata limits =
  VerbBar0 <$> makeVerb idata <*> makeConjP1 idata limits

--AdjBar main
makeAdjBar :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdjBar
makeAdjBar idata limits = join $ choice pickfrom where
  foop | ppl > 0 = [ makeAdjBar3 idata (cpl, ppl-1, ajl, avl, cjl)
                   , makeAdjBar41 idata (cpl, ppl-1, ajl, avl, cjl)
                   ]
       | otherwise = []
  fooj | ajl > 0 = [ makeAdjBar1 idata (cpl, ppl, ajl-1, avl, cjl)
                   ]
       | otherwise = []
  focj | cjl > 0 = [ makeAdjBar2 idata (cpl, ppl, ajl, avl, cjl-1)
                   ]
       | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickfrom = foop ++ fooj ++ focj ++ [makeAdjBar42 idata limits]

--AdjP + AdjBar
makeAdjBar1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdjBar
makeAdjBar1 idata limits =
  AdjBar1 <$> makeAdjP idata limits <*> makeAdjBar idata limits

--ConjP AdjP + AdjBar
makeAdjBar2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdjBar
makeAdjBar2 idata limits =
  AdjBar2 <$> makeConjP4 idata limits <*> makeAdjBar idata limits

--AdjBar PrepP
makeAdjBar3 :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdjBar
makeAdjBar3 idata limits =
  AdjBar3 <$> makeAdjBar idata limits <*> makePrepP idata limits

--Adj with the optional PrepP
makeAdjBar41 :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdjBar
makeAdjBar41 idata limits =
  AdjBar4 <$> makeAdj idata <*> (YesOpt <$> makePrepP idata limits)

--Adj without the optional PrepP
makeAdjBar42 :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdjBar
makeAdjBar42 idata limits = do
  foo <- makeAdj idata
  return $ AdjBar4 foo NoOpt

--AdvBar
makeAdvBar :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdvBar
makeAdvBar idata limits =
  AdvBar <$> makeAdv idata

--PrepBar main
makePrepBar :: InputData -> (Int,Int,Int,Int,Int) -> RVar PrepBar
makePrepBar idata limits = join $ choice pickfrom where
  foop | ppl > 0 = [ makePrepBar1 idata (cpl, ppl-1, ajl, avl, cjl)
                   ]
       | otherwise = []
  focj | cjl > 0 = [ makePrepBar3 idata (cpl, ppl, ajl, avl, cjl-1)
                   ]
       | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickfrom = foop ++ focj ++ [makePrepBar2 idata limits]

--PrepBar + with the optional PrepP
makePrepBar1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar PrepBar
makePrepBar1 idata limits =
  PrepBar1 <$> makePrepBar idata limits <*> makePrepP idata limits

--Prep + DetP
makePrepBar2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar PrepBar
makePrepBar2 idata limits =
  PrepBar2 <$> makePrep idata <*> makeDetP idata limits

--Prep + ConjP DetP
makePrepBar3 :: InputData -> (Int,Int,Int,Int,Int) -> RVar PrepBar
makePrepBar3 idata limits =
  PrepBar3 <$> makePrep idata <*> makeConjP1 idata limits

--DetBar main
makeDetBar :: InputData -> (Int,Int,Int,Int,Int) -> RVar DetBar
makeDetBar idata limits = join $ choice pickfrom where
  focj | cjl > 0 = [ makeDetBar2 idata (cpl, ppl, ajl, avl, cjl-1)
                   ]
       | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickfrom = focj ++ [makeDetBar1 idata limits]

--Det + NounP
makeDetBar1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar DetBar
makeDetBar1 idata limits =
  DetBar1 <$> makeDet idata <*> makeNounP idata limits

--Det + ConjP NounP
makeDetBar2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar DetBar
makeDetBar2 idata limits =
  DetBar2 <$> makeDet idata <*> makeConjP6 idata limits

--CompBar
makeCompBar :: InputData -> (Int,Int,Int,Int,Int) -> RVar CompBar
makeCompBar idata limits =
  CompBar <$> makeComp idata <*> makeTenseP idata limits


makeTenseBar :: InputData -> (Int,Int,Int,Int,Int) -> RVar TenseBar
makeTenseBar idata limits = join $ choice pickFrom where
  foov | cjl > 0 = [ makeTenseBar2 idata (cpl, ppl, ajl, avl, cjl-1)
                   ]
       | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickFrom = foov ++ [makeTenseBar1 idata limits]

--TenseBar with Tense and VerbP
makeTenseBar1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar TenseBar
makeTenseBar1 idata limits =
  TenseBar1 <$> makeTense idata <*> makeVerbP idata limits

--TenseBar with Conj VerbP
makeTenseBar2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar TenseBar
makeTenseBar2 idata limits =
  TenseBar2 <$> makeTense idata <*> makeConjP7 idata limits

--ConjBar for DetP
makeConjBar1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjBar
makeConjBar1 idata limits =
  ConjBar1 <$> makeConjCum idata <*> makeDetP idata limits

--ConjBar for AdvP
makeConjBar2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjBar
makeConjBar2 idata limits =
  ConjBar2 <$> makeConjCum idata <*> makeAdvP idata limits

--ConjBar for CompP
makeConjBar3 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjBar
makeConjBar3 idata limits =
  ConjBar3 <$> makeConjCum idata <*> makeCompP idata limits

--ConjBar for AdjP
makeConjBar4 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjBar
makeConjBar4 idata limits =
  ConjBar4 <$> makeConjCum idata <*> makeAdjP idata limits

--ConjBar for PrepP
makeConjBar5 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjBar
makeConjBar5 idata limits =
  ConjBar5 <$> makeConjCum idata <*> makePrepP idata limits

--ConjBar for NounP
makeConjBar6 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjBar
makeConjBar6 idata limits =
  ConjBar6 <$> makeConjCum idata <*> makeNounP idata limits

--ConjBar for VerbP
makeConjBar7 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjBar
makeConjBar7 idata limits =
  ConjBar7 <$> makeConjCum idata <*> makeVerbP idata limits

--ConjBar for TenseP
makeConjBar8 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjBar
makeConjBar8 idata limits = ConjBar8 <$> pickFrom <*> makeTenseP idata limits where
  pickFrom = join $ choice [makeConjCum idata, makeConjSub idata]

--NegBar
makeNegBar :: InputData -> (Int,Int,Int,Int,Int) -> RVar NegBar
makeNegBar idata limits =
  NegBar <$> makeNeg idata


----------------------------Make XP---------------------------------------
--NounP
makeNounP :: InputData -> (Int,Int,Int,Int,Int) -> RVar NounP
makeNounP idata limits =
  NounP <$> makeNounBar idata limits

--VerbP
makeVerbP :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbP
makeVerbP idata limits =
  join $ choice [makeVerbP1 idata limits, makeVerbP2 idata limits]

--VerbP with negation phrase
makeVerbP1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbP
makeVerbP1 idata limits =
  VerbP <$> (YesOpt <$> makeNegP idata limits) <*> makeVerbBar idata limits

--VerbP without negation phrase
makeVerbP2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar VerbP
makeVerbP2 idata limits =
  VerbP NoOpt <$> makeVerbBar idata limits

--AdjP
makeAdjP :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdjP
makeAdjP idata limits = join $ choice pickfrom where
  foov avl | avl > 0 = [ makeAdjP1 idata (cpl, ppl, ajl, avl-1, cjl)
                       ]
          | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickfrom = foov avl ++ [makeAdjP2 idata limits]

makeAdjP1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdjP
makeAdjP1 idata limits =
  AdjP <$> (YesOpt <$> makeAdvP idata limits) <*> makeAdjBar idata limits

makeAdjP2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdjP
makeAdjP2 idata limits =
  AdjP NoOpt <$> makeAdjBar idata limits

--AdvP main
makeAdvP :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdvP
makeAdvP idata limits = join $ choice pickfrom where
  foov avl | avl > 0 = [ makeAdvP1 idata (cpl, ppl, ajl, avl-1, cjl)
              ]
          | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickfrom = foov avl ++ [makeAdvP2 idata limits]

--AdvBar with optional AdvP
makeAdvP1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdvP
makeAdvP1 idata limits =
  AdvP <$> (YesOpt <$> makeAdvP idata limits) <*> makeAdvBar idata limits

--AdvBar without optional AdvP
makeAdvP2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar AdvP
makeAdvP2 idata limits =
  AdvP NoOpt <$> makeAdvBar idata limits

--PrepP main
makePrepP :: InputData -> (Int,Int,Int,Int,Int) -> RVar PrepP
makePrepP idata limits = join $ choice pickfrom where
  fooj avl | avl > 0 = [ makePrepP1 idata (cpl, ppl, ajl, avl-1, cjl)
                      ]
          | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickfrom = fooj ajl ++ [makePrepP2 idata limits]

--with optional AdjP
makePrepP1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar PrepP
makePrepP1 idata limits =
  PrepP <$> (YesOpt <$> makeAdvP idata limits) <*> makePrepBar idata limits

--without optional AdjP
makePrepP2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar PrepP
makePrepP2 idata limits =
  PrepP NoOpt <$> makePrepBar idata limits

--DetP
makeDetP :: InputData -> (Int,Int,Int,Int,Int) -> RVar DetP
makeDetP idata limits =
  DetP <$> makeDetBar idata limits

--CompP
makeCompP :: InputData -> (Int,Int,Int,Int,Int) -> RVar CompP
makeCompP idata limits =
  CompP <$> makeCompBar idata limits

--TenseP main
makeTenseP :: InputData -> (Int,Int,Int,Int,Int) -> RVar TenseP
makeTenseP idata limits = join $ choice pickFrom where
  focj | cjl > 0 = [ makeTenseP2 idata (cpl, ppl, ajl, avl, cjl-1)
                      ]
          | otherwise = []
  (cpl, ppl, ajl, avl, cjl) = limits
  pickFrom = focj ++ [makeTenseP1 idata limits]


makeTenseP1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar TenseP
makeTenseP1 idata limits =
  TenseP1 <$> makeDetP idata limits <*> makeTenseBar idata limits

makeTenseP2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar TenseP
makeTenseP2 idata limits =
  TenseP2 <$> makeConjP1 idata limits <*> makeTenseBar idata limits

--ConjP for DetP
makeConjP1 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjP
makeConjP1 idata limits =
  ConjP1 <$> makeDetP idata limits <*> makeConjBar1 idata limits

--ConjP for AdvP
makeConjP2 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjP
makeConjP2 idata limits =
  ConjP2 <$> makeAdvP idata limits <*> makeConjBar2 idata limits

--ConjP for CompP
makeConjP3 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjP
makeConjP3 idata limits =
  ConjP3 <$> makeCompP idata limits <*> makeConjBar3 idata limits

--ConjP for AdjP
makeConjP4 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjP
makeConjP4 idata limits =
  ConjP4 <$> makeAdjP idata limits <*> makeConjBar4 idata limits

--ConjP for PrepP
makeConjP5 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjP
makeConjP5 idata limits =
  ConjP5 <$> makePrepP idata limits <*> makeConjBar5 idata limits

--ConjP for NounP
makeConjP6 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjP
makeConjP6 idata limits =
  ConjP6 <$> makeNounP idata limits <*> makeConjBar6 idata limits

--ConjP for VerbP
makeConjP7 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjP
makeConjP7 idata limits =
  ConjP7 <$> makeVerbP idata limits <*> makeConjBar7 idata limits

--ConjP for TenseP
makeConjP8 :: InputData -> (Int,Int,Int,Int,Int) -> RVar ConjP
makeConjP8 idata limits =
  ConjP8 <$> makeTenseP idata limits <*> makeConjBar8 idata limits

--NegP
makeNegP :: InputData -> (Int,Int,Int,Int,Int) -> RVar NegP
makeNegP idata limits =
  NegP <$> makeNegBar idata limits
