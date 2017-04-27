{-# LANGUAGE ScopedTypeVariables #-}
module MakeX where

import           Control.Monad
import           Data.Random.Extras hiding (shuffle)
import           Data.Random hiding (sample)
import           Data.RVar
import           LoadData
import           Prelude
import           XBarType

--Make X

makeNoun :: InputData -> RVar Noun
makeNoun idata =
  choice (iNoun idata)

makeVerb :: InputData -> RVar Verb
makeVerb idata =
  choice (iVerb idata)

makeAdj :: InputData -> RVar Adj
makeAdj idata =
  choice (iAdj idata)

makeAdv :: InputData -> RVar Adv
makeAdv idata =
  choice (iAdv idata)

makePrep :: InputData -> RVar Prep
makePrep idata =
  choice (iPrep idata)

makeDet :: InputData -> RVar Det
makeDet idata =
  choice (iDet idata)

makeComp :: InputData -> RVar Comp
makeComp idata =
  choice (iComp idata)

makeTense :: InputData -> RVar Tense
makeTense idata =
  choice (iTense idata)

makeConjCum :: InputData -> RVar Conj
makeConjCum idata =
  choice (iConjCum idata)

makeConjSub :: InputData -> RVar Conj
makeConjSub idata =
  choice (iConjSub idata)

makeNeg :: InputData -> RVar Neg
makeNeg idata = return $ Neg "not"

makePron :: InputData -> RVar Pron
makePron idata =
  choice (iPron idata)
