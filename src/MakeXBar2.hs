module MakeXBar2
(
) where

import           Control.Monad
import           Prelude
import           XBarType2
import           Data.RVar
import           Data.Random.Extras hiding (shuffle)
import           Data.Random hiding (sample)

-- Load data
data InputData = InputData
    {
      iNoun    :: [String]
    , iVerb    :: [String]
    , iAdj     :: [String]
    , iAdv     :: [String]
    , iPrep    :: [String]
    , iDet     :: [String]
    , iComp    :: [String]
    , iInfl    :: [String]
    }

loadInputData :: IO InputData
loadInputData  =
    InputData
        <$> readFeature "raw/nouns/nouns.txt"
        <*> readFeature "raw/verbs/transitive.txt"
        <*> readFeature "raw/adjectives/size.txt"
        <*> readFeature "raw/adverbs/generic.txt"
        <*> readFeature "raw/prepositions/generic.txt"
        <*> readFeature "raw/determiners/articles.txt"
        <*> readFeature "raw/conjunctions/complementizers.txt"
        <*> readFeature "raw/inflections/tenses.txt"

readFeature :: Read a => FilePath -> IO a
readFeature = fmap read . readFile

-- Main
main :: IO ()
main = do
  dat <- loadInputData
  struct <- sampleRVar (makeXP dat Comp)
  writeFile "output.txt" $ show struct


-- Given a XP, what specifiers are allowed?
allowedSpec :: LexCat -> [LexCat]
allowedSpec Null = [Null]
allowedSpec Comp = [Null]
allowedSpec Infl = [Null]
allowedSpec Verb = [Det]
allowedSpec Det  = [Null]
allowedSpec Noun = [Null]
allowedSpec Prep = [Null]
allowedSpec Adj  = [Null]
allowedSpec Adv  = [Null, Adv]
allowedSpec Neg  = [Verb]
allowedSpec Quan = [Det]
allowedSpec Agr  = [Null]

-- Given a XBar, what adjuncts are allowed?
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

-- Given a XBar, what complements are allowed?
allowedComp :: LexCat -> [LexCat]
allowedComp Null = [Null]
allowedComp Comp = [Infl]
allowedComp Infl = [Verb]
allowedComp Verb = [Null, Comp, Det]
allowedComp Det  = [Noun]
allowedComp Noun = [Null, Prep]
allowedComp Prep = [Det]
allowedComp Adj  = [Null, Prep]
allowedComp Adv  = [Null]
allowedComp Neg  = [Null]
allowedComp Quan = [Null]
allowedComp Agr  = [Null] -- ??

-- Make the XBar tree
makeXP :: InputData -> LexCat -> RVar (Phrase LexCat)
makeXP idata cat = do
  newCat <- choice $ allowedSpec cat
  join $ choice [XP1 <$> makeXP idata newCat <*> makeXBar idata cat
                ]

makeXBar :: InputData -> LexCat -> RVar (Bar LexCat)
makeXBar idata cat = do
  newCat1 <- choice $ allowedAdjunt cat
  newCat2 <- choice $ allowedComp cat
  join $ choice [XBar2 <$> makeXBar idata cat <*> makeXP idata newCat1
                , XBar4 <$> makeX idata cat <*> makeXP idata newCat2
                ]

makeX :: InputData -> LexCat -> RVar (Head LexCat)
makeX idata Null = Head <$> return "" -- should never get to this, probably
makeX idata Comp = Head <$> choice (iComp idata)
makeX idata Infl = Head <$> choice (iInfl idata)
makeX idata Verb = Head <$> choice (iVerb idata)
makeX idata Det  = Head <$> choice (iDet  idata)
makeX idata Noun = Head <$> choice (iNoun idata)
makeX idata Prep = Head <$> choice (iPrep idata)
makeX idata Adj  = Head <$> choice (iAdj  idata)
makeX idata Adv  = Head <$> choice (iAdv  idata)
makeX idata Neg  = Head <$> return "not" --temp
makeX idata Quan = Head <$> return "both" --temp



-- stuff for looking back
{-
makeXP1 :: InputData -> LexCat -> PhraseR LexCat -> RVar (Phrase LexCat)
makeXP1 idata cat prev = join $ choice [ XP1 <$> makeXP1 idata newCat (XPR3 (XBarR0 cat) prev) <*> makeXBar1 idata cat (XPR1 prev (XPR0 newCat))
                                      ] where newCat = Verb

makeXP2 :: InputData -> LexCat -> BarR LexCat -> RVar (Phrase LexCat)
makeXP2 idata cat prev = join $ choice [ XP1 <$> makeXP1 idata newCat (XPR6 (XBarR0 cat) prev) <*> makeXBar1 idata cat (XPR7 prev (XPR0 newCat))
                                      ] where newCat = Verb

makeXBar1 :: InputData -> LexCat -> PhraseR LexCat -> RVar (Bar LexCat)
makeXBar1 idata cat prev = join $ choice [ XBar2 <$> makeXBar2 idata cat (XBarR7 (XPR0 newCat) prev) <*> makeXP2 idata newCat (XBarR3 prev (XBarR0 cat))
                                         , XBar4 <$> makeX idata cat (XBarR7 (XPR0 newCat) prev) <*> makeXP2 idata newCat (XBarR11 prev (HeadR0 cat))
                                         ] where newCat = Verb

makeXBar2 :: InputData -> LexCat -> BarR LexCat -> RVar (Bar LexCat)
makeXBar2 idata cat prev = join $ choice [ XBar2 <$> makeXBar2 idata cat (XBarR5 (XPR0 newCat) prev) <*> makeXP2 idata newCat (XBarR2 prev (XBarR0 cat))
                                        , XBar4 <$> makeX idata cat (XBarR5 (XPR0 newCat) prev) <*> makeXP2 idata newCat (XBarR9 prev (HeadR0 cat))
                                        ] where newCat = Verb
-}
