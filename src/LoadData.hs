module LoadData where

import           Control.Monad
import           Prelude
import           XBarType


-- Load words from files
data InputData = InputData
    {
      iNoun    :: [Noun]
    , iVerb    :: [Verb]
    , iAdj     :: [Adj]
    , iAdv     :: [Adv]
    , iPrep    :: [Prep]
    , iDet     :: [Det]
    , iComp    :: [Comp]
    , iConj    :: [Conj]
    , iPron    :: [Pron]
    , iTense   :: [Tense]
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
        <*> readFeature "raw/conjunctions/cumulative.txt"
        <*> readFeature "raw/nouns/pronouns.txt"
        <*> readFeature "raw/inflections/tenses.txt"

readFeature :: Read a => FilePath -> IO a
readFeature = fmap read . readFile

--Make random syntax tree
