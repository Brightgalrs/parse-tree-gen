module LoadData where

import           Control.Monad
import           Prelude
import           XBarType2


-- Load words from files
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
    , iInfl    :: [(String,String)]
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
