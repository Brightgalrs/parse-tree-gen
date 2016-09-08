module ParseTreeParseIntoSentence
( makeTenseP
, parseTenseP
) where

import           Control.Monad
import           Data.Random.Extras hiding (shuffle)
import           Data.RVar
import           Prelude
import           XBarType
import           ParseTreeGen

--Parse the tree and output full string
parseTenseP :: TenseP -> String
parseTenseP tensep = str where
  TenseP detp tensebar = tensep
  str = parseDetP detp ++ " " ++ parseTenseBar tensebar

parseCompP :: CompP -> String
parseCompP compp = str where
  CompP compbar = compp
  str = parseCompBar compbar

parseDetP :: DetP -> String
parseDetP detp = str where
  DetP detbar = detp
  str = parseDetBar detbar

parsePrepP :: PrepP -> String
parsePrepP prepp = str where
  PrepP optadjp prepbar = prepp
  fooBar (YesOpt adjp) = parseAdjP adjp ++ " " ++ parsePrepBar prepbar
  fooBar NoOpt = parsePrepBar prepbar
  str = fooBar optadjp

parseAdvP :: AdvP -> String
parseAdvP advp = str where
  AdvP optadvp advbar = advp
  fooBar (YesOpt advp) = parseAdvP advp ++ " " ++ parseAdvBar advbar
  fooBar NoOpt = parseAdvBar advbar
  str = fooBar optadvp

parseAdjP :: AdjP -> String
parseAdjP adjp = str where
  AdjP adjbar = adjp
  str = parseAdjBar adjbar

parseVerbP :: VerbP -> String
parseVerbP verbp = str where
  VerbP verbbar = verbp
  str = parseVerbBar verbbar

parseNounP :: NounP -> String
parseNounP nounp = str where
  NounP nounbar = nounp
  str = parseNounBar nounbar

parseTenseBar :: TenseBar -> String
parseTenseBar tensebar = str where
  TenseBar tense verbp = tensebar
  str = parseTense tense ++ " " ++ parseVerbP verbp

parseCompBar :: CompBar -> String
parseCompBar compbar = str where
  CompBar comp tensep = compbar
  str = parseComp comp ++ " " ++ parseTenseP tensep

parseDetBar :: DetBar -> String
parseDetBar detbar = str where
  DetBar optdet nounp = detbar
  fooBar (YesOpt det) = parseDet det ++ " " ++ parseNounP nounp
  fooBar NoOpt = parseNounP nounp
  str = fooBar optdet

parsePrepBar :: PrepBar -> String
parsePrepBar (PrepBar1 prepbar optprepp) = str where
  fooBar (YesOpt prepp) = parsePrepBar prepbar ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parsePrepBar prepbar
  str = fooBar optprepp
parsePrepBar (PrepBar2 prep detp) = str where
  str = parsePrep prep ++ " " ++ parseDetP detp

parseAdvBar :: AdvBar -> String
parseAdvBar advbar = str where
  AdvBar adv = advbar
  str = parseAdv adv

parseAdjBar :: AdjBar -> String
parseAdjBar (AdjBar1 advp adjbar) = str where
  str = parseAdvP advp ++ " " ++ parseAdjBar adjbar
parseAdjBar (AdjBar2 adjp adjbar) = str where
  str = parseAdjP adjp ++ " " ++ parseAdjBar adjbar
parseAdjBar (AdjBar3 adjbar optprepp) = str where
  fooBar (YesOpt prepp) = parseAdjBar adjbar ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parseAdjBar adjbar
  str = fooBar optprepp
parseAdjBar (AdjBar4 adj optprepp) = str where
  fooBar (YesOpt prepp) = parseAdj adj ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parseAdj adj
  str = fooBar optprepp

parseVerbBar :: VerbBar -> String
parseVerbBar (VerbBar1 advp verbbar) = str where
  str = parseAdvP advp ++ " " ++ parseVerbBar verbbar
parseVerbBar (VerbBar2 verbbar prepp) = str where
  str = parseVerbBar verbbar ++ " " ++ parsePrepP prepp
parseVerbBar (VerbBar3 verbbar advp) = str where
  str = parseVerbBar verbbar ++ " " ++ parseAdvP advp
parseVerbBar (VerbBar4 verb compp) = str where
  str = parseVerb verb ++ " " ++ parseCompP compp
parseVerbBar (VerbBar5 verb detp) = str where
  str = parseVerb verb ++ " " ++ parseDetP detp

parseNounBar :: NounBar -> String
parseNounBar (NounBar1 adjp nounbar) = str where
  str = parseAdjP adjp ++ " " ++ parseNounBar nounbar
parseNounBar (NounBar2 nounbar prepp) = str where
  str = parseNounBar nounbar ++ " " ++ parsePrepP prepp
parseNounBar (NounBar3 noun optprepp) = str where
  fooBar (YesOpt prepp) = parseNoun noun ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parseNoun noun
  str = fooBar optprepp

parseTense :: Tense -> String
parseTense (Tense str) = str

parseComp :: Comp -> String
parseComp (Comp str) = str

parseDet :: Det -> String
parseDet (Det str) = str

parsePrep :: Prep -> String
parsePrep (Prep str) = str

parseAdv :: Adv -> String
parseAdv (Adv str) = str

parseAdj :: Adj -> String
parseAdj (Adj str) = str

parseVerb :: Verb -> String
parseVerb (Verb str) = str

parseNoun :: Noun -> String
parseNoun (Noun str) = str
