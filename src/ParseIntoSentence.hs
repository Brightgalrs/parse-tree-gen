module ParseIntoSentence
( parseTenseP
) where

import           Control.Monad
import           Data.Random.Extras hiding (shuffle)
import           Data.RVar
import           Prelude
import           XBarType
import           MakeXBarXP

--Parse the tree and output full string
-------------------------------------parse XP-----------------------------------
parseNegP :: NegP -> String
parseNegP negp = str where
  NegP negbar = negp
  str = parseNegBar negbar

parseConjP :: ConjP -> String
parseConjP conjp = str where
  ConjP detp conjbar = conjp
  str = parseDetP detp ++ " " ++ parseConjBar conjbar

parseTenseP :: TenseP -> String
parseTenseP (TenseP1 detp tensebar) = str where
  str = parseDetP detp ++ " " ++ parseTenseBar tensebar
parseTenseP (TenseP2 conjp tensebar) = str where
  str = parseConjP conjp ++ " " ++ parseTenseBar tensebar

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
  PrepP optadvp prepbar = prepp
  fooBar (YesOpt advp) = parseAdvP advp ++ " " ++ parsePrepBar prepbar
  fooBar NoOpt = parsePrepBar prepbar
  str = fooBar optadvp

parseAdvP :: AdvP -> String
parseAdvP advp = str where
  AdvP optadvp advbar = advp
  fooBar (YesOpt advp) = parseAdvP advp ++ " " ++ parseAdvBar advbar
  fooBar NoOpt = parseAdvBar advbar
  str = fooBar optadvp

parseAdjP :: AdjP -> String
parseAdjP adjp = str where
  AdjP optadvp adjbar = adjp
  fooBar (YesOpt advp) = parseAdvP advp ++ " " ++ parseAdjBar adjbar
  fooBar NoOpt = parseAdjBar adjbar
  str = fooBar optadvp

parseVerbP :: VerbP -> Tense -> String
parseVerbP verbp tense = str where
  VerbP optneg verbbar = verbp
  fooBar (YesOpt negp) = parseNegP negp ++ " " ++ parseVerbBar verbbar tense
  fooBar NoOpt = parseVerbBar verbbar tense
  str = fooBar optneg

parseNounP :: NounP -> String
parseNounP nounp = str where
  NounP nounbar = nounp
  str = parseNounBar nounbar

----------------------------------parse Xbar------------------------------------

parseNegBar :: NegBar -> String
parseNegBar negbar = str where
  NegBar neg = negbar
  str = parseNeg neg

parseConjBar :: ConjBar -> String
parseConjBar conjbar = str where
  ConjBar conj detp = conjbar
  str = parseConj conj ++ " " ++ parseDetP detp

parseTenseBar :: TenseBar -> String
parseTenseBar tensebar = str where
  TenseBar tense verbp = tensebar
  str = parseTenseAux tense ++ " " ++ parseVerbP verbp tense

parseCompBar :: CompBar -> String
parseCompBar compbar = str where
  CompBar comp tensep = compbar
  str = parseComp comp ++ " " ++ parseTenseP tensep

parseDetBar :: DetBar -> String
parseDetBar detbar = str where
  DetBar1 optdet nounp = detbar
  fooBar (YesOpt det) = parseDet det ++ " " ++ parseNounP nounp
  fooBar NoOpt = parseNounP nounp
  str = fooBar optdet

parsePrepBar :: PrepBar -> String
parsePrepBar (PrepBar1 prepbar prepp) = str where
  str = parsePrepBar prepbar ++ " " ++ parsePrepP prepp
parsePrepBar (PrepBar2 prep detp) = str where
  str = parsePrep prep ++ " " ++ parseDetP detp

parseAdvBar :: AdvBar -> String
parseAdvBar advbar = str where
  AdvBar adv = advbar
  str = parseAdv adv

parseAdjBar :: AdjBar -> String
parseAdjBar (AdjBar1 adjp adjbar) = str where
  str = parseAdjP adjp ++ " " ++ parseAdjBar adjbar
parseAdjBar (AdjBar2 adjbar prepp) = str where
  str = parseAdjBar adjbar ++ " " ++ parsePrepP prepp
parseAdjBar (AdjBar3 adj optprepp) = str where
  fooBar (YesOpt prepp) = parseAdj adj ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parseAdj adj
  str = fooBar optprepp

parseVerbBar :: VerbBar -> Tense -> String
parseVerbBar (VerbBar1 advp verbbar) tense = str where
  str = parseAdvP advp ++ " " ++ parseVerbBar verbbar tense
parseVerbBar (VerbBar2 verbbar prepp) tense = str where
  str = parseVerbBar verbbar tense ++ " " ++ parsePrepP prepp
parseVerbBar (VerbBar3 verbbar advp) tense = str where
  str = parseVerbBar verbbar tense ++ " " ++ parseAdvP advp
parseVerbBar (VerbBar4 verb compp) tense = str where
  str = (parseVerb verb) ++ (parseTenseMorph tense) ++ " " ++ parseCompP compp
parseVerbBar (VerbBar5 verb detp) tense = str where
  str = (parseVerb verb) ++ (parseTenseMorph tense) ++ " " ++ parseDetP detp

parseNounBar :: NounBar -> String
parseNounBar (NounBar1 adjp nounbar) = str where
  str = parseAdjP adjp ++ " " ++ parseNounBar nounbar
parseNounBar (NounBar2 nounbar prepp) = str where
  str = parseNounBar nounbar ++ " " ++ parsePrepP prepp
parseNounBar (NounBar3 noun optprepp) = str where
  fooBar (YesOpt prepp) = parseNoun noun ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parseNoun noun
  str = fooBar optprepp

-----------------------------------parse X--------------------------------------
parseNeg :: Neg -> String
parseNeg (Neg str) = str

parseConj :: Conj -> String
parseConj (Conj str) = str

parseTenseAux :: Tense -> String
parseTenseAux (Tense aux morph) = aux

parseTenseMorph :: Tense -> String
parseTenseMorph (Tense aux morph) = morph

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
