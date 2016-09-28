module ParseIntoSentence
( parseSentence
) where

import           Control.Monad
import           Data.Random.Extras hiding (shuffle)
import           Data.RVar
import           Prelude
import           XBarType
import           MakeXBarXP

--Parse the tree and output full string
parseSentence :: Sentence -> String
parseSentence (Sentence1 tensep) = str where
  str = parseTenseP tensep
parseSentence (Sentence2 conjp) = str where
  str = parseConjP8 conjp

-------------------------------------parse XP-----------------------------------
parseNegP :: NegP -> String
parseNegP negp = str where
  NegP negbar = negp
  str = parseNegBar negbar

parseConjP1 :: ConjP -> String
parseConjP1 (ConjP1 detp conjbar) = str where
  str = parseDetP detp ++ " " ++ parseConjBar1 conjbar

parseConjP2 :: ConjP -> String
parseConjP2 (ConjP2 advp conjbar) = str where
  str = parseAdvP advp ++ " " ++ parseConjBar2 conjbar

parseConjP3 :: ConjP -> String
parseConjP3 (ConjP3 compp conjbar) = str where
  str = parseCompP compp ++ " " ++ parseConjBar3 conjbar

parseConjP4 :: ConjP -> String
parseConjP4 (ConjP4 adjp conjbar) = str where
  str = parseAdjP adjp ++ " " ++ parseConjBar4 conjbar

parseConjP5 :: ConjP -> String
parseConjP5 (ConjP5 prepp conjbar) = str where
  str = parsePrepP prepp ++ " " ++ parseConjBar5 conjbar

parseConjP6 :: ConjP -> String
parseConjP6 (ConjP6 nounp conjbar) = str where
  str = parseNounP nounp ++ " " ++ parseConjBar6 conjbar

parseConjP7 :: ConjP -> Tense -> String
parseConjP7 (ConjP7 verbp conjbar) tense = str where
  str = parseVerbP verbp tense ++ " " ++ parseConjBar7 conjbar tense

parseConjP8 :: ConjP -> String
parseConjP8 (ConjP8 tensep conjbar) = str where
  str = parseTenseP tensep ++ " " ++ parseConjBar8 conjbar

parseTenseP :: TenseP -> String
parseTenseP (TenseP1 detp tensebar) = str where
  str = parseDetP detp ++ " " ++ parseTenseBar tensebar
parseTenseP (TenseP2 conjp tensebar) = str where
  str = parseConjP1 conjp ++ " " ++ parseTenseBar tensebar

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

parseConjBar1 :: ConjBar -> String
parseConjBar1 (ConjBar1 conj detp) = str where
  str = parseConj conj ++ " " ++ parseDetP detp

parseConjBar2 :: ConjBar -> String
parseConjBar2 (ConjBar2 conj advp) = str where
  str = parseConj conj ++ " " ++ parseAdvP advp

parseConjBar3 :: ConjBar -> String
parseConjBar3 (ConjBar3 conj compp) = str where
  str = parseConj conj ++ " " ++ parseCompP compp

parseConjBar4 :: ConjBar -> String
parseConjBar4 (ConjBar4 conj adjp) = str where
  str = parseConj conj ++ " " ++ parseAdjP adjp

parseConjBar5 :: ConjBar -> String
parseConjBar5 (ConjBar5 conj prepp) = str where
  str = parseConj conj ++ " " ++ parsePrepP prepp

parseConjBar6 :: ConjBar -> String
parseConjBar6 (ConjBar6 conj nounp) = str where
  str = parseConj conj ++ " " ++ parseNounP nounp

parseConjBar7 :: ConjBar -> Tense -> String
parseConjBar7 (ConjBar7 conj verbp) tense = str where
  str = parseConj conj ++ " " ++ parseVerbP verbp tense

parseConjBar8 :: ConjBar -> String
parseConjBar8 (ConjBar8 conj tensep) = str where
  str = parseConj conj ++ " " ++ parseTenseP tensep

parseTenseBar :: TenseBar -> String
parseTenseBar (TenseBar1 tense verbp) = str where
  str = parseTenseAux tense ++ " " ++ parseVerbP verbp tense
parseTenseBar (TenseBar2 tense conjp) = str where
  str = parseTenseAux tense ++ " " ++ parseConjP7 conjp tense

parseCompBar :: CompBar -> String
parseCompBar compbar = str where
  CompBar comp tensep = compbar
  str = parseComp comp ++ " " ++ parseTenseP tensep

parseDetBar :: DetBar -> String
parseDetBar (DetBar1 det nounp) = str where
   str = parseDet det ++ " " ++ parseNounP nounp
parseDetBar (DetBar2 det conjp) = str where
   str = parseDet det ++ " " ++ parseConjP6 conjp

parsePrepBar :: PrepBar -> String
parsePrepBar (PrepBar1 prepbar prepp) = str where
  str = parsePrepBar prepbar ++ " " ++ parsePrepP prepp
parsePrepBar (PrepBar2 prep detp) = str where
  str = parsePrep prep ++ " " ++ parseDetP detp
parsePrepBar (PrepBar3 prep conjp) = str where
  str = parsePrep prep ++ " " ++ parseConjP1 conjp

parseAdvBar :: AdvBar -> String
parseAdvBar advbar = str where
  AdvBar adv = advbar
  str = parseAdv adv

parseAdjBar :: AdjBar -> String
parseAdjBar (AdjBar1 adjp adjbar) = str where
  str = parseAdjP adjp ++ " " ++ parseAdjBar adjbar
parseAdjBar (AdjBar2 conjp adjbar) = str where
  str = parseConjP4 conjp ++ " " ++ parseAdjBar adjbar
parseAdjBar (AdjBar3 adjbar prepp) = str where
  str = parseAdjBar adjbar ++ " " ++ parsePrepP prepp
parseAdjBar (AdjBar4 adj optprepp) = str where
  fooBar (YesOpt prepp) = parseAdj adj ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parseAdj adj
  str = fooBar optprepp

parseVerbBar :: VerbBar -> Tense -> String
parseVerbBar (VerbBar1 advp verbbar) tense = str where
  str = parseAdvP advp ++ " " ++ parseVerbBar verbbar tense
parseVerbBar (VerbBar2 conjp verbbar) tense = str where
  str = parseConjP2 conjp ++ " " ++ parseVerbBar verbbar tense
parseVerbBar (VerbBar3 verbbar advp) tense = str where
  str = parseVerbBar verbbar tense ++ " " ++ parseAdvP advp
parseVerbBar (VerbBar4 verbbar conjp) tense = str where
  str = parseVerbBar verbbar tense ++ " " ++ parseConjP2 conjp
parseVerbBar (VerbBar5 verbbar prepp) tense = str where
  str = parseVerbBar verbbar tense ++ " " ++ parsePrepP prepp
parseVerbBar (VerbBar6 verbbar conjp) tense = str where
  str = parseVerbBar verbbar tense ++ " " ++ parseConjP5 conjp
parseVerbBar (VerbBar7 verb compp) tense = str where
  str = parseVerb verb ++ parseTenseMorph tense ++ " " ++ parseCompP compp
parseVerbBar (VerbBar8 verb conjp) tense = str where
  str = parseVerb verb ++ parseTenseMorph tense ++ " " ++ parseConjP3 conjp
parseVerbBar (VerbBar9 verb detp) tense = str where
  str = parseVerb verb ++ parseTenseMorph tense ++ " " ++ parseDetP detp
parseVerbBar (VerbBar0 verb conjp) tense = str where
  str = parseVerb verb ++ parseTenseMorph tense ++ " " ++ parseConjP1 conjp

parseNounBar :: NounBar -> String
parseNounBar (NounBar1 adjp nounbar) = str where
  str = parseAdjP adjp ++ " " ++ parseNounBar nounbar
parseNounBar (NounBar2 conjp nounbar) = str where
  str = parseConjP4 conjp ++ " " ++ parseNounBar nounbar
parseNounBar (NounBar3 nounbar prepp) = str where
  str = parseNounBar nounbar ++ " " ++ parsePrepP prepp
parseNounBar (NounBar4 noun prepp) = str where
  str = parseNoun noun ++ " " ++ parsePrepP prepp
parseNounBar (NounBar5 noun conjp) = str where
  str = parseNoun noun ++ " " ++ parseConjP5 conjp
parseNounBar (NounBar6 noun) = str where
  str = parseNoun noun

-----------------------------------parse X--------------------------------------
parseNeg :: Neg -> String
parseNeg (Neg str) = str

parseConj :: Conj -> String
parseConj (Conj1 str) = str
parseConj (Conj2 str) = str

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
