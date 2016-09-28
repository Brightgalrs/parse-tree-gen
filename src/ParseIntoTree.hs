module ParseIntoTree
( parseToTreeSentence
) where

import           Control.Monad
import           Data.Random.Extras hiding (shuffle)
import           Data.RVar
import           MakeXBarXP
import           Prelude
import           XBarType

--Parse the tree and output a tree diagram
-------------------------------------parse XP-----------------------------------

parseToTreeSentence :: Sentence -> String
parseToTreeSentence (Sentence1 tensep) = "\n" ++ "Sentence" ++ str where
  str = parseToTreeTenseP tensep 1
parseToTreeSentence (Sentence2 conjp) = "\n" ++ "Sentence" ++ str where
  str = parseToTreeConjP conjp 1

parseToTreeNegP :: NegP -> Int -> String
parseToTreeNegP (NegP negbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "NegP" ++ str where
  str = parseToTreeNegBar negbar (ntabs+1)

parseToTreeConjP :: ConjP -> Int -> String
parseToTreeConjP (ConjP1 detp conjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "ConjP" ++ str where
  str = parseToTreeDetP detp (ntabs+1) ++ parseToTreeConjBar1 conjbar (ntabs+1)
parseToTreeConjP (ConjP2 advp conjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "ConjP" ++ str where
  str = parseToTreeAdvP advp (ntabs+1) ++ parseToTreeConjBar2 conjbar (ntabs+1)
parseToTreeConjP (ConjP3 compp conjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "ConjP" ++ str where
  str = parseToTreeCompP compp (ntabs+1) ++ parseToTreeConjBar3 conjbar (ntabs+1)
parseToTreeConjP (ConjP4 adjp conjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "ConjP" ++ str where
  str = parseToTreeAdjP adjp (ntabs+1) ++ parseToTreeConjBar4 conjbar (ntabs+1)
parseToTreeConjP (ConjP5 prepp conjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "ConjP" ++ str where
  str = parseToTreePrepP prepp (ntabs+1) ++ parseToTreeConjBar5 conjbar (ntabs+1)
parseToTreeConjP (ConjP6 nounp conjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "ConjP" ++ str where
  str = parseToTreeNounP nounp (ntabs+1) ++ parseToTreeConjBar6 conjbar (ntabs+1)
parseToTreeConjP (ConjP7 verbp conjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "ConjP" ++ str where
  str = parseToTreeVerbP verbp (ntabs+1) ++ parseToTreeConjBar7 conjbar (ntabs+1)
parseToTreeConjP (ConjP8 tensep conjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "ConjP" ++ str where
  str = parseToTreeTenseP tensep (ntabs+1) ++ parseToTreeConjBar8 conjbar (ntabs+1)

parseToTreeTenseP :: TenseP -> Int -> String
parseToTreeTenseP (TenseP1 detp tensebar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "TP" ++ str where
  str = parseToTreeDetP detp (ntabs+1) ++ parseToTreeTenseBar tensebar (ntabs+1)
parseToTreeTenseP (TenseP2 conjp tensebar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "TP" ++ str where
  str = parseToTreeConjP conjp (ntabs+1) ++ parseToTreeTenseBar tensebar (ntabs+1)

parseToTreeCompP :: CompP -> Int -> String
parseToTreeCompP compp ntabs = "\n" ++ (replicate ntabs '\t') ++ "CP" ++ str where
  CompP compbar = compp
  str = parseToTreeCompBar compbar (ntabs+1)

parseToTreeDetP :: DetP -> Int -> String
parseToTreeDetP detp ntabs = "\n" ++ (replicate ntabs '\t') ++ "DP" ++ str where
  DetP detbar = detp
  str = parseToTreeDetBar detbar (ntabs+1)

parseToTreePrepP :: PrepP -> Int -> String
parseToTreePrepP prepp ntabs = "\n" ++ (replicate ntabs '\t') ++ "PP" ++ str where
  PrepP optadvp prepbar = prepp
  fooBar (YesOpt advp) = parseToTreeAdvP advp (ntabs+1) ++ parseToTreePrepBar prepbar (ntabs+1)
  fooBar NoOpt = parseToTreePrepBar prepbar (ntabs+1)
  str = fooBar optadvp

parseToTreeAdvP :: AdvP -> Int -> String
parseToTreeAdvP advp ntabs = "\n" ++ (replicate ntabs '\t') ++ "AdvP" ++ str where
  AdvP optadvp advbar = advp
  fooBar (YesOpt advp) = parseToTreeAdvP advp (ntabs+1) ++ parseToTreeAdvBar advbar (ntabs+1)
  fooBar NoOpt = parseToTreeAdvBar advbar (ntabs+1)
  str = fooBar optadvp

parseToTreeAdjP :: AdjP -> Int -> String
parseToTreeAdjP adjp ntabs = "\n" ++ (replicate ntabs '\t') ++ "AdjP" ++ str where
  AdjP optadvp adjbar = adjp
  fooBar (YesOpt advp) = parseToTreeAdvP advp (ntabs+1) ++ parseToTreeAdjBar adjbar (ntabs+1)
  fooBar NoOpt = parseToTreeAdjBar adjbar (ntabs+1)
  str = fooBar optadvp

parseToTreeVerbP :: VerbP -> Int -> String
parseToTreeVerbP verbp ntabs = "\n" ++ (replicate ntabs '\t') ++ "VP" ++ str where
  VerbP optnegp verbbar = verbp
  fooBar (YesOpt negp) = parseToTreeNegP negp (ntabs+1) ++ parseToTreeVerbBar verbbar (ntabs+1)
  fooBar NoOpt = parseToTreeVerbBar verbbar (ntabs+1)
  str = fooBar optnegp

parseToTreeNounP :: NounP -> Int -> String
parseToTreeNounP nounp ntabs = "\n" ++ (replicate ntabs '\t') ++ "NP" ++ str where
  NounP nounbar = nounp
  str = parseToTreeNounBar nounbar (ntabs+1)

---------------------------------parse Xbar-------------------------------------

parseToTreeNegBar :: NegBar -> Int -> String
parseToTreeNegBar negbar ntabs = "\n" ++ (replicate ntabs '\t') ++ "Neg'" ++ str where
  NegBar neg = negbar
  str = parseToTreeNeg neg (ntabs+1)

parseToTreeConjBar1 :: ConjBar -> Int -> String
parseToTreeConjBar1 (ConjBar1 conj detp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj'" ++ str where
  str = parseToTreeConj conj (ntabs+1) ++ parseToTreeDetP detp (ntabs+1)

parseToTreeConjBar2 :: ConjBar -> Int -> String
parseToTreeConjBar2 (ConjBar2 conj advp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj'" ++ str where
  str = parseToTreeConj conj (ntabs+1) ++ parseToTreeAdvP advp (ntabs+1)

parseToTreeConjBar3 :: ConjBar -> Int -> String
parseToTreeConjBar3 (ConjBar3 conj compp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj'" ++ str where
  str = parseToTreeConj conj (ntabs+1) ++ parseToTreeCompP compp (ntabs+1)

parseToTreeConjBar4 :: ConjBar -> Int -> String
parseToTreeConjBar4 (ConjBar4 conj adjp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj'" ++ str where
  str = parseToTreeConj conj (ntabs+1) ++ parseToTreeAdjP adjp (ntabs+1)

parseToTreeConjBar5 :: ConjBar -> Int -> String
parseToTreeConjBar5 (ConjBar5 conj prepp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj'" ++ str where
  str = parseToTreeConj conj (ntabs+1) ++ parseToTreePrepP prepp (ntabs+1)

parseToTreeConjBar6 :: ConjBar -> Int -> String
parseToTreeConjBar6 (ConjBar6 conj nounp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj'" ++ str where
  str = parseToTreeConj conj (ntabs+1) ++ parseToTreeNounP nounp (ntabs+1)

parseToTreeConjBar7 :: ConjBar -> Int -> String
parseToTreeConjBar7 (ConjBar7 conj verbp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj'" ++ str where
  str = parseToTreeConj conj (ntabs+1) ++ parseToTreeVerbP verbp (ntabs+1)

parseToTreeConjBar8 :: ConjBar -> Int -> String
parseToTreeConjBar8 (ConjBar8 conj verbp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj'" ++ str where
  str = parseToTreeConj conj (ntabs+1) ++ parseToTreeTenseP verbp (ntabs+1)

parseToTreeTenseBar :: TenseBar -> Int -> String
parseToTreeTenseBar (TenseBar1 tense verbp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "T'" ++ str where
  str = parseToTreeTense tense (ntabs+1) ++ parseToTreeVerbP verbp (ntabs+1)
parseToTreeTenseBar (TenseBar2 tense conjp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "T'" ++ str where
  str = parseToTreeTense tense (ntabs+1) ++ parseToTreeConjP conjp (ntabs+1)

parseToTreeCompBar :: CompBar -> Int -> String
parseToTreeCompBar compbar ntabs = "\n" ++ (replicate ntabs '\t') ++ "C'" ++ str where
  CompBar comp tensep = compbar
  str = parseToTreeComp comp (ntabs+1) ++ parseToTreeTenseP tensep (ntabs+1)

parseToTreeDetBar :: DetBar -> Int -> String
parseToTreeDetBar (DetBar1 det nounp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "D'" ++ str where
  str = parseToTreeDet det (ntabs+1) ++ parseToTreeNounP nounp (ntabs+1)
parseToTreeDetBar (DetBar2 det conjp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "D'" ++ str where
  str = parseToTreeDet det (ntabs+1) ++ parseToTreeConjP conjp (ntabs+1)

parseToTreePrepBar :: PrepBar -> Int -> String
parseToTreePrepBar (PrepBar1 prepbar prepp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "P'" ++ str where
  str = parseToTreePrepBar prepbar (ntabs+1) ++ parseToTreePrepP prepp (ntabs+1)
parseToTreePrepBar (PrepBar2 prep detp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "P'" ++ str where
  str = parseToTreePrep prep (ntabs+1) ++ parseToTreeDetP detp (ntabs+1)

parseToTreeAdvBar :: AdvBar -> Int -> String
parseToTreeAdvBar advbar ntabs = "\n" ++ (replicate ntabs '\t') ++ "Adv'" ++ str where
  AdvBar adv = advbar
  str = parseToTreeAdv adv (ntabs+1)

parseToTreeAdjBar :: AdjBar -> Int -> String
parseToTreeAdjBar (AdjBar1 adjp adjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Adj'" ++ str where
  str = parseToTreeAdjP adjp (ntabs+1) ++ parseToTreeAdjBar adjbar (ntabs+1)
parseToTreeAdjBar (AdjBar2 conjp adjbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Adj'" ++ str where
  str = parseToTreeConjP conjp (ntabs+1) ++ parseToTreeAdjBar adjbar (ntabs+1)
parseToTreeAdjBar (AdjBar3 adjbar prepp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Adj'" ++ str where
  str = parseToTreeAdjBar adjbar (ntabs+1) ++ parseToTreePrepP prepp (ntabs+1)
parseToTreeAdjBar (AdjBar4 adj optprepp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Adj'" ++ str where
  fooBar (YesOpt prepp) = parseToTreeAdj adj (ntabs+1) ++ parseToTreePrepP prepp (ntabs+1)
  fooBar NoOpt = parseToTreeAdj adj (ntabs+1)
  str = fooBar optprepp

parseToTreeVerbBar :: VerbBar -> Int -> String
parseToTreeVerbBar (VerbBar1 advp verbbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeAdvP advp (ntabs+1) ++ parseToTreeVerbBar verbbar (ntabs+1)
parseToTreeVerbBar (VerbBar2 conjp verbbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeConjP conjp (ntabs+1) ++ parseToTreeVerbBar verbbar (ntabs+1)
parseToTreeVerbBar (VerbBar3 verbbar advp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeVerbBar verbbar (ntabs+1) ++ parseToTreeAdvP advp (ntabs+1)
parseToTreeVerbBar (VerbBar4 verbbar conjp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeVerbBar verbbar (ntabs+1) ++ parseToTreeConjP conjp (ntabs+1)
parseToTreeVerbBar (VerbBar5 verbbar prepp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeVerbBar verbbar (ntabs+1) ++ parseToTreePrepP prepp (ntabs+1)
parseToTreeVerbBar (VerbBar6 verbbar conjp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeVerbBar verbbar (ntabs+1) ++ parseToTreeConjP conjp (ntabs+1)
parseToTreeVerbBar (VerbBar7 verb compp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeVerb verb (ntabs+1) ++ parseToTreeCompP compp (ntabs+1)
parseToTreeVerbBar (VerbBar8 verb conjp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeVerb verb (ntabs+1) ++ parseToTreeConjP conjp (ntabs+1)
parseToTreeVerbBar (VerbBar9 verb detp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeVerb verb (ntabs+1) ++ parseToTreeDetP detp (ntabs+1)
parseToTreeVerbBar (VerbBar0 verb conjp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V'" ++ str where
  str = parseToTreeVerb verb (ntabs+1) ++ parseToTreeConjP conjp (ntabs+1)

parseToTreeNounBar :: NounBar -> Int -> String
parseToTreeNounBar (NounBar1 adjp nounbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "N'" ++ str where
  str = parseToTreeAdjP adjp (ntabs+1) ++ parseToTreeNounBar nounbar (ntabs+1)
parseToTreeNounBar (NounBar2 conjp nounbar) ntabs = "\n" ++ (replicate ntabs '\t') ++ "N'" ++ str where
  str = parseToTreeConjP conjp (ntabs+1) ++ parseToTreeNounBar nounbar (ntabs+1)
parseToTreeNounBar (NounBar3 nounbar prepp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "N'" ++ str where
  str = parseToTreeNounBar nounbar (ntabs+1) ++ parseToTreePrepP prepp (ntabs+1)
parseToTreeNounBar (NounBar4 noun prepp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "N'" ++ str where
  str = parseToTreeNoun noun (ntabs+1) ++ parseToTreePrepP prepp (ntabs+1)
parseToTreeNounBar (NounBar5 noun conjp) ntabs = "\n" ++ (replicate ntabs '\t') ++ "N'" ++ str where
  str = parseToTreeNoun noun (ntabs+1) ++ parseToTreeConjP conjp (ntabs+1)
parseToTreeNounBar (NounBar6 noun ) ntabs = "\n" ++ (replicate ntabs '\t') ++ "N'" ++ str where
  str = parseToTreeNoun noun (ntabs+1)
-----------------------------------parse X--------------------------------------

parseToTreeNeg :: Neg -> Int -> String
parseToTreeNeg (Neg str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Neg\n" ++ (replicate (ntabs+1) '\t') ++ str

parseToTreeConj :: Conj -> Int -> String
parseToTreeConj (Conj1 str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj\n" ++ (replicate (ntabs+1) '\t') ++ str
parseToTreeConj (Conj2 str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Conj\n" ++ (replicate (ntabs+1) '\t') ++ str

parseToTreeTense :: Tense -> Int -> String
parseToTreeTense (Tense str1 str2) ntabs = "\n" ++ (replicate ntabs '\t') ++ "T\n" ++ (replicate (ntabs+1) '\t') ++ "(" ++ str1 ++ " ___" ++ str2 ++ ")"

parseToTreeComp :: Comp -> Int -> String
parseToTreeComp (Comp str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "C\n" ++ (replicate (ntabs+1) '\t') ++ str

parseToTreeDet :: Det -> Int -> String
parseToTreeDet (Det str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "D\n" ++ (replicate (ntabs+1) '\t') ++ str

parseToTreePrep :: Prep -> Int -> String
parseToTreePrep (Prep str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "P\n" ++ (replicate (ntabs+1) '\t') ++ str

parseToTreeAdv :: Adv -> Int -> String
parseToTreeAdv (Adv str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Adv\n" ++ (replicate (ntabs+1) '\t') ++ str

parseToTreeAdj :: Adj -> Int -> String
parseToTreeAdj (Adj str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "Adj\n" ++ (replicate (ntabs+1) '\t') ++ str

parseToTreeVerb :: Verb -> Int -> String
parseToTreeVerb (Verb str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "V\n" ++ (replicate (ntabs+1) '\t') ++ str

parseToTreeNoun :: Noun -> Int -> String
parseToTreeNoun (Noun str) ntabs = "\n" ++ (replicate ntabs '\t') ++ "N\n" ++ (replicate (ntabs+1) '\t') ++ str
