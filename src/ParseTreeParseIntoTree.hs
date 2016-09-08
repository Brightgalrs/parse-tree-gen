module ParseTreeParseIntoTree
( parseToTreeTenseP
) where

import           Control.Monad
import           Data.Random.Extras hiding (shuffle)
import           Data.RVar
import           Prelude
import           XBarType
import           ParseTreeGen

--Parse the tree and output a tree diagram
parseToTreeTenseP :: TenseP -> Int -> String
parseToTreeTenseP tensep ntabs = (replicate ntabs '\t') ++ str where
  TenseP detp tensebar = tensep
  str = parseToTreeDetP detp (ntabs+1) ++ "\n" ++ parseToTreeTenseBar tensebar (ntabs+1)

parseToTreeCompP :: CompP -> Int -> String
parseToTreeCompP compp ntabs = (replicate ntabs '\t') ++ str where
  CompP compbar = compp
  str = parseToTreeCompBar compbar (ntabs+1)

parseToTreeDetP :: DetP -> Int -> String
parseToTreeDetP detp ntabs = (replicate ntabs '\t') ++ str where
  DetP detbar = detp
  str = parseToTreeDetBar detbar (ntabs+1)

parseToTreePrepP :: PrepP -> Int -> String
parseToTreePrepP prepp ntabs = (replicate ntabs '\t') ++ str where
  PrepP optadjp prepbar = prepp
  fooBar (YesOpt adjp) = parseToTreeAdjP adjp (ntabs+1) ++ "\n" ++ parseToTreePrepBar prepbar (ntabs+1)
  fooBar NoOpt = parseToTreePrepBar prepbar (ntabs+1)
  str = fooBar optadjp

parseToTreeAdvP :: AdvP -> Int -> String
parseToTreeAdvP advp ntabs = (replicate ntabs '\t') ++ str where
  AdvP optadvp advbar = advp
  fooBar (YesOpt advp) = parseToTreeAdvP advp (ntabs+1) ++ "\n" ++ parseToTreeAdvBar advbar (ntabs+1)
  fooBar NoOpt = parseToTreeAdvBar advbar (ntabs+1)
  str = fooBar optadvp

parseToTreeAdjP :: AdjP -> Int -> String
parseToTreeAdjP adjp ntabs = (replicate ntabs '\t') ++ str where
  AdjP adjbar = adjp
  str = parseToTreeAdjBar adjbar (ntabs+1)

parseToTreeVerbP :: VerbP -> Int -> String
parseToTreeVerbP verbp ntabs = (replicate ntabs '\t') ++ str where
  VerbP verbbar = verbp
  str = parseToTreeVerbBar verbbar (ntabs+1)

parseToTreeNounP :: NounP -> Int -> String
parseToTreeNounP nounp ntabs = (replicate ntabs '\t') ++ str where
  NounP nounbar = nounp
  str = parseToTreeNounBar nounbar (ntabs+1)

parseToTreeTenseBar :: TenseBar -> Int -> String
parseToTreeTenseBar tensebar ntabs = (replicate ntabs '\t') ++ str where
  TenseBar tense verbp = tensebar
  str = parseToTreeTense tense (ntabs+1) ++ "\n" ++ parseToTreeVerbP verbp (ntabs+1)

parseToTreeCompBar :: CompBar -> Int -> String
parseToTreeCompBar compbar ntabs = (replicate ntabs '\t') ++ str where
  CompBar comp tensep = compbar
  str = parseToTreeComp comp (ntabs+1) ++ "\n" ++ parseToTreeTenseP tensep (ntabs+1)

parseToTreeDetBar :: DetBar -> Int -> String
parseToTreeDetBar detbar ntabs = (replicate ntabs '\t') ++ str where
  DetBar optdet nounp = detbar
  fooBar (YesOpt det) = parseToTreeDet det (ntabs+1) ++ "\n" ++ parseToTreeNounP nounp (ntabs+1)
  fooBar NoOpt = parseToTreeNounP nounp (ntabs+1)
  str = fooBar optdet

parseToTreePrepBar :: PrepBar -> Int -> String
parseToTreePrepBar (PrepBar1 prepbar optprepp) ntabs = (replicate ntabs '\t') ++ str where
  fooBar (YesOpt prepp) = parseToTreePrepBar prepbar (ntabs+1) ++ "\n" ++ parseToTreePrepP prepp (ntabs+1)
  fooBar NoOpt = parseToTreePrepBar prepbar (ntabs+1)
  str = fooBar optprepp
parseToTreePrepBar (PrepBar2 prep detp) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreePrep prep (ntabs+1) ++ "\n" ++ parseToTreeDetP detp (ntabs+1)

parseToTreeAdvBar :: AdvBar -> Int -> String
parseToTreeAdvBar advbar ntabs = (replicate ntabs '\t') ++ str where
  AdvBar adv = advbar
  str = parseToTreeAdv adv (ntabs+1)

parseToTreeAdjBar :: AdjBar -> Int -> String
parseToTreeAdjBar (AdjBar1 advp adjbar) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreeAdvP advp (ntabs+1) ++ "\n" ++ parseToTreeAdjBar adjbar (ntabs+1)
parseToTreeAdjBar (AdjBar2 adjp adjbar) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreeAdjP adjp (ntabs+1) ++ "\n" ++ parseToTreeAdjBar adjbar (ntabs+1)
parseToTreeAdjBar (AdjBar3 adjbar optprepp) ntabs = (replicate ntabs '\t') ++ str where
  fooBar (YesOpt prepp) = parseToTreeAdjBar adjbar (ntabs+1) ++ "\n" ++ parseToTreePrepP prepp (ntabs+1)
  fooBar NoOpt = parseToTreeAdjBar adjbar (ntabs+1)
  str = fooBar optprepp
parseToTreeAdjBar (AdjBar4 adj optprepp) ntabs = (replicate ntabs '\t') ++ str where
  fooBar (YesOpt prepp) = parseToTreeAdj adj (ntabs+1) ++ "\n" ++ parseToTreePrepP prepp (ntabs+1)
  fooBar NoOpt = parseToTreeAdj adj (ntabs+1)
  str = fooBar optprepp

parseToTreeVerbBar :: VerbBar -> Int -> String
parseToTreeVerbBar (VerbBar1 advp verbbar) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreeAdvP advp (ntabs+1) ++ "\n" ++ parseToTreeVerbBar verbbar (ntabs+1)
parseToTreeVerbBar (VerbBar2 verbbar prepp) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreeVerbBar verbbar (ntabs+1) ++ "\n" ++ parseToTreePrepP prepp (ntabs+1)
parseToTreeVerbBar (VerbBar3 verbbar advp) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreeVerbBar verbbar (ntabs+1) ++ "\n" ++ parseToTreeAdvP advp (ntabs+1)
parseToTreeVerbBar (VerbBar4 verb compp) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreeVerb verb (ntabs+1) ++ "\n" ++ parseToTreeCompP compp (ntabs+1)
parseToTreeVerbBar (VerbBar5 verb detp) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreeVerb verb (ntabs+1) ++ "\n" ++ parseToTreeDetP detp (ntabs+1)

parseToTreeNounBar :: NounBar -> Int -> String
parseToTreeNounBar (NounBar1 adjp nounbar) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreeAdjP adjp (ntabs+1) ++ "\n" ++ parseToTreeNounBar nounbar (ntabs+1)
parseToTreeNounBar (NounBar2 nounbar prepp) ntabs = (replicate ntabs '\t') ++ str where
  str = parseToTreeNounBar nounbar (ntabs+1) ++ "\n" ++ parseToTreePrepP prepp (ntabs+1)
parseToTreeNounBar (NounBar3 noun optprepp) ntabs = (replicate ntabs '\t') ++ str where
  fooBar (YesOpt prepp) = parseToTreeNoun noun (ntabs+1) ++ "\n" ++ parseToTreePrepP prepp (ntabs+1)
  fooBar NoOpt = parseToTreeNoun noun (ntabs+1)
  str = fooBar optprepp

parseToTreeTense :: Tense -> Int -> String
parseToTreeTense (Tense str) ntabs = (replicate ntabs '\t') ++ str

parseToTreeComp :: Comp -> Int -> String
parseToTreeComp (Comp str) ntabs = (replicate ntabs '\t') ++ str

parseToTreeDet :: Det -> Int -> String
parseToTreeDet (Det str) ntabs = (replicate ntabs '\t') ++ str

parseToTreePrep :: Prep -> Int -> String
parseToTreePrep (Prep str) ntabs = (replicate ntabs '\t') ++ str

parseToTreeAdv :: Adv -> Int -> String
parseToTreeAdv (Adv str) ntabs = (replicate ntabs '\t') ++ str

parseToTreeAdj :: Adj -> Int -> String
parseToTreeAdj (Adj str) ntabs = (replicate ntabs '\t') ++ str

parseToTreeVerb :: Verb -> Int -> String
parseToTreeVerb (Verb str) ntabs = (replicate ntabs '\t') ++ str

parseToTreeNoun :: Noun -> Int -> String
parseToTreeNoun (Noun str) ntabs = (replicate ntabs '\t') ++ str
