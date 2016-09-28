module Main where

--import PhonologyGen

import           Data.Random
import           Data.RVar
import           MakeXBarXP
import           ParseIntoSentence
import           ParseIntoTree
import           System.IO
import           LoadData

main :: IO ()
main = do
  dat <- loadInputData
  --(cpl, ppl, ajl, avl, conj)
  --cpl: number of compliment phrases allowed (in a direct line)
  --ppl: number of prep phrases
  --ajl: number of adj phrases
  --avl: number of adv phrases
  --cjl: number of conj phrases
  struct <- sampleRVar (makeSentence dat (1,1,1,1,1))
  writeFile "treeoutput.txt" $ show struct
                            ++ "\n"
                            ++ parseSentence struct
                            ++ "\n"
                            ++ parseToTreeSentence struct
