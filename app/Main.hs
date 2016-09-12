module Main where

--import PhonologyGen
import           Data.Random
import           Data.RVar
import           ParseTreeGen
import           ParseTreeParseIntoSentence
import           ParseTreeParseIntoTree
import           System.IO


main :: IO ()
main = do
  dat <- loadInputData
  struct <- sampleRVar (makeTenseP dat 6)
  writeFile "treeoutput.txt" $ (show struct) ++ "\n" ++ (parseTenseP struct) ++ "\n" ++ (parseToTreeTenseP struct 0)
