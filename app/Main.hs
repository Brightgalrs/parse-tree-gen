module Main where

--import PhonologyGen
import ParseTreeGen
import ParseTreeParseIntoSentence
import ParseTreeParseIntoTree
import Data.Random
import Data.RVar


main :: IO ()
main = do
  dat <- loadInputData
  struct <- sampleRVar (makeTenseP dat 6)
  putStr $ show $ parseTenseP struct
  putStr $ "\n"
  putStr $ parseToTreeTenseP struct 0
