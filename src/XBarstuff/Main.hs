module Main where

import           Data.Random
import           Data.RVar
import           System.IO
import           Data.List

import           MakeXBar2
import           XBarType2
import           LoadData

main :: IO ()
main = do
  dat <- loadInputData
  -- number of phrases allowed in any line down the tree
  let lims = CatLimit { nullL = 100
                      , compL = 1
                      , inflL = 100
                      , verbL = 100
                      , detL = 100
                      , nounL = 100
                      , prepL = 1
                      , adjL = 1
                      , advL = 1
                      , negL = 100
                      , quanL = 100
                      , agrL = 100
                      }

  struct_ <- sampleRVar (makeXP dat lims Infl)
  let struct = XP Comp Ini XPNull (XBar2 Comp Ini (Head Comp "") struct_)
  writeFile "treeoutput.txt" $ showsen struct ++ "\n\n" ++ showtree struct
