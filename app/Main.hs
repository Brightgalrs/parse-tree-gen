module Main where

import           Data.Random
import           Data.RVar
import           System.IO
import           Data.List

import Min
--import           MakeXBar2
--import           XBarType2
--import           LoadData
{-
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
-}

main :: IO ()
main = do
  let lexicon = [ LexItem [Minus N, D] "the"
                , LexItem [Minus N, D, Minus Nom] "the"
                , LexItem [Minus N, D, Minus WH] "which"
                , LexItem [Minus N, D, Minus Nom, Minus WH] "which"
                , LexItem [Minus D, Minus D, D] "\'s"
                , LexItem [N] "pigs"
                , LexItem [V] "sleep"
                , LexItem [Minus D, V] "kiss"
                , LexItem [Minus D, Minus D, V] "owe"
                , LexItem [Minus C, Minus D, V] "tell"
                , LexItem [Minus V, Minus D, LV] "" --v
                , LexItem [Minus LV, Nom, T] "" --tense?
                , LexItem [Minus T, C] "that"
                , LexItem [Minus T, C] "" -- sentence
                , LexItem [Minus T, WH, C] "" -- interrogative sentence
                ]

  let num = [ LexItem [Minus T, C] ""
            , LexItem [Minus LV, Nom, T] ""
            , LexItem [Minus N, D, Minus Nom] "the"
            , LexItem [N] "pigs"
            , LexItem [Minus V, Minus D, LV] ""
            , LexItem [V] "sleep"
            ]
  let objs = derive num []
  let out = map spellout objs

  putStr $ intercalate "\n" out ++ "\n"
