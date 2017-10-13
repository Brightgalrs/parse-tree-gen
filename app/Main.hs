module Main where

import System.IO
import Data.List

import Min
import MinData

main :: IO ()
main = do
{-  let lexicon = [ LexItem [Equal N, D] noPhi "the"
                , LexItem [Equal N, D, Minus Nom] noPhi "the"
                , LexItem [Equal N, D, Minus WH] noPhi "which"
                , LexItem [Equal N, D, Minus Nom, Minus WH] noPhi "which"
                , LexItem [Equal D, Equal D, D] noPhi "\'s"
                , LexItem [N] noPhi "pigs"
                , LexItem [V] noPhi "sleep"
                , LexItem [Equal D, V] noPhi "kiss"
                , LexItem [Equal D, Equal D, V] noPhi "owe"
                , LexItem [Equal C, Equal D, V] noPhi "tell"
                , LexItem [Equal V, Equal D, LV] noPhi "" --v
                , LexItem [Equal LV, Nom, T] noPhi "" --tense?
                , LexItem [Equal T, C] noPhi "that"
                , LexItem [Equal T, C] noPhi "" -- sentence
                , LexItem [Equal T, WH, C] noPhi "" -- interrogative sentence
                ]

  let num = [ LexItem [Equal T, C] noPhi ""
            , LexItem [Equal LV, Nom, T] noPhi ""
            , LexItem [Equal N, D, Minus Nom] noPhi "the"
            , LexItem [N] noPhi "pigs"
            , LexItem [Equal V, Equal D, LV] noPhi ""
            , LexItem [Equal D, Acc, V] noPhi "kiss"
            --, LexItem [Equal N, ] noPhi "" -- null determiner for "John"
            , LexItem [D, Minus Acc] noPhi "John"
            ]
-}
-- Feature Uninterp Case Weak
-- , Feature Interp Case Weak

  let num2 = [ LexItem [Feature Interp C Weak, Feature Uninterp T Weak] NoPhi  "" -- C
             , LexItem [Feature Interp T Weak, Feature Uninterp LV Weak, Feature Uninterp D Weak] NoPhi  "" -- T
             , LexItem [Feature Interp LV Weak, Feature Uninterp V Weak, Feature Uninterp D Weak] NoPhi  "" -- Little v
             , LexItem [Feature Interp V Weak, Feature Uninterp D Weak] NoPhi  "divorce"
             , LexItem [Feature Interp D Weak] NoPhi  "Mary"
             , LexItem [Feature Interp D Weak] NoPhi  "John"
             ]

  let num3 = [ LexItem [Feature Uninterp T Weak, Feature Interp C Weak] NoPhi  "C"
             , LexItem [Feature Uninterp V Weak, Feature Interp T Weak] NoPhi  "T"
             , LexItem [Feature Uninterp N Weak, Feature Interp V Weak] NoPhi  "V"
             , LexItem [Feature Interp N Weak] NoPhi  "N"
             ]

  let objs = derive num2 []
  writeFile "treeoutput.txt" (intercalate "\n" (map (\x -> "\n" ++ spellout x ++ showtree x ++ "\n" ++ show x) objs))
