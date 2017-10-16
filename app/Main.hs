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
-- UFeature Case
-- , IFeature Case

  let num2 = [ LexItem [IFeature C, UFeature T] "" -- C
             , LexItem [IFeature T, UFeature LV, UFeature D] "" -- T
             , LexItem [IFeature LV, UFeature V, UFeature D] "" -- Little v
             , LexItem [IFeature V, UFeature D] "divorce"
             , LexItem [IFeature D] "Mary"
             , LexItem [IFeature D] "John"
             ]

  let num3 = [ LexItem [UFeature T, IFeature C] "C"
             , LexItem [UFeature V, IFeature T] "T"
             , LexItem [UFeature N, IFeature V] "V"
             , LexItem [IFeature N] "N"
             ]

  let objs = derive num2 []
  writeFile "treeoutput.txt" (intercalate "\n" (map (\x -> "\n" ++ linearize x ++ showtree x ++ "\n" ++ show x) objs))
