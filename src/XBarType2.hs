module XBarType2
( LexCat(..)
, Phrase(..)
, Bar(..)
, Head(..)
, HeadR(..)
, BarR(..)
, PhraseR(..)
) where

data LexCat = Null | Comp | Infl | Verb | Det | Noun | Prep | Adj | Adv | Neg | Quan | Agr deriving (Show)
data Phrase a = XP1 (Phrase LexCat) (Bar a)
              | XP2 (Bar a) (Phrase LexCat) deriving (Show)
data Bar a = XBar1 (Phrase LexCat) (Bar a)
           | XBar2 (Bar a) (Phrase LexCat)
           | XBar3 (Phrase LexCat) (Head a)
           | XBar4 (Head a) (Phrase LexCat) deriving (Show)
data Head a = Head String deriving (Show)

--looking back / reverse - this needs to be simpler
data HeadR a = HeadR0 a deriving (Show)
data BarR a = XBarR0 a
            | XBarR1 (BarR a) (BarR a) -- grandparent, sister
            | XBarR2 (BarR a) (BarR a) -- sister, grandparent
            | XBarR3 (PhraseR a) (BarR a)
            | XBarR4 (BarR a) (PhraseR a)
            | XBarR5 (PhraseR LexCat) (BarR a)
            | XBarR6 (BarR a) (PhraseR LexCat)
            | XBarR7 (PhraseR LexCat) (PhraseR a)
            | XBarR8 (PhraseR a) (PhraseR LexCat)
            | XBarR9 (BarR a) (HeadR a)
            | XBarR10 (HeadR a) (BarR a)
            | XBarR11 (PhraseR a) (HeadR a)
            | XBarR12 (HeadR a) (PhraseR a)

data PhraseR a = XPR0 a
               | XPR1 (PhraseR LexCat) (PhraseR LexCat) -- grandparent, sister
               | XPR2 (PhraseR LexCat) (PhraseR LexCat) -- sister, grandparent
               | XPR3 (BarR a) (PhraseR LexCat)
               | XPR4 (PhraseR LexCat) (BarR a)
               | XPR5 (BarR LexCat) (BarR a)
               | XPR6 (BarR a) (BarR LexCat)
               | XPR7 (BarR LexCat) (PhraseR LexCat)
               | XPR8 (PhraseR LexCat) (BarR LexCat)
