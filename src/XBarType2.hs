{-# LANGUAGE FlexibleInstances #-}
module XBarType2
( LexCat(..)
, Pos(..)
, Phrase(..)
, Bar(..)
, Head(..)
, showsen
, showtree
, CatLimit(..)
) where

data LexCat = Null | Comp | Infl | Verb | Det | Noun | Prep | Adj | Adv | Neg | Quan | Agr deriving (Show, Eq)
data Pos = Ini | Fin
data Phrase a = XP a Pos (Phrase LexCat) (Bar a) | XPNull
data Bar a = XBar1 a Pos (Phrase LexCat) (Bar a)
           | XBar2 a Pos (Head a) (Phrase LexCat)
data Head a = Head a String | HInfl a (String, String)

class ShowSen a where
  showsen :: a -> String
  showsenT :: String -> a -> String --allows tense to be passed down

-- show sentence
instance ShowSen (Phrase LexCat) where
  showsen (XP _ i p b) = case i of Ini-> showsen p ++ showsen b
                                   Fin -> showsen b ++ showsen p
  showsen XPNull = ""

  showsenT t (XP _ i p b) = case i of Ini -> showsen p ++ showsenT t b
                                      Fin -> showsenT t b ++ showsen p

instance ShowSen (Bar LexCat) where
  showsen (XBar1 _ i p b) = case i of Ini -> showsen p ++ showsen b
                                      Fin -> showsen b ++ showsen p
  showsen (XBar2 Infl i (HInfl cat (aux, t)) p) = case i of Ini -> showsen (HInfl cat (aux, t)) ++ showsenT t p
                                                            Fin -> showsenT t p ++ showsen (HInfl cat (aux, t))
  showsen (XBar2 _ i h p) = case i of Ini -> showsen h ++ showsen p
                                      Fin -> showsen p ++ showsen h

  showsenT t (XBar1 _ i p b) = case i of Ini -> showsen p ++ showsenT t b
                                         Fin -> showsenT t b ++ showsen p
  showsenT t (XBar2 Verb i h p) = case i of Ini -> showsenT t h ++ showsen p
                                            Fin -> showsen p ++ showsenT t h
  showsenT t (XBar2 _ i h p) = case i of Ini -> showsen h ++ showsenT t p
                                         Fin -> showsenT t p ++ showsen h

instance ShowSen (Head LexCat) where
  showsen (Head _ "") = ""
  showsen (Head _ s) = s ++ " "
  showsen (HInfl _ ("", _)) = ""
  showsen (HInfl _ (aux, _)) = aux ++ " "

  showsenT t (Head _ s) = s ++ t ++ " "


-- show tree
class ShowTree a where
  showtree :: a -> String
  showtree_ :: Int -> a -> String

instance Show a => ShowTree (Phrase a) where
  showtree = showtree_ 0
  showtree_ ntab (XP t i p b) = "\n" ++ replicate ntab '\t' ++ show t ++ "P" ++ case i of Ini -> showtree_ (ntab+1) p ++ showtree_ (ntab+1) b
                                                                                          Fin -> showtree_ (ntab+1) b ++ showtree_ (ntab+1) p
  showtree_ _ XPNull = ""

instance Show a => ShowTree (Bar a) where
  showtree = showtree_ 0
  showtree_ ntab (XBar1 t i p b) = "\n" ++ replicate ntab '\t' ++ show t ++ "\'" ++ case i of Ini -> showtree_ (ntab+1) p ++ showtree_ (ntab+1) b
                                                                                              Fin -> showtree_ (ntab+1) b ++ showtree_ (ntab+1) p
  showtree_ ntab (XBar2 t i h p) = "\n" ++ replicate ntab '\t' ++ show t ++ "\'" ++ case i of Ini -> showtree_ (ntab+1) h ++ showtree_ (ntab+1) p
                                                                                              Fin -> showtree_ (ntab+1) p ++ showtree_ (ntab+1) h

instance Show a => ShowTree (Head a) where
  showtree = showtree_ 0

  showtree_ ntab (Head t "") = "\n" ++ replicate ntab '\t' ++ show t ++ "\n" ++ replicate (ntab+1) '\t' ++ "∅"
  showtree_ ntab (Head t s) = "\n" ++ replicate ntab '\t' ++ show t ++ "\n" ++ replicate (ntab+1) '\t' ++ s
  showtree_ ntab (HInfl t (aux, ten)) = "\n" ++ replicate ntab '\t' ++ show t ++ "\n" ++ replicate (ntab+1) '\t' ++ aux ++ " (-" ++ ten ++ ")"


-- category limits
data CatLimit = CatLimit { nullL :: Int
                         , compL :: Int
                         , inflL :: Int
                         , verbL :: Int
                         , detL :: Int
                         , nounL :: Int
                         , prepL :: Int
                         , adjL :: Int
                         , advL :: Int
                         , negL :: Int
                         , quanL :: Int
                         , agrL :: Int
                         }
