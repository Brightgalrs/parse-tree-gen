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
  showtree_ :: String -> String -> a -> String

instance Show a => ShowTree (Phrase a) where
  showtree = showtree_ " " " "
  showtree_ pad char (XP t Ini p b) = "\n" ++ init pad ++ char ++ show t ++ "P" ++ showtree_ (pad++"|") "├" p ++ showtree_ (pad++" ") "└" b
  showtree_ pad char (XP t Fin XPNull b) = "\n" ++ init pad ++ char ++ show t ++ "P" ++ showtree_ (pad++" ") "└" b
  showtree_ pad char (XP t Fin p b) = "\n" ++ init pad ++ char ++ show t ++ "P" ++ showtree_ (pad++"│") "├" b ++ showtree_ (pad++" ") "└" p
  showtree_ _ _ XPNull = ""

instance Show a => ShowTree (Bar a) where
  showtree = showtree_ " " " "
  showtree_ pad char (XBar1 t Ini p b) = "\n" ++ init pad ++ char ++ show t ++ "\'" ++ showtree_ (pad++"│") "├" p ++ showtree_ (pad++" ") "└" b
  showtree_ pad char (XBar1 t Fin XPNull b) = "\n" ++ init pad ++ char ++ show t ++ "\'" ++ showtree_ (pad++" ") "└" b
  showtree_ pad char (XBar1 t Fin p b) = "\n" ++ init pad ++ char ++ show t ++ "\'" ++ showtree_ (pad++"│") "├" b ++ showtree_ (pad++" ") "└" p
  showtree_ pad char (XBar2 t Ini h XPNull) = "\n" ++ init pad ++ char ++ show t ++ "\'" ++ showtree_ (pad++" ") "└" h
  showtree_ pad char (XBar2 t Ini h p) = "\n" ++ init pad ++ char ++ show t ++ "\'" ++ showtree_ (pad++"│") "├" h ++ showtree_ (pad++" ") "└" p
  showtree_ pad char (XBar2 t Fin h p) = "\n" ++ init pad ++ char ++ show t ++ "\'" ++ showtree_ (pad++"│") "├" p ++ showtree_ (pad++" ") "└" h

instance Show a => ShowTree (Head a) where
  showtree = showtree_ " " " "

  showtree_ pad char (Head t "") = "\n" ++ init pad ++ char ++ show t ++ "\n" ++ pad ++ "└∅"
  showtree_ pad char (Head t s) = "\n" ++ init pad ++ char ++ show t ++ "\n" ++ pad ++ "└" ++ s
  showtree_ pad char (HInfl t (aux, ten)) = "\n" ++ init pad ++ char ++ show t ++ "\n" ++ pad ++ "└" ++ aux ++ " (-" ++ ten ++ ")"


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
