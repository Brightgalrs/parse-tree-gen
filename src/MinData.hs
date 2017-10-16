module MinData
( Numeration
, Workspace
, SyntaxObj(..)
, Feature(..)
, Category(..)
, Number(..)
, Gender(..)
, Person(..)
, showtree
) where

type Numeration = [SyntaxObj]
type Workspace = [SyntaxObj]

data SyntaxObj = Crash
               | LexItem { feats :: [Feature]
                         , str :: String
                         }
               | Trace { feats ::  [Feature]
                       , string :: String
                       }
               | Branch { feats :: [Feature]
                        , left :: SyntaxObj
                        , right :: SyntaxObj
                        } deriving (Eq, Show)

-- show tree
class ShowTree a where
  showtree :: a -> String
  showtree_ :: String -> String -> a -> String

instance ShowTree SyntaxObj where
  showtree = showtree_ " " " "
  showtree_ pad char (Branch fs left right) = "\n" ++ init pad ++ char ++ show fs ++ showtree_ (pad++"|") "├" left ++ showtree_ (pad++" ") "└" right
  showtree_ pad char (LexItem fs "") = "\n" ++ init pad ++ char ++ show fs
  showtree_ pad char (LexItem fs s) = "\n" ++ init pad ++ char ++ show fs ++ "\n" ++ pad ++ "└" ++ s
  showtree_ pad char (Trace _ s) = "\n" ++ init pad ++ char ++ "t (" ++ s ++ ")"
  showtree_ pad char Crash = "\n" ++ init pad ++ char ++ "<CRASH>"

-- features
data Feature = UFeature {category :: Category}
             | IFeature {category :: Category} deriving (Eq)

instance Show Feature where
  show (UFeature c) = "u" ++ show c
  show (IFeature c) = show c

data Category = D | N | V | P | C | T | LV | Case Case
              | Phi { num :: Number
                    , per :: Person
                    , gen :: Gender
                    } deriving (Eq)

instance Show Category where
  show (Phi UNum UPer UGen) = "φ:__"
  show (Phi n p g) = "φ:" ++ unwords [show n, show p, show g]
  show (Case c) = "Case:" ++ show c
  show D = "D"
  show N = "N"
  show V = "V"
  show P = "P"
  show C = "C"
  show T = "T"
  show LV = "v"

data Number = UNum | Pl | Sg deriving (Show, Eq)

data Person = UPer | First | Second | Third deriving (Eq)
instance Show Person where
  show UPer = ""
  show First = "1"
  show Second = "2"
  show Third = "3"

data Gender = UGen | Masc | Fem deriving (Show, Eq)

data Case   = UCase | Nom | Acc | Gen | Dat deriving (Eq)
instance Show Case where
  show UCase = "__"
  show Nom = "Nom"
  show Acc = "Acc"
  show Gen = "Gen"
  show Dat = "Dat"
