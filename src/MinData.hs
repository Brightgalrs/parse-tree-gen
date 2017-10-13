module MinData
( Numeration
, Workspace
, SyntaxObj(..)
, Feature(..)
, Interpretable(..)
, Category(..)
, Stength(..)
, Phi(..)
, Number(..)
, Gender(..)
, Person(..)
, showtree
) where

type Numeration = [SyntaxObj]

type Workspace = [SyntaxObj]

data SyntaxObj = Crash
               | Trace
               | LexItem { feats :: [Feature]
                         , phis :: Phi
                         , str :: String
                         }
               | Branch { feats :: [Feature]
                        , phis :: Phi
                        , left :: SyntaxObj
                        , right :: SyntaxObj
                        } deriving (Eq, Show)

-- show tree
class ShowTree a where
  showtree :: a -> String
  showtree_ :: String -> String -> a -> String

instance ShowTree SyntaxObj where
  showtree = showtree_ " " " "
  showtree_ pad char (Branch fs _ left right) = "\n" ++ init pad ++ char ++ show (head $ filter ((Interp == ).interp) fs) ++ showtree_ (pad++"|") "├" left ++ showtree_ (pad++" ") "└" right
  showtree_ pad char (LexItem fs _ "") = "\n" ++ init pad ++ char ++ show (head $ filter ((Interp == ).interp) fs)
  showtree_ pad char (LexItem fs _ s) = "\n" ++ init pad ++ char ++ show (head $ filter ((Interp == ).interp) fs) ++ "\n" ++ pad ++ "└" ++ s
  showtree_ pad char Trace = "\n" ++ init pad ++ char ++ "t"
  showtree_ pad char Crash = "\n" ++ init pad ++ char ++ "<CRASH>"

-- features
data Feature = Feature { interp :: Interpretable
                       , category :: Category
                       , strength :: Stength
                       } deriving (Eq)
data Interpretable = Uninterp | Interp deriving (Eq)
data Category = D | N | V | P | C | T | LV | Case deriving (Show, Eq)
data Stength = Weak | Strong deriving (Eq)

instance Show Feature where
  show (Feature i c s) = show i ++ show c ++ show s
instance Show Interpretable where
  show Uninterp = "u"
  show Interp = ""
instance Show Stength where
  show Strong = "*"
  show Weak = ""


-- phi features?
data Phi = Phi { num :: Number
               , gen :: Gender
               , per :: Person
               }
         | NoPhi deriving (Show, Eq)
data Number = Num | Pl | Sing deriving (Show, Eq)
data Gender = Gen | Mas | Fem deriving (Show, Eq)
data Person = Per | First | Second | Third deriving (Show, Eq)

--data Direct = L | R deriving (Show, Eq)
