module XBarType
( Optional(..)
, NounP(..), NounBar(..), Noun(..)
, VerbP(..), VerbBar(..), Verb(..)
, AdjP(..), AdjBar(..), Adj(..)
, AdvP(..), AdvBar(..), Adv(..)
, PrepP(..), PrepBar(..), Prep(..)
, DetP(..), DetBar(..), Det(..)
, CompP(..), CompBar(..), Comp(..)
, TenseP(..), TenseBar(..), Tense(..)
, ConjP(..), ConjBar(..), Conj(..)
, NegP(..), NegBar(..), Neg(..)
, Pron(..)
) where

-- X-bar theory data

--data Reverse a b = XbarFirst a b | XbarSecond b a

data Optional a = YesOpt a | NoOpt  deriving (Show, Read)

--Noun phrase
data NounP    = NounP NounBar deriving (Show, Read)
data NounBar  = NounBar1 AdjP NounBar
              | NounBar2 (ConjP AdjP) NounBar
              | NounBar3 NounBar PrepP
              | NounBar4 Noun PrepP
              | NounBar5 Noun (ConjP PrepP)
              | NounBar6 Noun deriving (Show, Read)
data Noun     = Noun String deriving (Show, Read)

--Verb phrase
data VerbP    = VerbP (Optional NegP) VerbBar deriving (Show, Read)
data VerbBar  = VerbBar1 AdvP VerbBar
              | VerbBar2 (ConjP AdvP) VerbBar
              | VerbBar3 VerbBar AdvP
              | VerbBar4 VerbBar (ConjP AdvP)
              | VerbBar5 VerbBar PrepP
              | VerbBar6 VerbBar (ConjP PrepP)
              | VerbBar7 Verb CompP
              | VerbBar8 Verb (ConjP CompP)
              | VerbBar9 Verb DetP
              | VerbBar0 Verb (ConjP DetP) deriving (Show, Read)
data Verb     = Verb String deriving (Show, Read)

--Adjective phrase
data AdjP     = AdjP (Optional AdvP) AdjBar deriving (Show, Read) --degree adv? no conj
data AdjBar   = AdjBar1 AdjP AdjBar
              | AdjBar2 (ConjP AdjP) AdjBar
              | AdjBar3 AdjBar PrepP
              | AdjBar4 Adj (Optional PrepP) deriving (Show, Read)
data Adj      = Adj String deriving (Show, Read)

--Adverb phrase
data AdvP     = AdvP (Optional AdvP) AdvBar deriving (Show, Read) --degree adverb? no conj
data AdvBar   = AdvBar Adv deriving (Show, Read)
data Adv      = Adv String deriving (Show, Read)

--Prepositional phrase
data PrepP    = PrepP (Optional AdvP) PrepBar deriving (Show, Read) --should be degree adverb, no conj
data PrepBar  = PrepBar1 PrepBar PrepP
              | PrepBar2 Prep DetP
              | PrepBar3 Prep (ConjP DetP) deriving (Show, Read)
data Prep     = Prep String deriving (Show, Read)

--Determiner phrase
data DetP     = DetP DetBar deriving (Show, Read)
data DetBar   = DetBar1 Det NounP
              | DetBar2 Det (ConjP NounP) deriving (Show, Read)
data Det      = Det String deriving (Show, Read)

--Complementizer phrase
data CompP    = CompP CompBar deriving (Show, Read)
data CompBar  = CompBar Comp TenseP deriving (Show, Read)
data Comp     = Comp String deriving (Show, Read)

--Tense phrase
data TenseP   = TenseP1 DetP TenseBar
              | TenseP2 (ConjP DetP) TenseBar deriving (Show, Read)
data TenseBar = TenseBar1 Tense VerbP
              | TenseBar2 Tense (ConjP VerbP) deriving (Show, Read)
data Tense    = Tense String String deriving (Show, Read)

--Conjunction phrase
data ConjP a   = ConjP a (ConjBar a) deriving (Show, Read)
data ConjBar a = ConjBar Conj a deriving (Show, Read)
data Conj      = Conj String deriving (Show, Read)

--Negation phrase
data NegP     = NegP NegBar deriving (Show, Read)
data NegBar   = NegBar Neg deriving (Show, Read)
data Neg      = Neg String deriving (Show, Read)

--Pronoun (special case of Noun? DetP?)
data Pron     = Pron String deriving (Show, Read) --needs implementing

--Adverb phrase for degree only (special case of AdvP)
data AdvDegP =  AdvDegP String deriving (Show, Read) --needs implementing
--AdvBarMannerP etc. etc.

--Adjective phrase for age (special case of AdjP)
data AdjAgeP =  AdjDegP String deriving (Show, Read) --needs implementing
--AdjColorP etc. etc.
