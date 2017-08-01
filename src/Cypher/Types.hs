module Cypher.Types where

import qualified Data.Text as T

newtype Pattern = Pattern [PatternPart] deriving (Eq)

data PatternPart
  =  Assigned Variable PatternElement
  | Anonymous PatternElement
  deriving (Eq)

data PatternElement = PatternElement NodePattern (Maybe PatternElementChain) deriving (Eq)

data NodePattern = NodePattern
  { _var :: Maybe Variable
  , _labels :: [Label]
  , _props :: [Property]
  } deriving (Eq)

data PatternElementChain = PatternElementChain RelationshipPattern NodePattern
  deriving (Eq)

data RelationshipPattern
  = BiDirectional (Maybe RelationshipDetail)
  | ArrowLeft (Maybe RelationshipDetail)
  | ArrowRight (Maybe RelationshipDetail)
  | NoDirection (Maybe RelationshipDetail)
  deriving (Eq)

-- TODO support range literals.
data RelationshipDetail = RelationshipDetail
  { _relVar :: Maybe Variable
  , _relTypes :: [RelationshipType]
  , _relProperties :: [Property]
  } deriving (Eq)

-- | TODO is this sufficient?
newtype RelationshipType = RelationshipType T.Text deriving (Eq)

newtype Variable = Variable T.Text deriving (Eq)

-- | TODO the cypher spec has this as a MapLiteral or a parameter
data Property = Property T.Text T.Text deriving (Eq)
newtype Label = Label T.Text deriving (Eq)

newtype Optional = Optional T.Text deriving (Eq)

data Match = Match (Maybe Optional) Pattern deriving (Eq)
