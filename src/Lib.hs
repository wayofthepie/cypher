module Lib where

import qualified Data.Text as T

import Prelude hiding ((-), Left)

newtype Pattern = Pattern [PatternPart] deriving (Eq, Show)
data PatternPart
  =  Assigned PatternElement
  | Anonymous PatternElement
  deriving (Eq, Show)

newtype PatternElement = E Path deriving (Eq, Show)
data Path = Path NodePattern (Maybe PatternElementChain) deriving (Eq, Show)
data NodePattern = NodePattern
  { _var :: T.Text
  , _labels :: [T.Text]
  , _props :: [T.Text]
  } deriving (Eq, Show)
data PatternElementChain = PatternElementChain RelationshipPattern NodePattern
  deriving (Eq, Show)

data RelationshipPattern
  = BiDirectional (Maybe RelationshipDetail)
  | ArrowLeft (Maybe RelationshipDetail)
  | ArrowRight (Maybe RelationshipDetail)
  | NoDirection (Maybe RelationshipDetail)
  deriving (Eq, Show)

-- | TODO support range literals.
data RelationshipDetail = RelationshipDetail
  { _relVar :: Maybe Variable
  , _relTypes :: Maybe RelationshipTypes
  , _relProperties :: [Property]
  } deriving (Eq, Show)

-- | TODO is this sufficient?
newtype RelationshipTypes = RelationshipTypes [T.Text] deriving (Eq, Show)

newtype Variable = Variable T.Text deriving (Eq, Show)

-- | TODO the cypher spec has this as a MapLiteral or a parameter
newtype Property = Property T.Text deriving (Eq, Show)
