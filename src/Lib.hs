{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.List
import Data.Monoid
import Fmt
import Safe (headMay)
import qualified Data.Text as T

newtype Pattern = Pattern [PatternPart] deriving (Eq)

data PatternPart
  =  Assigned Variable PatternElement
  | Anonymous PatternElement
  deriving (Eq)

newtype PatternElement = PatternElement Path deriving (Eq)
data Path = Path NodePattern (Maybe PatternElementChain) deriving (Eq)

-- | Represents nodes in a pattern.
-- {
--  (n:Label {name: "SomeName"})
-- }
data NodePattern = NodePattern
  { _var :: Maybe Variable
  , _labels :: [Label]
  , _props :: [Property]
  } deriving (Eq)

genNodePattern :: NodePattern -> T.Text
genNodePattern (NodePattern maybeVar labels props) =
  ("("#|foldLabels maybeVar labels|#""#|genPropsPattern props|#")")

genPropsPattern :: [Property] -> T.Text
genPropsPattern props =
  maybe "" (\(first,rest) -> genPropsPattern' first rest) (uncons props)
 where
   genPropsPattern' f r = " {"#|foldProps f r|#"}"

-- | Fold an optional variable name and labels into the expected format.
foldLabels :: Maybe Variable -> [Label] -> T.Text
foldLabels maybeVar labels =
  let var = justOrEmpty maybeVar
  in  foldr (\(Label l) acc -> ""#|acc|#":"#|l|#"") var labels
 where
  justOrEmpty :: Maybe Variable -> T.Text
  justOrEmpty (Just (Variable v)) = v
  justOrEmpty Nothing = T.empty

foldProps :: Property -> [Property] -> T.Text
foldProps (Property k v) props = foldr toPropTextAcc (""#|k|#":\""#|v|#"\"") props
  where
    toPropTextAcc (Property k v) acc = ""#| acc |#", "#| k |#":\""#| v |# "\""

data PatternElementChain = PatternElementChain RelationshipPattern NodePattern
  deriving (Eq)

data RelationshipPattern
  = BiDirectional (Maybe RelationshipDetail)
  | ArrowLeft (Maybe RelationshipDetail)
  | ArrowRight (Maybe RelationshipDetail)
  | NoDirection (Maybe RelationshipDetail)
  deriving (Eq)

-- | TODO support range literals.
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

-- node :: Variable -> [Label] -> [Property] -> NodePattern
-- node = NodePattern
