{-# LANGUAGE OverloadedStrings #-}
module Cypher.Dsl where

import Control.Monad.Trans.State
import qualified Data.Set as S
import qualified Data.Text as T

import Cypher.Types

data CypherEnv = CypherEnv
  { _vars :: S.Set Variable
  }

match :: Pattern -> State CypherEnv T.Text
match p = do
  (CypherEnv s) <- get
  let b = S.member (Variable "v") s
  pure "f"

data RightRel a = RDash a | RArrow a

-- | Right dash, proceeding a relationship.
(|-) :: RelationshipDetail -> NodePattern -> RightRel (RelationshipDetail, NodePattern)
(|-) r n = RDash (r,n)
infixr 5 |-

-- | Left dash, preceeding a relationship.
(-|) :: NodePattern -> RightRel (RelationshipDetail, NodePattern) -> PatternElement
(-|) n (RDash (r, n2)) = PatternElement n (Just $ PatternElementChain (NoDirection (Just r)) n2)
infixr 5 -|

node :: Maybe Variable -> [Label] -> [Property] -> NodePattern
node = NodePattern

var :: T.Text -> Variable
var = Variable

label :: T.Text -> Label
label = Label

labels :: [T.Text] -> [Label]
labels = fmap label

-- | Convert a key and its value into a 'Property'.
prop :: T.Text -> T.Text -> Property
prop = Property

-- | Convert a list of tuples of key value pairs into a List
-- of 'Property''s.
toProps :: [(T.Text, T.Text)] -> [Property]
toProps = fmap (uncurry prop)
