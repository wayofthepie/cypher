{-# LANGUAGE OverloadedStrings #-}
module Cypher.Print where

import Data.List (uncons)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Fmt

import Cypher.Types


genPatternCypher :: Pattern -> Maybe T.Text
genPatternCypher (Pattern parts) =
  maybe Nothing (Just . (uncurry foldParts)) (uncons parts)
 where
  foldParts part parts =
    let partText = genPatternPartCypher part
    in  foldr (\p acc -> acc <> ", " <> genPatternPartCypher p) partText parts


genPatternPartCypher :: PatternPart -> T.Text
genPatternPartCypher (Assigned (Variable v) element) =
  v <> "=" <> genPatternElemCypher element
genPatternPartCypher (Anonymous element) = genPatternElemCypher element


genPatternElemCypher :: PatternElement -> T.Text
genPatternElemCypher (PatternElement node (Just chain)) =
  genNodeCypher node <> genPatternChainCypher chain
genPathCypher (PatternElement node Nothing) = genNodeCypher node


genPatternChainCypher :: PatternElementChain -> T.Text
genPatternChainCypher (PatternElementChain rel node) =
  genRelationshipCypher rel <> genNodeCypher node

-- | Generate the cypher value representing a 'NodePattern'.
--
-- @
-- >>> labels = [Label "MyLabel", Label "AnotherLabel"]
-- >>> props = [Property "f" "dsfd"]
-- >>> genNodeCypher $ NodePattern (Just (Variable "n")) labels props
-- "(n:AnotherLabel:MyLabel {f:\"dsfd\"})"
-- @
genNodeCypher :: NodePattern -> T.Text
genNodeCypher (NodePattern maybeVar labels props) =
  ("("#|foldVarWithLabels maybeVar labels|#""#|genPropsCypher props|#")")


-- | Fold an optional variable name and labels into their cypher format.
foldVarWithLabels :: Maybe Variable -> [Label] -> T.Text
foldVarWithLabels maybeVar labels =
  let var = justOrEmpty maybeVar
  in  foldr (\(Label l) acc -> ""#|acc|#":"#|l|#"") var labels


genRelationshipCypher :: RelationshipPattern -> T.Text
genRelationshipCypher (BiDirectional (Just rel)) =
  "<-["#|genRelationshipDetailCypher rel|#"]->"


genRelationshipDetailCypher :: RelationshipDetail -> T.Text
genRelationshipDetailCypher (RelationshipDetail maybeVar relTypes props) =
  foldRelVarWithTypes maybeVar relTypes <> genPropsCypher props


foldRelVarWithTypes :: Maybe Variable -> [RelationshipType] -> T.Text
foldRelVarWithTypes maybeVar relTypes =
  maybe var (uncurry f) (uncons relTypes)
 where
  var = justOrEmpty maybeVar
  f (RelationshipType rt) rts =
    foldRelVarWithTypes' (""#| var |#":"#| rt |#"") rts
  foldRelVarWithTypes' =
      foldr (\(RelationshipType rt) acc -> ""#| acc |#"|:"#|rt|#"")

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
-- | Convert a list of properties into its cypher format.
genPropsCypher :: [Property] -> T.Text
genPropsCypher props =
  maybe "" (\(first,rest) -> genPropsPattern' first rest) (uncons props)
 where
   genPropsPattern' f r = " {"#|foldProps f r|#"}"

-- | Fold a property and a list of properties into their cypher formats.
foldProps :: Property -> [Property] -> T.Text
foldProps (Property k v) props = foldr toPropTextAcc (""#|k|#":\""#|v|#"\"") props
  where
    toPropTextAcc (Property k v) acc = ""#| acc |#", "#| k |#":\""#| v |# "\""

justOrEmpty :: Maybe Variable -> T.Text
justOrEmpty (Just (Variable v)) = v
justOrEmpty Nothing = T.empty
