{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.State
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import Fmt

import Prelude hiding ((-), Left)

data N
data R
data Direction = L | R | Bi | None deriving Show
data PathComponent where
    Relationship  :: PathComponent -> PathEntity R -> PathComponent -> Direction -> PathComponent
    Node          :: PathEntity N -> PathComponent

instance Show PathComponent where
  show (Relationship n r n2 Bi) = (""#|show n|#"<-"#|show r|#"->"#|show n2|#"")
  show (Relationship n r n2 L) = (""#|show n|#"<-"#|show r|#"-"#|show n2|#"")
  show (Relationship n r n2 R) = (""#|show n|#"-"#|show r|#"->"#|show n2|#"")
  show (Relationship n r n2 None) = (""#|show n|#"-"#|show r|#"-"#|show n2|#"")
  show (Node n) = show n


data PathEntity a = PathEntity
  { _var :: Maybe T.Text
  , _labels :: [T.Text]
  } | EmptyPathEntity deriving Eq

instance Show (PathEntity N) where
  show pe@(PathEntity _ _) =showPathEntity "(" ")" pe
  show EmptyPathEntity = "()"

instance Show (PathEntity R) where
  show pe@(PathEntity _ _) = showPathEntity "[" "]" pe
  show EmptyPathEntity = "[]"

showPathEntity :: T.Text -> T.Text -> PathEntity a -> String
showPathEntity lbrac rbrac (PathEntity maybeVar labels) =
  let lbls = foldr (\lbl acc -> acc <> ":" <> lbl ) "" labels
      pathEntity = maybe lbls (\v -> lbrac <> v <> lbls <> rbrac) maybeVar
  in T.unpack pathEntity

node :: T.Text -> [T.Text] -> PathEntity N
node var lbls = PathEntity (Just var) lbls

relEntity :: T.Text -> [T.Text] -> PathEntity R
relEntity var lbls = PathEntity (Just var) lbls

rel :: PathEntity R -> PathEntity N -> Maybe (PathEntity R, PathEntity N)
rel r n = Just (r,n)

match :: PathComponent -> State (M.Map T.Text T.Text) PathComponent
match pc@(Relationship lPc r rPc _) = do
  match lPc
  isUniqueVar (_var r)
  match rPc
  pure pc
match pc@(Node n) = isUniqueVar (_var n) >> pure pc

isUniqueVar :: Maybe T.Text -> State (M.Map T.Text T.Text) ()
isUniqueVar maybeVar = case maybeVar of
  Just v -> do
    varMap <- get
    case M.lookup v varMap of
      Nothing -> do
        modify (\m -> M.insert v "" m)
      Just val -> error  (show v ++ show varMap)
  Nothing -> pure ()
