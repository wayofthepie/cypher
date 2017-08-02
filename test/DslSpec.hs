module DslSpec where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Cypher.Dsl

rightDash_construct :: IO ()
rightDash_construct = [1, 2, 3] `compare` [1,2,1111] @?= EQ
 where
   nodePattern = node
