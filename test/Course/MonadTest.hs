{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.MonadTest where

import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))

import           Course.Core
import           Course.ExactlyOne (ExactlyOne (..))
import           Course.List       (List (..))
import           Course.Monad      (join, (=<<), (>>=), (<=<))
import           Course.Optional   (Optional (..))

test_Monad :: TestTree
test_Monad =
  testGroup "Monad" [
    bindExactlyOneTest
  , bindListTest
  , bindOptionalTest
  , bindReaderTest
  , joinTest
  , bindFlippedTest
  , kleisliCompositionTest
  ]

bindExactlyOneTest :: TestTree
bindExactlyOneTest =
  testCase "(=<<) for ExactlyOne" $
    ((\x -> ExactlyOne(x+1)) =<< ExactlyOne 2) @?= ExactlyOne 3

bindListTest :: TestTree
bindListTest =
  testCase "(=<<) for List" $
    ((\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)) @?= (1:.1:.2:.2:.3:.3:.Nil)

bindOptionalTest :: TestTree
bindOptionalTest =
  testCase "(=<<) for Optional" $
    ((\n -> Full (n + n)) =<< Full 7) @?= Full 14

bindReaderTest :: TestTree
bindReaderTest =
  testCase "(=<<) for (->)" $
    ((*) =<< (+10)) 7 @?= 119

joinTest :: TestTree
joinTest =
  testGroup "join" [
    testCase "List" $
      join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil) @?= (1:.2:.3:.1:.2:.Nil)
  , testCase "Optional with Empty" $
      join (Full Empty) @?= (Empty :: Optional Integer)
  , testCase "Optional all Full" $
      join (Full (Full 7)) @?= Full 7
  , testCase "(->)" $
      join (+) 7 @?= 14
  ]

bindFlippedTest :: TestTree
bindFlippedTest =
  testCase "(>>=)" $
    ((+10) >>= (*)) 7 @?= 119

kleisliCompositionTest :: TestTree
kleisliCompositionTest =
  testCase "kleislyComposition" $
    ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1 @?= (2:.2:.3:.3:.Nil)
