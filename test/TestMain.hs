module Main where

import Test.Hspec ( hspec, describe, it, shouldBe )
import qualified Editable.List.TestReplace
import qualified Editable.List.TestUtil

main :: IO ()
main = hspec $ do
    Editable.List.TestUtil.testSuite
    Editable.List.TestReplace.testSuite
