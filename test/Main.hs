module Main where

import Test.Hspec ( hspec, describe, it, shouldBe )
import qualified Test.Editable.List.Replace
import qualified Test.Editable.List.Util
import qualified Test.Editable.List.InsDel


main :: IO ()
main = hspec $ do
    Test.Editable.List.Util.testSuite
    Test.Editable.List.Replace.testSuite
    Test.Editable.List.InsDel.testSuite

