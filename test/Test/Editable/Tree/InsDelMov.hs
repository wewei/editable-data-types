{-# LANGUAGE QuasiQuotes #-}
module Test.Editable.Tree.InsDelMov where
import Test.Hspec (describe, it, shouldBe, SpecWith)
import Data.Tree (unfoldTree, Tree, drawTree)
import Editable.Core ((~), Invertable (invert))
import Editable.Tree.InsDelMov (InsDelMov(..), splitTree)
import Data.Functor ((<&>), void)
import Editable.Tree.TreeIx (TreeIx(..), diff)
import Test.Editable.Tree.TreeQQ (tree)

printTree :: Show a => Tree a -> IO ()
printTree t = putStrLn $ drawTree $ show <$> t

insert :: [Int] -> Tree Int -> InsDelMov Int
insert = Insert . TreeIx

delete :: [Int] -> Tree Int -> InsDelMov Int
delete = Delete . TreeIx

move :: [Int] -> [Int] -> InsDelMov Int
move ns ms = Move (TreeIx ns) (TreeIx ms)

nochange :: InsDelMov Int
nochange = NoChange

testSuite :: SpecWith ()
testSuite =
    describe "InsDelMov" $ do
        describe "as Editable TreeIx" $ do
            describe "Insert" $ do
                it "should adjust the TreeIx according to the insertion" $ do
                    Just (TreeIx [0, 1]) ~ insert [1] [tree|(4)|] `shouldBe` Just (TreeIx [0, 1])
                    Just (TreeIx [0, 1]) ~ insert [0, 0, 1] [tree|(4)|] `shouldBe` Just (TreeIx [0, 1])
                    Just (TreeIx [0, 1]) ~ insert [0, 1] [tree|(4)|] `shouldBe` Just (TreeIx [0, 2])
                    Just (TreeIx [0, 1]) ~ insert [0] [tree|(4)|] `shouldBe` Just (TreeIx [1, 1])
                it "should fail if the location is illegal" $ do
                    Just (TreeIx [0, 1]) ~ insert [] [tree|(4)|] `shouldBe` Nothing
                    Just (TreeIx [0, 1]) ~ insert [-1] [tree|(4)|] `shouldBe` Nothing

            describe "Delete" $ do
                it "should adjust the TreeIx according to the deletion" $ do
                    Just (TreeIx [0, 1]) ~ delete [1] [tree|(4)|] `shouldBe` Just (TreeIx [0, 1])
                    Just (TreeIx [0, 1]) ~ delete [0, 0, 1] [tree|(4)|] `shouldBe` Just (TreeIx [0, 1])
                    Just (TreeIx [0, 1]) ~ delete [0, 1, 2] [tree|(4)|] `shouldBe` Just (TreeIx [0, 1])
                    Just (TreeIx [0, 1]) ~ delete [0, 0] [tree|(4)|] `shouldBe` Just (TreeIx [0, 0])
                    Just (TreeIx [0, 1]) ~ delete [0, 1] [tree|(4)|] `shouldBe` Just (TreeIx [])
                    Just (TreeIx [0, 1]) ~ delete [0] [tree|(4)|] `shouldBe` Just (TreeIx [])
                    Just (TreeIx [1, 1]) ~ delete [0] [tree|(4)|] `shouldBe` Just (TreeIx [0, 1])
                it "should fail if the location is illegal" $ do
                    Just (TreeIx [0, 1]) ~ delete [] [tree|(4)|] `shouldBe` Nothing
                    Just (TreeIx [0, 1]) ~ delete [-1] [tree|(4)|] `shouldBe` Nothing

            describe "Move" $ do
                it "should adjust the TreeIx according to the moving" $ do
                    Just (TreeIx [0, 1]) ~ move [1] [2] `shouldBe` Just (TreeIx [0, 1])
                    Just (TreeIx [0, 1]) ~ move [0, 0, 1] [0, 2] `shouldBe` Just (TreeIx [0, 1])
                    Just (TreeIx [0, 1]) ~ move [0, 0] [0, 1] `shouldBe` Just (TreeIx [0, 1])
                    Just (TreeIx [0, 1]) ~ move [0, 0] [0, 2] `shouldBe` Just (TreeIx [0, 0])
                    Just (TreeIx [0, 1]) ~ move [0, 2] [0, 1] `shouldBe` Just (TreeIx [0, 2])
                    Just (TreeIx [0, 1]) ~ move [0, 2] [0] `shouldBe` Just (TreeIx [1, 1])
                    Just (TreeIx [0, 1]) ~ move [0, 1] [0] `shouldBe` Just (TreeIx [0])
                it "should fail if the location is illegal" $ do
                    Just (TreeIx [0, 1]) ~ move [] [1] `shouldBe` Nothing
                    Just (TreeIx [0, 1]) ~ move [1] [] `shouldBe` Nothing
                    Just (TreeIx [0, 1]) ~ move [-1] [0] `shouldBe` Nothing
                    Just (TreeIx [0, 1]) ~ move [0] [-1] `shouldBe` Nothing
                    Just (TreeIx [0, 1]) ~ move [0] [0, 1] `shouldBe` Nothing



        describe "as Editable (Tree a)" $ do
            let base = [tree|(0, (1, (2, (3)), (3)), (2, (3)), (3))|] :: Tree Int
            describe "Insert" $ do
                it "should insert the subtree at the given location" $ do
                    Just base ~ insert [0] [tree|(4)|] `shouldBe`
                        Just [tree|(0, (4), (1, (2, (3)), (3)), (2, (3)), (3))|]
                    Just base ~ insert [0, 1] [tree|(4, (5))|] `shouldBe`
                        Just [tree|(0, (1, (2, (3)), (4, (5)), (3)), (2, (3)), (3))|]
                    Just base ~ insert [2, 0] [tree|(4, (5))|] `shouldBe`
                        Just [tree|(0, (1, (2, (3)), (3)), (2, (3)), (3, (4, (5))))|]
                        
                it "should fail if the location is illegal" $ do
                    Just base ~ insert [] [tree|(4)|] `shouldBe` Nothing
                    Just base ~ insert [-1] [tree|(4)|] `shouldBe` Nothing
                    Just base ~ insert [0, 3] [tree|(4)|] `shouldBe` Nothing
                    Just base ~ insert [3, 0] [tree|(4)|] `shouldBe` Nothing

            describe "Delete" $ do
                it "should delete the subtree from the given location" $ do
                    Just base ~ delete [0] [tree|(1, (2, (3)), (3))|] `shouldBe`
                        Just [tree|(0, (2, (3)), (3))|]
                    Just base ~ delete [0, 1] [tree|(3)|] `shouldBe`
                        Just [tree|(0, (1, (2, (3))), (2, (3)), (3))|]
                it "should fail if the subtree does not match" $ do
                    Just base ~ delete [0, 1] [tree|(4)|] `shouldBe` Nothing
                    Just base ~ delete [0, 1] [tree|(3, (4))|] `shouldBe` Nothing
                    Just base ~ delete [0] [tree|(1, (2, (3)), (4))|]  `shouldBe` Nothing
                it "should fail if the location is illegal" $ do
                    Just base ~ delete [] [tree|(4)|] `shouldBe` Nothing
                    Just base ~ delete [-1] [tree|(4)|] `shouldBe` Nothing
                    Just base ~ delete [0, 2] [tree|(4)|] `shouldBe` Nothing
                    Just base ~ delete [2, 0] [tree|(4)|] `shouldBe` Nothing

            describe "Move" $ do
                it "should move the subtree to the given location" $ do
                    Just base ~ move [2] [0] `shouldBe`
                        Just [tree|(0, (3), (1, (2, (3)), (3)), (2, (3)))|]
                        
                    Just base ~ move [1] [0, 1] `shouldBe`
                        Just [tree|(0, (1, (2, (3)), (2, (3)), (3)), (3))|]
                        
                    Just base ~ move [0, 0, 0] [0, 1] `shouldBe`
                        Just [tree|(0, (1, (2), (3), (3)), (2, (3)), (3))|]
                        
                it "should fail if the moving forms a cycle" $ do
                    Just base ~ move [0, 1] [0, 1, 0] `shouldBe` Nothing
                    Just base ~ move [0] [0, 0, 2] `shouldBe` Nothing
                it "should fail if the location is illegal" $ do
                    Just base ~ move [-1] [0] `shouldBe` Nothing
                    Just base ~ move [0] [-1] `shouldBe` Nothing
                    Just base ~ move [0] [2, 1] `shouldBe` Nothing
                    Just base ~ move [2, 0] [0] `shouldBe` Nothing

            describe "NoChange" $ do
                it "should keep the tree unchanged" $ do
                    Just base ~ nochange `shouldBe`
                        Just [tree|(0, (1, (2, (3)), (3)), (2, (3)), (3))|]
                        
        describe "As Invertable" $ do
            it "should invert the operations correctly" $ do
                invert nochange `shouldBe` NoChange
                invert (insert [0, 1] [tree|(3)|]) `shouldBe` delete [0, 1] [tree|(3)|]
                invert (delete [0, 1] [tree|(3)|]) `shouldBe` insert [0, 1] [tree|(3)|]
                invert (move [0, 1] [2, 3]) `shouldBe` move [2, 3] [0, 1]