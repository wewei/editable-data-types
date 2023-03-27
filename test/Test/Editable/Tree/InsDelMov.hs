module Test.Editable.Tree.InsDelMov where
import Test.Hspec (describe, it, shouldBe, SpecWith)
import Data.Tree (unfoldTree, Tree (Node), drawTree)
import Editable.Core ((~), Invertable (invert))
import Editable.Tree.InsDelMov (InsDelMov(..), splitTree, adjustDeletion, TreeIx (TreeIx))
import Data.Functor ((<&>))

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
        describe "as Editable" $ do
            let base = unfoldTree (\i -> (i, [i + 1..3])) (0::Int)
            describe "Insert" $ do
                it "should insert the subtree at the given location" $ do
                    Just base ~ insert [0] (Node 4 []) `shouldBe` Just
                        ( Node 0
                            [ Node 4 []
                            , Node 1
                                [ Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
                    Just base ~ insert [0, 1] (Node 4 [Node 5 []]) `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []]
                                , Node 4 [Node 5 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
                    Just base ~ insert [2, 0] (Node 4 [Node 5 []]) `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3
                                [ Node 4
                                    [ Node 5 [] ] ] ] )
                it "should fail if the location is illegal" $ do
                    Just base ~ insert [] (Node 4 []) `shouldBe` Nothing
                    Just base ~ insert [-1] (Node 4 []) `shouldBe` Nothing
                    Just base ~ insert [0, 3] (Node 4 []) `shouldBe` Nothing
                    Just base ~ insert [3, 0] (Node 4 []) `shouldBe` Nothing

            describe "Delete" $ do
                it "should delete the subtree from the given location" $ do
                    Just base ~ delete [0] (Node 1 [ Node 2 [Node 3 []], Node 3 []]) `shouldBe` Just
                        ( Node 0
                            [ Node 2 [Node 3 []]
                            , Node 3 [] ] )
                    Just base ~ delete [0, 1] (Node 3 []) `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
                it "should fail if the subtree does not match" $ do
                    Just base ~ delete [0, 1] (Node 4 []) `shouldBe` Nothing
                    Just base ~ delete [0, 1] (Node 3 [ Node 4 [] ]) `shouldBe` Nothing
                    Just base ~ delete [0] (Node 1 [ Node 2 [Node 3 []], Node 4 []]) `shouldBe` Nothing
                it "should fail if the location is illegal" $ do
                    Just base ~ delete [] (Node 4 []) `shouldBe` Nothing
                    Just base ~ delete [-1] (Node 4 []) `shouldBe` Nothing
                    Just base ~ delete [0, 2] (Node 4 []) `shouldBe` Nothing
                    Just base ~ delete [2, 0] (Node 4 []) `shouldBe` Nothing

            describe "Move" $ do
                it "should move the subtree to the given location" $ do
                    Just base ~ move [2] [0] `shouldBe` Just
                        ( Node 0
                            [ Node 3 []
                            , Node 1
                                [ Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []] ] )
                    Just base ~ move [1] [0, 1] `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []]
                                , Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 3 [] ] )
                    Just base ~ move [0, 0, 0] [0, 1] `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 []
                                , Node 3 []
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
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
                    Just base ~ nochange `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
        describe "As Invertable" $ do
            it "should invert the operations correctly" $ do
                invert nochange `shouldBe` NoChange
                invert (insert [0, 1] (Node 3 [])) `shouldBe` delete [0, 1] (Node 3 [])
                invert (delete [0, 1] (Node 3 [])) `shouldBe` insert [0, 1] (Node 3 [])
                invert (move [0, 1] [2, 3]) `shouldBe` move [2, 3] [0, 1]