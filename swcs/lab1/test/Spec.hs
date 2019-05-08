import Protolude
import Test.Hspec
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Map.Strict as M

import IGE.Types

import Algo

import Examples

mkGraph' :: [LNode Weight] -> [LEdge Weight] -> Gr Weight Weight
mkGraph' = mkGraph

main :: IO ()
main = hspec $ do
  describe "isAcyclic" $ do
    it "returns yes for empty graph"
      $          isAcyclic (mkGraph' [] [])
      `shouldBe` True
    it "returns yes for one-node graph"
      $          isAcyclic (mkGraph' [(1, 0)] [])
      `shouldBe` True
    it "returns no for three-node cycle"
      $          isAcyclic
                   (mkGraph' [(1, 0), (2, 0), (3, 0)] [(1, 2, 0), (2, 3, 0), (3, 1, 0)])
      `shouldBe` False
    it "returns no for three-node tree"
      $          isAcyclic
                   (mkGraph' [(1, 0), (2, 0), (3, 0)] [(1, 2, 0), (2, 3, 0), (1, 3, 0)])
      `shouldBe` True
    it "returns no for three-node-cycle-with-separate-node graph"
      $          isAcyclic
                   ( mkGraph' [(1, 0), (2, 0), (3, 0), (4, 0)]
                              [(1, 2, 0), (2, 3, 0), (3, 1, 0)]
                   )
      `shouldBe` False
    it "returns no for three-node-tree-and-separate-node"
      $          isAcyclic
                   ( mkGraph' [(1, 0), (2, 0), (3, 0), (4, 0)]
                              [(1, 2, 0), (2, 3, 0), (1, 3, 0)]
                   )
      `shouldBe` True
  describe "isConnected" $ do
    it "returns yes for empty graph"
      $          isConnected (mkGraph' [] [])
      `shouldBe` True
    it "returns yes for one-node graph"
      $          isConnected (mkGraph' [(1, 0)] [])
      `shouldBe` True
    it "returns no for two-node acyclic graph"
      $          isConnected (mkGraph' [(1, 0), (2, 0)] [(1, 2, 0)])
      `shouldBe` False
    it "returns yes for two-node cyclic graph"
      $          isConnected (mkGraph' [(1, 0), (2, 0)] [(1, 2, 0), (2, 1, 0)])
      `shouldBe` True
    it "returns no for two-node not-connected graph"
      $          isConnected (mkGraph' [(1, 0), (2, 0)] [])
      `shouldBe` False
    it "returns yes for three-node cyclic graph"
      $          isConnected
                   (mkGraph' [(1, 0), (2, 0), (3, 0)] [(1, 2, 0), (2, 3, 0), (3, 1, 0)])
      `shouldBe` True
    it "returns no for three-node not-cyclic graph"
      $          isConnected
                   (mkGraph' [(1, 0), (2, 0), (3, 0)] [(1, 2, 0), (2, 3, 0), (1, 3, 0)])
      `shouldBe` False
    it "returns no for four-node not-connected graph"
      $          isConnected
                   ( mkGraph' [(1, 0), (2, 0), (3, 0), (4, 0)]
                              [(1, 2, 0), (2, 3, 0), (3, 1, 0)]
                   )
      `shouldBe` False
  describe "criticalPaths" $ do
    it "empty graph" $ criticalPaths (mkGraph' [] []) `shouldBe` M.fromList []
    it "one-node graph"
      $          criticalPaths (mkGraph' [(1, 4)] [])
      `shouldBe` M.fromList [(1, (4, 1))]
    it "two-node graph"
      $          criticalPaths (mkGraph' [(1, 4), (2, 5)] [(1, 2, 10)])
      `shouldBe` M.fromList [(1, (9, 2)), (2, (5, 1))]
    it "two-level tree graph"
      $          criticalPaths
                   (mkGraph' [(1, 4), (2, 5), (3, 6)] [(1, 2, 10), (1, 3, 11)])
      `shouldBe` M.fromList [(1, (10, 2)), (2, (5, 1)), (3, (6, 1))]
    it "test graph" $ criticalPaths testGr `shouldBe` testGrCriticalPaths
  describe "taskQueue" $ do
    it "empty graph" $ taskQueue (mkGraph' [] []) `shouldBe` []
    it "one-node graph" $ taskQueue (mkGraph' [(1, 4)] []) `shouldBe` [1]
    it "two-node graph"
      $          taskQueue (mkGraph' [(1, 4), (2, 5)] [(1, 2, 3)])
      `shouldBe` [1, 2]
    it "three-node graph"
      $ taskQueue (mkGraph' [(1, 4), (2, 5), (3, 6)] [(1, 2, 3), (1, 3, 5)])
      `shouldBe` [1, 3, 2]
    it "test graph" $ taskQueue testGr `shouldBe` testGrTaskQueue
