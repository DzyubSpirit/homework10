import Protolude
import Test.Hspec
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import System.Random
import qualified Data.Map.Strict as M

import IGE.Types

import Algo
import Gant
import Model
import Types

import Examples
import GeneratorTests
import ModelTests hiding (mkGraph)

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
  describe "randomQueue" $ do
    let [g1, g2, g3, g4] = map
          mkStdGen
          [ -8488355513137236356
          , 5869735224982706236
          , -7982683311953146195
          , -8152950985931255716
          ]
    it "empty graph" $ randomQueue g1 (mkGraph' [] []) `shouldBe` []
    it "one-node graph" $ randomQueue g2 (mkGraph' [(1, 2)] []) `shouldBe` [1]
    it "two-node graph"
      $          randomQueue g3 (mkGraph' [(1, 3), (2, 4)] [(1, 2, 5)])
      `shouldBe` [1, 2]
    it "test graph"
      $          length (randomQueue g4 testGr)
      `shouldBe` length testGrTaskQueue
  votes $ mkStdGen 5595375131091215088
  describe "makeIntervals" $ do
    it "empty timeline gives empty intervals"
      $          makeIntervals ([], [])
      `shouldBe` [Interval 0 maxBound []]
    it "one-element timeline gives one interval"
      $          makeIntervals ([TimeRecord 0 2 1], [])
      `shouldBe` [Interval 0 2 [Left 1], Interval 2 maxBound []]

    it "two separate elements timeline gives three intervals"
      $          makeIntervals ([TimeRecord 0 2 1, TimeRecord 3 4 2], [])
      `shouldBe` [ Interval 0 2        [Left 1]
                 , Interval 2 3        []
                 , Interval 3 4        [Left 2]
                 , Interval 4 maxBound []
                 ]

    it "two elements with the same border give three intervals"
      $ makeIntervals ([TimeRecord 0 2 1], [TimeRecord 0 1 (Transfer 1 2 1)])
      `shouldBe` [ Interval 0 1        [Right (Transfer 1 2 1), Left 1]
                 , Interval 1 2        [Left 1]
                 , Interval 2 maxBound []
                 ]
    it "two elements (big and small) give three intervals"

      $ makeIntervals ([TimeRecord 0 3 1], [TimeRecord 1 2 (Transfer 1 2 1)])
      `shouldBe` [ Interval 0 1        [Left 1]
                 , Interval 1 2        [Right (Transfer 1 2 1), Left 1]
                 , Interval 2 3        [Left 1]
                 , Interval 3 maxBound []
                 ]
  modelTests
