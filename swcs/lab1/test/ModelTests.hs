module ModelTests where

import Protolude

import Test.Hspec
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import IGE.Types
import Model
import Algo
import Gant
import Types

mkGraph :: [G.LNode Weight] -> [G.LEdge Weight] -> Gr Weight Weight
mkGraph = G.mkGraph

modelTests = do
  let mp = ModelParams (G.mkGraph [(1, Weight 5)] [])
                       (G.mkGraph [(1, Weight 1)] [])
                       (CPUParams 1 True True 0)

  let
    mp2 = ModelParams
      (G.mkGraph [(1, Weight 5), (2, Weight 8)] [(1, 2, Weight 4)])
      (G.mkGraph [(1, Weight 1)] [])
      (CPUParams 1 True True 0)
    mp2FRes = ModelState
      (M.fromList [(1, 1)])
      [2]
      (M.singleton 1 (S.fromList [1]))
      (GantDiagram $ M.singleton 1 ([TimeRecord 0 5 1], []))
    mp3_4 = ModelParams
      ( G.mkGraph [(1, Weight 10), (2, Weight 9), (3, Weight 12)]
                  [(1, 2, Weight 14), (3, 1, Weight 10), (3, 2, Weight 7)]
      )
      ( G.mkGraph
        [(1, Weight 13), (2, Weight 5), (3, Weight 13), (4, Weight 5)]
        [ (1, 2, Weight 11)
        , (2, 1, Weight 11)
        , (1, 3, Weight 13)
        , (3, 1, Weight 13)
        , (1, 4, Weight 12)
        , (4, 1, Weight 12)
        ]
      )
      (CPUParams 1 True True 0)
    mp3_4FRes = ModelState
      (M.fromList [(3, 1)])
      [1, 2]
      ( M.fromList
        [(1, S.singleton 3), (3, S.empty), (2, S.empty), (4, S.empty)]
      )
      ( GantDiagram $ M.fromList
        [ (1, ([TimeRecord 0 12 3], []))
        , (2, ([], []))
        , (3, ([], []))
        , (4, ([], []))
        ]
      )
    mp3_4ORes = ModelState
      (M.fromList [(3, 1), (1, 2)])
      [2]
      ( M.fromList
        [(1, S.singleton 3), (2, S.singleton 1), (3, S.empty), (4, S.empty)]
      )
      ( GantDiagram $ M.fromList
        [ (1, ([TimeRecord 0 12 3], [TimeRecord 12 22 $ Transfer 1 2 3]))
        , (2, ([TimeRecord 22 32 1], [TimeRecord 12 22 $ Transfer 1 2 3]))
        , (3, ([], []))
        , (4, ([], []))
        ]
      )
  describe "first tasks matching" $ do
    it "empty task queue" $ firstTasksMatching mp [] `shouldBe` ModelState
      M.empty
      []
      (M.singleton 1 S.empty)
      (GantDiagram $ M.singleton 1 ([], []))
    it "one task queue" $ firstTasksMatching mp [1] `shouldBe` ModelState
      (M.singleton 1 1)
      []
      (M.singleton 1 (S.singleton 1))
      (GantDiagram $ M.singleton 1 ([TimeRecord 0 5 1], []))
    it "two task, one cpu" $ firstTasksMatching mp2 [1, 2] `shouldBe` mp2FRes
    it "three tasks, four cpus"
      $          firstTasksMatching mp3_4 [3, 1, 2]
      `shouldBe` mp3_4FRes


  describe "closest matching" $ do
    it "empty task queue"
      $          closestMatching mp []
      `shouldBe` (GantDiagram $ M.singleton 1 ([], []))
    it "one task queue"
      $          closestMatching mp [1]
      `shouldBe` (GantDiagram $ M.singleton 1 ([TimeRecord 0 5 1], []))
    it "two task, one cpu"
      $          closestMatching mp2 [1, 2]
      `shouldBe` ( GantDiagram
                 $ M.singleton 1 ([TimeRecord 0 5 1, TimeRecord 5 13 2], [])
                 )
    it "three tasks, 4 cpus"
      $          closestMatching mp3_4 [3, 1, 2]
      `shouldBe` ( GantDiagram $ M.fromList
                   [ ( 1
                     , ( [TimeRecord 0 12 3]
                       , [ TimeRecord 12 22 $ Transfer 1 2 3
                         , TimeRecord 22 29 $ Transfer 1 3 3
                         , TimeRecord 32 46 $ Transfer 2 1 1
                         , TimeRecord 46 60 $ Transfer 1 3 1
                         ]
                       )
                     )
                   , ( 2
                     , ( [TimeRecord 22 32 1]
                       , [ TimeRecord 12 22 $ Transfer 1 2 3
                         , TimeRecord 32 46 $ Transfer 2 1 1
                         ]
                       )
                     )
                   , ( 3
                     , ( [TimeRecord 60 69 2]
                       , [ TimeRecord 22 29 $ Transfer 1 3 3
                         , TimeRecord 46 60 $ Transfer 1 3 1
                         ]
                       )
                     )
                   , (4, ([], []))
                   ]
                 )
    it "two transfers in the same time"
      $          closestMatching
                   ( ModelParams
                     ( mkGraph [(2, Weight 1), (3, Weight 3), (4, Weight 3)]
                               [(3, 4, Weight 2), (2, 4, Weight 4)]
                     )
                     ( mkGraph
                       [(1, Weight 4), (2, Weight 2), (3, Weight 2), (4, Weight 1)]
                       [ (1, 2, Weight 1)
                       , (2, 1, Weight 1)
                       , (1, 3, Weight 1)
                       , (3, 1, Weight 1)
                       , (2, 3, Weight 2)
                       , (3, 2, Weight 2)
                       , (2, 4, Weight 4)
                       , (4, 2, Weight 4)
                       , (3, 4, Weight 1)
                       , (4, 3, Weight 1)
                       ]
                     )
                     (CPUParams 1 True True 0)
                   )
                   [3, 2, 4]
      `shouldBe` ( GantDiagram $ M.fromList
                   [ ( 1
                     , ( [TimeRecord 7 10 4]
                       , [ TimeRecord 1 5 $ Transfer 2 1 2
                         , TimeRecord 5 7 $ Transfer 3 1 3
                         ]
                       )
                     )
                   , ( 2
                     , ([TimeRecord 0 1 2], [TimeRecord 1 5 $ Transfer 2 1 2])
                     )
                   , ( 3
                     , ([TimeRecord 0 3 3], [TimeRecord 5 7 $ Transfer 3 1 3])
                     )
                   , (4, ([], []))
                   ]
                 )

  describe "otherTaskMatching" $ do
    it "mp2 other task" $ otherTaskMatching mp2 mp2FRes `shouldBe` ModelState
      (M.fromList [(1, 1), (2, 1)])
      []
      (M.singleton 1 (S.fromList [1, 2]))
      (GantDiagram $ M.singleton 1 ([TimeRecord 0 5 1, TimeRecord 5 13 2], []))
    it "mp3_4 other task"
      $          otherTaskMatching mp3_4 mp3_4FRes
      `shouldBe` mp3_4ORes
--    it "long distanse transfer" $ otherTaskMatching
--      mp3_4
--      ( ModelState
--        (M.fromList [(3, 1), (1, 2)])
--        [2]
--        ( M.fromList
--          [(1, S.singleton 3), (2, S.singleton 1), (3, S.empty), (4, S.empty)]
--        )
--        (GantDiagram $ M.fromList [(1,([TimeRecord  0 12 3],[TimeRecord  12 22 $Transfer  1 2 3,TimeRecord  46 60 $Transfer  1 3 1])),(2,([TimeRecord  22 32 1],[TimeRecord  12 22$ Transfer  1 2 3])),(3,([],[])),(4,([],[]))]      ))`shouldBe`

  describe "addTask" $ do
    it "addTask to one task timeline"
      $ addTask (CPUParams 1 True True 0) (0, 8, 2) ([TimeRecord 0 5 1], [])
      `shouldBe` ([TimeRecord 0 5 1, TimeRecord 5 13 2], [])
    it "addTask to one transfer timeline"
      $          addTask (CPUParams 1 True True 0)
                         (22, 10, 1)
                         ([], [TimeRecord 12 22 $ Transfer 1 2 3])
      `shouldBe` ([TimeRecord 22 32 1], [TimeRecord 12 22 $ Transfer 1 2 3])

  describe "findDataPaths" $ do
    it "one dependency"
      $          findDataPaths
                   ( mkGraph [(1, Weight 1), (2, Weight 2)]
                             [(1, 2, Weight 3), (2, 1, Weight 3)]
                   )
                   (M.fromList [(1, S.singleton 1), (2, S.empty)])
                   2
                   [1]
      `shouldBe` [(1, [1, 2])]
    it "long distance tranfer"
      $          findDataPaths
                   (sysGr mp3_4)
                   ( M.fromList
                     [(1, S.singleton 3), (2, S.singleton 1), (3, S.empty), (4, S.empty)]
                   )
                   3
                   [1, 3]
      `shouldBe` [(1, [2, 1, 3]), (3, [1, 3])]

  describe "addTransfer" $ do
    it "one transfer"
      $          addTransfer
                   (CPUParams 1 True True 0)
                   10
                   (Transfer 1 2 1)
                   ( GantDiagram
                     $ M.fromList [(1, ([TimeRecord 0 5 1], [])), (2, ([], []))]
                   , 5
                   )
      `shouldBe` ( GantDiagram $ M.fromList
                   [ ( 1
                     , ([TimeRecord 0 5 1], [TimeRecord 5 15 $ Transfer 1 2 1])
                     )
                   , (2, ([], [TimeRecord 5 15 (Transfer 1 2 1)]))
                   ]
                 , 15
                 )
    it "transfer upon transfer with 1 phys connection"
      $          addTransfer
                   (CPUParams 1 True True 0)
                   7
                   (Transfer 1 3 1)
                   ( GantDiagram $ M.fromList
                     [ (1, ([TimeRecord 0 5 1], [TimeRecord 5 8 $ Transfer 1 2 1]))
                     , (2, ([], [TimeRecord 5 8 $ Transfer 1 2 1]))
                     , (3, ([], []))
                     ]
                   , 5
                   )
      `shouldBe` ( GantDiagram $ M.fromList
                   [ ( 1
                     , ( [TimeRecord 0 5 1]
                       , [ TimeRecord 5 8 $ Transfer 1 2 1
                         , TimeRecord 8 15 $ Transfer 1 3 1
                         ]
                       )
                     )
                   , (2, ([], [TimeRecord 5 8 $ Transfer 1 2 1]))
                   , (3, ([], [TimeRecord 8 15 $ Transfer 1 3 1]))
                   ]
                 , 15
                 )
