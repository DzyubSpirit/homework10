module Model where

import Protolude
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Graph.Inductive.PatriciaTree
import Data.Maybe (fromJust)
import Data.List (partition, groupBy, delete, nub)
import Data.Function (on)

import IGE.Types

import Types
import Gant

data ModelParams = ModelParams
  { taskGr :: Gr Weight Weight
  , sysGr :: Gr Weight Weight
  , cpuParams :: CPUParams
  }

data ModelState = ModelState
  { _completed :: M.Map Task CPU
  , _remaining :: [Task]
  , _cpuData :: M.Map CPU (S.Set Task)
  , _gant :: GantDiagram
  } deriving (Eq, Show)

closestMatching :: ModelParams -> [Task] -> GantDiagram
closestMatching mp = _gant . otherTasksMatching mp . firstTasksMatching mp

firstTasksMatching :: ModelParams -> [Task] -> ModelState
firstTasksMatching mp@(ModelParams tGr sGr cp) tq
  | length ftq <= length cpuPriors = ModelState
    { _completed = M.fromList $ zip ftq cpuPriors
    , _remaining = otq
    , _cpuData   = M.fromList
      $ zip cpuPriors (map S.singleton ftq ++ repeat S.empty)
    , _gant      = GantDiagram $ M.union oneTaskPerCPU $ M.fromList
      [ (cpu, ([], [])) | cpu <- drop (length ftq) cpuPriors ]
    }
  | otherwise = ModelState
    { _completed = M.fromList $ taskCpuMatch ++ zip ftq cpuPriors
    , _remaining = otq
    , _cpuData   = foldl
        (\m (k, v) -> M.alter (Just . maybe (S.singleton v) (S.insert v)) k m)
        M.empty
      $  zip cpuPriors ftq
      ++ map swap      taskCpuMatch
    , _gant      = GantDiagram allTasksMatched
    }
 where
  (ftq, otq) = partition (null . G.inn tGr) tq
  cpuPriors  = snd <$> sortBy
    (flip compare)
    [ (G.deg sGr cpu, cpu) | (cpu, _) <- G.labNodes sGr ]
  oneTaskPerCPU = M.fromList $ zip cpuPriors $ fmap singleTaskTimeLine ftq

  (allTasksMatched, taskCpuMatch) =
    foldl addTask (oneTaskPerCPU, []) $ drop (length cpuPriors) ftq

  singleTaskTimeLine t = ([TimeRecord 0 et t], [])
    where et = fromIntegral $ fromJust $ G.lab tGr t

  addTask (cpus, taskCpuMatch) t =
    ( M.adjust (first (++ [TimeRecord bt et t])) cpu cpus
    , (t, cpu) : taskCpuMatch
    )
   where
    (bt, cpu) = minimum $ map swap $ M.toList $ M.map tasksEndTime cpus
    et        = bt + fromIntegral (fromJust $ G.lab tGr t)

otherTasksMatching :: ModelParams -> ModelState -> ModelState
otherTasksMatching mp =
  fromJust . head . dropWhile (not . null . _remaining) . iterate
    (otherTaskMatching mp)

otherTaskMatching :: ModelParams -> ModelState -> ModelState
otherTaskMatching _ ms@(ModelState _ [] _ _) = ms
otherTaskMatching (ModelParams tGr sGr cpup) (ModelState completed (task:tt) dat gant)
  = ModelState completed' tt dat' gant'
 where
  dat'       = M.adjust (S.insert task) cpu dat
  completed' = M.insert task cpu completed
  gant' =
    GantDiagram
      $ M.adjust
          (addTask cpup (mbt, weight $ fromJust $ G.lab tGr task, task))
          cpu
      $ cpus gantWithTransfers

  (gantWithTransfers, mbt) =
    foldl (\(g, mbt) tp -> let (g', et) = addPath g tp in (g', max mbt et))
          (gant, 0)
      $ filter ((> 1) . length . snd) paths
  addPath g (t, ps) =
    foldl (\ig (fr, to) -> addTransfer cpup td (Transfer fr to t) ig)
          (g, taskEnd t)
      $ zip ps (drop 1 ps)
   where
    td =
      weight
        $ G.edgeLabel
        $ fromJust
        $ head
        $ dropWhile ((/= task) . snd . G.toEdge)
        $ G.out tGr t

  cpu =
    fromJust
      $ head
      $ minimum
      $ map swap
      $ M.toList
      $ M.map
          ( map (\i -> iEnd i - iBegin i)
          . filter (not . null . payloads)
          . makeIntervals
          )
      $ cpus gant

  paths = findDataPaths sGr dat cpu (G.pre tGr task)
  taskEnd t =
    end
      $   fromJust
      $   head
      $   dropWhile ((/= t) . label)
      $   fst
      $   cpus gant
      M.! (completed M.! t)

findDataPaths
  :: G.Graph gr
  => gr Weight Weight
  -> M.Map CPU (S.Set Task)
  -> CPU
  -> [Task]
  -> [(Task, [CPU])]
findDataPaths sysGr cpuDat cpu tasks =
  snd $ snd $ fromJust $ head $ dropWhile (not . null . fst . snd) $ iterate
    nextStep
    ((M.singleton cpu [cpu], S.singleton cpu), (tasks, []))
 where
  nextStep st@((_, _), ([], _)) = st
  nextStep ((cpus, usedCpus), (ts, datCpuMatch)) =
    ( (cpus'           , S.union usedCpus (S.fromList $ M.keys cpus'))
    , (map fst notFound, map (second fromJust) found ++ datCpuMatch)
    )
   where
    (found, notFound) =
      partition (isJust . snd) [ (t, cpuWithData t) | t <- ts ]
    cpuWithData t =
      fmap (cpus M.!) $ head $ filter (S.member t . (cpuDat M.!)) $ M.keys cpus
    cpus' =
      M.fromList
        $ filter ((`S.notMember` usedCpus) . fst)
        $ map (fromJust . head)
        $ groupBy ((==) `on` fst)
        $ sort
        $ concatMap (\(c, p) -> [ (nc, nc : p) | nc <- G.suc sysGr c ])
        $ M.toList cpus

randomMatching :: ModelParams -> GantDiagram
randomMatching = undefined

tasksEndTime :: TimeLine -> Int
tasksEndTime tl = maybe 0 end $ head $ drop (length ts - 1) ts
  where ts = fst tl

recordInterval :: TimeRecord a -> (Int, Int)
recordInterval (TimeRecord b e _) = (b, e)

transferGaps :: [TimeRecord Transfer] -> [(Int, Int)]
transferGaps [] = [(0, maxBound)]
transferGaps ts = foldl addInterval (take 1 ts') $ drop 1 ts'
 where
  ts' = map recordInterval ts
  addInterval gps@((pb, pe):ogps) ngp@(nb, ne)
    | pe <= nb  = ngp : gps
    | otherwise = (pb, max pe ne) : ogps

addTask :: CPUParams -> (Int, Int, Task) -> TimeLine -> TimeLine
addTask (CPUParams _ inOut _ _) (mbt, d, n) tl =
  (bts ++ TimeRecord bt (bt + d) n : ats, snd tl)
 where
  taskIntervals = recordInterval <$> fst tl
  intervals | inOut     = taskIntervals
            | otherwise = sort $ taskIntervals ++ transferGaps (snd tl)
  gaps
    | null intervals = [(0, maxBound)]
    | otherwise = zip (map snd intervals)
                      (drop 1 (map fst intervals) ++ [maxBound])
  bt = max mbt $ fst $ fromJust $ head $ filter inGap gaps
  inGap (b, e) = let b' = max b mbt in e - b' >= d
  (bts, ats) = partition ((< bt) . begin) $ fst tl

data Interval = Interval
  { iBegin :: Int
  , iEnd :: Int
  , payloads :: [Either Task Transfer]
  } deriving (Eq, Show)

timePair :: Interval -> (Int, Int)
timePair (Interval b e _) = (b, e)

addTransToTimeLine
  :: (Int, Int, Transfer) -> [TimeRecord Transfer] -> [TimeRecord Transfer]
addTransToTimeLine (bt, d, tr) ts = bef ++ TimeRecord bt (bt + d) tr : aft
  where (bef, aft) = partition ((< bt) . begin) ts

addTransfer
  :: CPUParams -> Int -> Transfer -> (GantDiagram, Int) -> (GantDiagram, Int)
addTransfer cp@(CPUParams phNum inOut dup pl) d tr@(Transfer fr to task) (GantDiagram cpus, mbt)
  = (gant, bt + d)
 where
  gant =
    GantDiagram
      $ M.adjust (second $ addTransToTimeLine (bt, d, tr)) to
      $ M.adjust (second $ addTransToTimeLine (bt, d, tr)) fr cpus
  bt = max mbt $ fst $ fromJust $ head $ dropWhile
    (\(b, e) -> let b' = max b mbt in e - b' < d)
    mis
  mis = mergeIntervals fis tis
  fis = complianceIntervals (fr, True)
  tis = complianceIntervals (to, False)
  complianceIntervals (cpu, sent) = reverse $ foldl addInterval [] $ filter
    comply
    is
   where
    is     = filter ((> mbt) . iEnd) $ makeIntervals $ cpus M.! cpu
    comply = complyWithPayloads cp (fr, to) sent . payloads
  addInterval [] i = [timePair i]
  addInterval xx@((b, e):xs) i | e > iBegin i = (b, iEnd i) : xs
                               | otherwise    = (iBegin i, iEnd i) : xx
{-
  checkNext [] _  = Nothing
  checkNext _  [] = Nothing
  checkNext f@(fi:fis) t@(ti:tis)
    | all (transferIsOkay (bt, d)) tls   = bt
    | iBegin (fst fi) >= iBegin (fst ti) = checkNext f tis
    | otherwise                          = checkNext fis t
   where
    bt  = maximum [iBegin $ fst fi, iBegin $ fst ti, mbt]
    tls = if iBegin (fst fi) >= iBegin (fst ti) then [fis, t] else [f, tis]
-}

transferIsOkay :: (Int, Int) -> [(Interval, Bool)] -> Bool
transferIsOkay (bt, d) = all (\(i, _) -> iBegin i < bt + d && iEnd i > bt)

complyWithPayloads
  :: CPUParams -> (G.Node, G.Node) -> Bool -> [Either Task Transfer] -> Bool
complyWithPayloads (CPUParams phNum inOut dup _) e@(fr, to) sent ps
  | not inOut && any isLeft ps = False
  | any (\(oe, d) -> d && e == oe) uniqConns = False
  | not dup && elem (fr, to) (map fst uniqConns) = False
  | length (nub $ map fst uniqConns) == phNum = False
  | otherwise                  = True
 where
  uniqConns = mapMaybe (either (const Nothing) $ Just . toConn) ps
  toConn (Transfer f t _) | f <= t    = ((f, t), True)
                          | otherwise = ((t, f), False)

mergeIntervals :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mergeIntervals is1 is2 = mergeIntervals' is1 is2 []
 where
  mergeIntervals' [] x  res = res ++ x
  mergeIntervals' x  [] res = res ++ x
  mergeIntervals' xx@((xb, xe):xs) yy@((yb, ye):ys) res
    | xb >= ye  = mergeIntervals' xx ys res
    | yb >= xe  = mergeIntervals' xs yy res
    | xe < ye   = mergeIntervals' xs yy $ res ++ [(max xb yb, min xe ye)]
    | otherwise = mergeIntervals' xx ys $ res ++ [(max xb yb, min xe ye)]

makeIntervals :: TimeLine -> [Interval]
nameIntervals ([], []) = []
makeIntervals (tasks, transfers) = intervals
 where
  timeLabels = groupBy ((==) `on` fst) $ sort $ tas ++ trs
  intervals  = wrap $ snd $ foldl updPayloads ([], []) $ zipWith
    ( \tl1 tl2 ->
      ((fst (fromJust $ head tl1), fst (fromJust $ head tl2)), map snd tl1)
    )
    timeLabels
    (drop 1 timeLabels)

  wrap [] = [Interval 0 maxBound []]
  wrap xs =
    (if bt == 0 then [] else [Interval 0 bt []])
      ++ xs
      ++ [Interval et maxBound []]
   where
    bt = minimum $ iBegin <$> xs
    et = maximum $ iEnd <$> xs

  updPayloads (ps, intervals) ((b, e), lbls) =
    (ps', intervals ++ [Interval b e ps'])
    where ps' = foldl updLbl ps lbls

  updLbl ps (True , t) = t : ps
  updLbl ps (False, t) = delete t ps

  tas = concatMap
    (\(TimeRecord b e t) -> [(b, (True, Left t)), (e, (False, Left t))])
    tasks
  trs = concatMap
    (\(TimeRecord b e t) -> [(b, (True, Right t)), (e, (False, Right t))])
    transfers

transferTask :: CPUParams -> GantDiagram -> Int -> Transfer -> GantDiagram
transferTask (CPUParams phNum inOut dup pl) (GantDiagram cpus) mbt (Transfer fr to task)
  = undefined
--    where frInterfals = 
