module Gant where

import Protolude
import Data.Graph.Inductive.Graph
import qualified Data.Map.Strict as M
import Graphics.Rendering.Cairo

import IGE.Types
import IGE.Render

type Task = Int
type Transfer = (Int, Int)
type TimeLine = ([TimeRecord Task], [TimeRecord Transfer])

data TimeRecord a = TimeRecord
  { begin :: Int
  , end :: Int
  , label :: a
  }

newtype GantDiagram = GantDiagram
  {  cpus :: Map Node TimeLine
  }

padding = 30
arrowSize = 5
textPadding = 3

arrow :: (Double, Double) -> (Double, Double) -> Double -> [Text] -> Render ()
arrow (x1, y1) (x2, y2) size labels = do
  let n = length labels
  moveTo x1 y1
  lineTo x2 y2
  when (x1 == x2) $ do
    lineTo (x2 - size) (y2 + size)
    moveTo x2          y2
    lineTo (x2 + size) (y2 + size)
    for_ (zip [1 .. n] labels) $ \(i, l) -> do
      let y = y1 + (y2 - y1) * fromIntegral i / fromIntegral (n + 1)
      moveTo (x1 - arrowSize) y
      lineTo (x1 + arrowSize) y
      ex <- textExtents l
      moveTo (x1 - arrowSize - textExtentsWidth ex - textPadding)
             (y + (textExtentsHeight ex / 2))
      showText l
  when (y1 == y2) $ do
    lineTo (x2 - size) (y2 - size)
    moveTo x2          y2
    lineTo (x2 - size) (y2 + size)
    for_ (zip [1 .. n] labels) $ \(i, l) -> do
      let x = x1 + (x2 - x1) * fromIntegral i / fromIntegral (n + 1)
      moveTo x (y1 - arrowSize)
      lineTo x (y1 + arrowSize)
      ex <- textExtents l
      moveTo (x - (textExtentsWidth ex / 2))
             (y1 + textPadding + arrowSize + textExtentsHeight ex)
      showText l

instance DimsRenderable GantDiagram where
  dimsRender (wI, hI) (GantDiagram cpus) = do
    renderBackground
    fill
    let (w, h) = (fromIntegral wI, fromIntegral hI)
        nodeN = M.size cpus
        timeN = maximum $ (0:)
              $ concatMap (\(ts, tr) -> map end ts ++ map end tr)
              $ M.elems cpus
        (lx, uy, rx, dy) = (padding, padding, w-padding, h-padding)
        trXY (x,y) = (nx, ny)
          where nx = lx +(rx-lx)*x/(fromIntegral timeN+1)
                ny = dy +(uy-dy)*y/(fromIntegral nodeN+1)
        plotLine :: Double -> Double -> Double -> Text -> Render ()
        plotLine y x1 x2 l = do
          uncurry moveTo $ trXY (x1, y)
          uncurry lineTo $ trXY (x2, y)
          let (mx, my) = trXY (x1+ (x2-x1)/2, y)
          ext <- textExtents l
          moveTo (mx-(textExtentsWidth ext / 2)) (my-textPadding)
          showText l

    -- Y axis
    arrow (lx, dy) (lx, uy) arrowSize
          (show <$> M.keys cpus)
    -- X axis
    arrow (lx, dy) (rx, dy) arrowSize
          (map show [1..timeN])
    let tasks :: [(Double, TimeRecord Task)]
        tasks = concat $ zipWith (\i ts -> map ((,) i) ts) [1..]
              $ map fst $ M.elems cpus

        transfers :: [(Double, TimeRecord Transfer)]
        transfers = concat $ zipWith (\i ts -> map ((,) i) ts) [1..]
                  $ map snd $ M.elems cpus
    for_ tasks $ \(i, TimeRecord b e t) ->
      plotLine i (fromIntegral b) (fromIntegral e) (show t)
    for_ transfers $ \(i, TimeRecord b e (from, to)) ->
      plotLine (i-0.5) (fromIntegral b) (fromIntegral e)
               (show from<>" - "<> show to)
    stroke
