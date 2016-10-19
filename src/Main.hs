module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (minimumBy, sort, transpose)
import Data.Ord (comparing)

import Debug.Trace

-- A `Point` is a just a `List` of `Double` entries (a Euclidean vector):
type Point = [Double]

-- Computes the Euclidean distance between two points `a` and `b`.
dist :: Point -> Point -> Double
dist a b =
  sqrt
  $ sum
  $ zipWith (\p q -> let x = p - q in x * x) a b

-- Returns a `Map`, which assigns each point from `points` to the closest
-- centroid (taken from `centroids`).
assign :: [Point] -> [Point] -> Map Point [Point]
assign centroids points =
  Map.fromListWith (++) [ (center p, [p])
                        | p <- points
                        ]
  where
    center p =
      minimumBy (comparing (dist p)) centroids

-- Replaces the centroid (key) in `centroidsMap` with the centroids
-- computed from the points (value) in `centroidsMap`, thus returning.
relocate :: Map Point [Point] -> Map Point [Point]
relocate centroidsMap =
  Map.fromList [ (center, points)
               | (_, points) <- Map.toList centroidsMap
               , let n      = fromIntegral (length points)
                     center = map (/ n)
                              $ foldl1 (zipWith (+)) points
               ]

-- Performs the k-means algorithm
kmeans :: [Point] -> [Point] -> [Point]
kmeans centroids0 points0 = go 5 centroids0 points0
  where
    go 0 centroids _ = centroids
    go i centroids points =
      let
        centroidMap = relocate $ assign centroids points
        centroids'  = Map.keys centroidMap
        points'     = mconcat $ Map.elems centroidMap
      in go (i - 1) centroids' points'

main :: IO ()
main = do
  let points = [ [0, 0], [1, 0], [0,1], [1,1]
               , [7, 5], [9, 6], [8, 7] ]
  let centroids = kmeans (take 2 points) points
  print centroids
