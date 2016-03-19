{-# LANGUAGE TypeFamilies #-}
module Vindinium.Board.ShortestPath where

import Vindinium.Types
import qualified Data.Array as Arr
import qualified Data.Array.MArray as Arr
import qualified Data.Array.ST as Arr
import qualified Data.Map.Strict as Map
import Data.Function
import Control.Monad.ST
import Data.Ix
import Data.List
import Data.Maybe

import qualified Data.HashPSQ as PSQ

data PathInfo = PathInfo
  { piFrom :: Dir
  , piDist :: Int
  } deriving (Show)

findShortestPaths :: Board -> (Int, Int) -> Map.Map (Int,Int) (Maybe PathInfo)
findShortestPaths (b@(Board sz mat)) src = findShortestPaths' initQueue Map.empty
  where
    bound = Arr.bounds mat
    inf = sz * sz * 2
    expand :: (Int, Int) -> [( (Int,Int), Dir)]
    expand (x,y) = filter (\(c,_) -> inRange bound c && atCoord b c /= WoodTile)
                          [ ((x,y-1), West)
                          , ((x,y+1), East)
                          , ((x-1,y), North)
                          , ((x+1,y), South) ]
    initQueue = PSQ.insert src 0 (Just (PathInfo Stay 0))
                $ PSQ.fromList (mapMaybe tr (Arr.assocs mat))
      where
        tr (_,WoodTile) = Nothing
        tr (coord,_) = Just (coord,inf,Nothing)
    findShortestPaths' :: ( arr ~ Arr.STArray s (Int,Int) (Maybe PathInfo)
                            -- PSQ <coord> <distance> <path info>
                          , queue ~ PSQ.HashPSQ (Int,Int) Int (Maybe PathInfo)
                          )
                       => queue
                       -> Map.Map (Int,Int) (Maybe PathInfo)
                       -> Map.Map (Int,Int) (Maybe PathInfo)
    findShortestPaths' q visited = case PSQ.minView q of
        Nothing -> visited
        -- even the smallest point is unreachable, stop searching
        Just (_, d, _, _) | d >= inf -> visited
        Just (coord,dist,v,newQ) ->
            let newVisited = Map.insert coord v visited
                -- update pqueue
                newQ2 = foldl' go newQ (expand coord)
                go curQueue (curCoord,curDir) =
                    snd (PSQ.alter modify curCoord curQueue)
                  where
                    modify Nothing = ((), Nothing)
                    modify (e@(Just (curDist,_)))
                        | dist+1 < curDist = ((), Just (dist+1, Just (PathInfo curDir (dist+1))))
                        | otherwise = ((), e)
            in findShortestPaths' newQ2 newVisited

