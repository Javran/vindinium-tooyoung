{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Vindinium.Board.ShortestPath where

import Vindinium.Types
import qualified Data.Array as Arr
import qualified Data.Array.MArray as Arr
import qualified Data.Array.ST as Arr
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Control.Monad
import Control.DeepSeq
import GHC.Generics

import qualified Data.HashPSQ as PSQ

data PathInfo = PathInfo
  { piFrom :: [Dir] -- INVARIANT: always non-empty
  , piDist :: Int
  } deriving (Show, Generic)

instance NFData PathInfo

type ShortestPathInfo = Arr.Array Coord (Maybe PathInfo)

calcShortestPathInfo :: Board -> Coord -> ShortestPathInfo
calcShortestPathInfo b@(Board _ mat) src =
    mapToArray (Arr.bounds mat) Nothing (findShortestPaths b src)

findPathTo :: ShortestPathInfo -> Coord -> Maybe [Dir]
findPathTo spi = (reverse <$>) . findPathTo'
  where
    findPathTo' :: Coord -> Maybe [Dir]
    findPathTo' tgt@(x,y) = do
        (PathInfo (prevDir:_) _) <- unsafeIndex spi tgt
        case prevDir of
            -- only one place where Stay be, which is the source pos
            Stay -> pure []
            _ ->
                let newTgt = case prevDir of
                        North -> (x+1,y)
                        South -> (x-1,y)
                        West -> (x,y+1)
                        East -> (x,y-1)
                        _ -> error "impossible"
                in (prevDir :) <$> findPathTo' newTgt

findShortestPaths :: Board -> Coord -> Map.Map Coord (Maybe PathInfo)
findShortestPaths (b@(Board sz mat)) src = findShortestPaths' initQueue Map.empty
  where
    inf = sz * sz * 2
    expand :: Coord -> [(Coord, Dir)]
    expand (x,y)
        | not (shouldExpand (x,y)) = []
        | otherwise =
            [ ((x,y-1), West)
            , ((x,y+1), East)
            , ((x-1,y), North)
            , ((x+1,y), South) ]
      where
        -- tells whether we want to expand this node,
        -- as the hero might be standing on top of a tavern / a mine
        -- to pretend that he has reached the destination,
        -- in this case we should not expand further.
        shouldExpand :: Coord -> Bool
        shouldExpand c = case atCoord b c of
            FreeTile -> True
            -- other heros are ignored for now
            HeroTile _ -> True
            _ -> False
    -- INVARIANT: source should be the only place where dir == Stay
    initQueue = PSQ.insert src 0 (Just (PathInfo [Stay] 0))
                $ PSQ.fromList (mapMaybe tr (Arr.assocs mat))
      where
        tr (_,WoodTile) = Nothing
        tr (coord,_) = Just (coord,inf,Nothing)
    findShortestPaths' :: PSQ.HashPSQ Coord Int (Maybe PathInfo)
                       -> Map.Map Coord (Maybe PathInfo)
                       -> Map.Map Coord (Maybe PathInfo)
    findShortestPaths' q visited = case PSQ.minView q of
        Nothing -> visited
        -- even the smallest point is unreachable, stop searching
        Just (_, d, _, _) | d >= inf -> visited
        -- TODO: reaching a target, but should not expaned further
        -- Just (coord,_,_,newQ) | not (shouldExpand b coord) ->
        --   let newVisited = Map.insert coord (Just (PathInfo _ _)) visited
        -- in findShortestPaths' newQ newVisited

        Just (coord,dist,v,newQ) ->
            let newVisited = Map.insert coord v visited
                -- update pqueue
                newQ2 = foldl' go newQ (expand coord)
                go curQueue (curCoord,curDir) =
                    snd (PSQ.alter modify curCoord curQueue)
                  where
                    modify Nothing = ((), Nothing)
                    modify (e@(Just (curDist,prevPI)))
                        | not (validTargetCoord b curCoord) = ((), e)
                        | dist+1 < curDist = ((), Just (dist+1, Just (PathInfo [curDir] (dist+1))))
                        | dist == curDist =
                            let Just (PathInfo dirs _) = prevPI
                            in ((), Just (dist, Just (PathInfo (curDir:dirs) dist)))
                        | otherwise = ((), e)
            in findShortestPaths' newQ2 newVisited

-- store coordinate-indexed map for efficient access
mapToArray :: NFData a => (Coord,Coord) -> a -> Map.Map Coord a -> Arr.Array Coord a
mapToArray bound dval mp  = Arr.runSTArray $ do
    ar <- Arr.newArray bound dval
    forM_ (Map.toList mp) $ \ (coord,v) ->
        v `deepseq` Arr.writeArray ar coord v
    pure ar
