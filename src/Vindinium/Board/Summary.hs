module Vindinium.Board.Summary where

import Vindinium.Types
import qualified Data.IntMap as IM
import qualified Data.Array as Arr
import Data.Foldable
import Data.Maybe

-- board preprocessing, summarize important info
-- that can be used by planners

data Summary = Summary
  { sSpawnPoints :: IM.IntMap Coord
  , sTaverns :: [Coord]
  , sMines :: [Coord]
  } deriving (Show, Eq)

summarize :: Board -> Summary
summarize (Board _ b) =
    foldl' collectInfo (Summary IM.empty [] []) (Arr.assocs b)
  where
    collectInfo s (c,t) = case t of
        HeroTile h -> s { sSpawnPoints = IM.insert h c (sSpawnPoints s) }
        TavernTile -> s { sTaverns = c : sTaverns s }
        MineTile _ -> s { sMines = c : sMines s }
        _ -> s

spawningPointOf :: Summary -> HeroId -> Coord
spawningPointOf s h = fromJust (IM.lookup h (sSpawnPoints s))
