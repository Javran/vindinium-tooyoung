{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Vindinium.Board where

import Data.Aeson
import Data.List.Split
import Control.Monad
import Data.Char
import qualified Data.Array.IArray as Arr
import qualified Data.Array.Base as Arr
import GHC.Generics
import Control.DeepSeq
import Data.Universe
import Data.Universe.Helpers

import qualified Data.Text as T

type HeroId = Int

data Tile
  = FreeTile
  | WoodTile
  | TavernTile
  | HeroTile HeroId
  | MineTile (Maybe HeroId)
    deriving (Show, Eq, Generic)

instance NFData Tile

data Dir = Stay | North | South | East | West
  deriving (Show, Eq, Read, Generic, Bounded, Enum)

instance Universe Dir where
    universe = universeDef

instance NFData Dir

reverseDir :: Dir -> Dir
reverseDir d = case d of
    North -> South
    South -> North
    West -> East
    East -> West
    Stay -> error "cannot reverse Stay"

applyDir :: Dir -> Coord -> Coord
applyDir d v@(x,y) = case d of
    Stay -> v
    North -> (x-1,y)
    South -> (x+1,y)
    West -> (x,y-1)
    East -> (x,y+1)

-- ignoring heros
applyDir' :: Board -> Dir -> Coord -> Maybe Coord
applyDir' bd@(Board _ mat) d c
    | not (Arr.inRange (Arr.bounds mat) c') = Nothing
    | otherwise = case atCoord bd c' of
        FreeTile -> Just c'
        WoodTile -> Nothing
        TavernTile -> Just c
        HeroTile _ -> Just c'
        MineTile _ -> Just c
  where
    c' = applyDir d c

data Board = Board
  { boardSize  :: ! Int
  , boardTiles :: ! (Arr.Array (Int,Int) Tile)
  } deriving (Show, Eq)

instance ToJSON Board where
    toJSON b  = object
      [ "size"  .= boardSize b
      , "tiles" .= printTiles (Arr.elems . boardTiles $ b)
      ]
instance FromJSON Board where
    parseJSON (Object o) = parseBoard <$> o .: "size" <*> o .: "tiles"
    parseJSON _ = mzero

type Coord = (Int, Int)

newtype Pos = Pos Coord
  deriving (Show, Eq)

mkPos :: Int -> Int -> Pos
mkPos a b = Pos (a,b)

instance FromJSON Pos where
    parseJSON (Object o) = mkPos <$> o .: "x" <*> o .: "y"
    parseJSON _ = mzero

printTiles :: [Tile] -> T.Text
printTiles = foldMap (T.pack . printTile)

printTile :: Tile -> String
printTile FreeTile = "  "
printTile WoodTile = "##"
printTile (HeroTile i) = "@" ++ show i
printTile TavernTile = "[]"
printTile (MineTile Nothing) = "$-"
printTile (MineTile (Just i)) = "$" ++ show i

-- whole game board is force because we don't need the laziness.
-- NOTE: make sure not to repeatly deepseq something, as it'll
-- traverse the whole structure every time doing so.
parseBoard :: Int -> String -> Board
parseBoard s raw = tileArr `deepseq` Board s tileArr
  where
    tileArr = Arr.array ((0,0),(s-1,s-1)) tilesWithInd
    tilesWithInd = zip [(x,y) | x <-[0..s-1], y <-[0..s-1]] (go raw)
    go (a:b:xs) = tr a b : go xs
    go [] = []
    go _ = error "wrong format, odd char count?"

    tr ' ' ' ' = FreeTile
    tr '#' '#' = WoodTile
    tr '[' ']' = TavernTile
    tr '@' b = HeroTile (ord b - ord '0')
    tr '$' b = MineTile (case b of
                           '-' -> Nothing
                           _ -> Just (ord b - ord '0'))
    tr _ _ = error "impossible"

pprBoard :: Board -> IO ()
pprBoard (Board s ts) = do
    let rows = chunksOf s (Arr.elems ts)
    putStrLn $ replicate 10 '=' ++ " Board"
    mapM_ (putStrLn . concatMap printTile) rows
    putStrLn ""

unsafeIndex :: Arr.Ix i => Arr.Array i a -> i -> a
unsafeIndex arr idx = Arr.unsafeAt arr (Arr.index (Arr.bounds arr) idx)

atCoord :: Board -> Coord -> Tile
atCoord (Board _ t) = unsafeIndex t

atPos :: Board -> Pos -> Tile
atPos b (Pos p) = atCoord b p

-- a valid target position is any non-WoodTile in range of the board
validTargetCoord :: Board -> Coord -> Bool
validTargetCoord b@(Board _ mat) c =
    Arr.inRange bound c && atCoord b c /= WoodTile
  where
    bound = Arr.bounds mat

atCoordSafe :: Board -> Coord -> Maybe Tile
atCoordSafe b@(Board _ mat) c
    | Arr.inRange bound c = Just (atCoord b c)
    | otherwise = Nothing
  where
    bound = Arr.bounds mat

coordDist :: Coord -> Coord -> Int
coordDist (x1,y1) (x2,y2) = abs (x1-x2) + abs(y1-y2)
