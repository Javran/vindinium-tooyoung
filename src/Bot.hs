module Bot
    where

import Vindinium
import Vindinium.VdmEff

import Control.Monad.Random

randomBot' :: State -> VdmEff Dir
randomBot' _ = do
    let candidates = [Stay, North, South, East, West]
    idx <- io $ getRandomR (0, length candidates-1)
    return (candidates !! idx)

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) =
    if inBoard b p
        then Just $ boardTiles b !! idx
        else Nothing
  where
    idx = y * boardSize b + x

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx
