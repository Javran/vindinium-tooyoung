module Bot where

import Vindinium
import Vindinium.Vdm
import Vindinium.Board
import Vindinium.Board.Summary
import Vindinium.Board.ShortestPath
import Data.Foldable
import Data.Maybe
import Control.Monad.Random
import qualified Data.Array.IArray as IA
import Data.Function
import Data.List

myBot :: VPlanner
myBot =
    fullRecoverPlanner `composePlanner`
    healthMaintainPlanner `composePlanner`
    mineObtainPlanner

composePlanner :: VPlanner -> VPlanner -> VPlanner
composePlanner p1 p2 vstate gstate = do
    r1 <- p1 vstate gstate
    case r1 of
        Nothing -> do
            vstate' <- getVState
            p2 vstate' gstate
        Just _ -> pure r1

fullRecoverPlanner :: VPlanner
fullRecoverPlanner vstate gstate = do
    let board = gameBoard . stateGame $ gstate
        hero = stateHero $ gstate
        (Pos hPos) = heroPos hero
        spi = vShortestPathInfo $ vstate
        nearbyTaverns = [ (c,dir)
                        | dir <- [North, South, West, East]
                        , let c = applyDir dir hPos
                        , Just TavernTile == atCoordSafe board c
                        ]
    if heroLife hero <= 95 && not (null nearbyTaverns)
       then do
          io $ do
              putStrLn $ "hero life: " ++ show (heroLife hero)
              putStrLn $ "keep healing"
          pure (Just (snd (head nearbyTaverns)))
       else pure Nothing

healthMaintainPlanner :: VPlanner
healthMaintainPlanner vstate gstate = do
    let board = gameBoard . stateGame $ gstate
        hero = stateHero $ gstate
        spi = vShortestPathInfo $ vstate
    if heroLife hero <= 20
       then do
          io $ putStrLn "Low health, need to recover."
          -- find near by tavern
          let taverns =
                    filter (\coord -> case atCoord board coord of
                              TavernTile | Just _ <- unsafeIndex spi coord -> True
                              _ -> False)
                  . sTaverns
                  . vSummary
                  $ vstate
          case taverns of
              [] -> pure Nothing
              _ -> do
                  let getDist c = piDist (fromJust (unsafeIndex spi c))
                      target = minimumBy (compare `on` getDist) taverns
                      pathM = findPathTo spi target
                  io $ putStrLn $ "Heading to: " ++ show target
                  case pathM of
                      Just (p:_) -> pure (Just p)
                      _ -> pure Nothing
       else pure Nothing

mineObtainPlanner :: VPlanner
mineObtainPlanner vstate gstate = do
    -- find closest not-obtained mine
    let board = gameBoard . stateGame $ gstate
        hero = stateHero $ gstate
        spi = vShortestPathInfo $ vstate
        targetMines = filter (\coord -> case atCoord board coord of
                                  MineTile s
                                      | s /= Just (heroId hero)
                                      , Just _ <- unsafeIndex spi coord -> True
                                  _ -> False)
                    . sMines
                    . vSummary
                    $ vstate
    io $ putStrLn $ "I found " ++ show (length targetMines) ++ " target mines"
    case targetMines of
        [] -> pure Nothing
        _ -> do
            -- here fromJust is safe because of the "Just _ <- " check
            -- in the previous filter
            let getDist c = piDist (fromJust (unsafeIndex spi c))
                target = minimumBy (compare `on` getDist) targetMines
                pathM = findPathTo spi target
            io $ putStrLn $ "Heading to: " ++ show target
            case pathM of
                Just (p:_) -> pure (Just p)
                _ -> pure Nothing



inBoard :: Board -> Pos -> Bool
inBoard b (Pos (x,y)) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos (x,y)) =
    if inBoard b p
        then Just $ boardTiles b IA.! (x,y)
        else Nothing

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx
