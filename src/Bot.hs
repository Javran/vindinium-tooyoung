module Bot where

import Vindinium
import Vindinium.Vdm
import Vindinium.Board.Summary
import Vindinium.Board.ShortestPath
import Data.Foldable
import Data.Maybe
import Data.Function
import Data.Typeable
import Control.Monad
import Data.Default
import Data.List
import Control.Monad.Random

data MyState = VState
  { vStarted :: Bool
  , vSummary :: Summary
  , vShortestPathInfo :: ShortestPathInfo
  }  deriving (Show)

instance Default MyState where
    -- these 2 pieces of info (set to error)
    -- will be made available once the game is started
    def = VState
            False
            (error "summary not available")
            (error "spi not available")

myPP :: VPreprocessor MyState
myPP vs state = do
    unless (vStarted vs) $
        putVState vs
          { vStarted = True
          , vSummary = summarize (gameBoard . stateGame $ state)
          }

    -- do path finding
    modifyVState
        (\s -> let board = gameBoard . stateGame $ state
                   (Pos coord) = heroPos . stateHero $ state
               in s { vShortestPathInfo = calcShortestPathInfo board coord })

myBot :: VPlanner MyState
myBot =
    fullRecoverPlanner `composePlanner`
    avoidPlayerPlanner `composePlanner`
    healthMaintainPlanner `composePlanner`
    mineObtainPlanner

headToClosestOf :: [Coord] -> VPlanner MyState
headToClosestOf [] _ _ = do
    io $ putStrLn "headToClosestOf: no candidate available"
    pure Nothing
headToClosestOf cs@(_:_) vstate _ = do
    -- NOTE: make sure fromJust is safe
    -- INVARIANT: cs is not empty (guaranteed by pattern matching)
    let spi = vShortestPathInfo vstate
        getDist c = piDist (fromJust (unsafeIndex spi c))
        target = minimumBy (compare `on` getDist) cs
        pathM = findPathTo spi target
    io $ putStrLn $ "Heading to: " ++ show target
    case pathM of
        Just (p:_) -> pure (Just p)
        _ -> do
            io $ putStrLn "headToClosestOf: failed on path finding"
            pure Nothing

composePlanner :: Typeable s => VPlanner s -> VPlanner s -> VPlanner s
composePlanner p1 p2 vstate gstate = do
    r1 <- p1 vstate gstate
    case r1 of
        Nothing -> do
            vstate' <- getVState
            p2 vstate' gstate
        Just _ -> pure r1

fullRecoverPlanner :: VPlanner MyState
fullRecoverPlanner _ gstate = do
    let board = gameBoard . stateGame $ gstate
        hero = stateHero gstate
        (Pos hPos) = heroPos hero
        nearbyTaverns = [ (c,dir)
                        | dir <- [North, South, West, East]
                        , let c = applyDir dir hPos
                        , Just TavernTile == atCoordSafe board c
                        ]
    if heroLife hero <= 95 && not (null nearbyTaverns)
       then do
          io $ do
              putStrLn $ "hero life: " ++ show (heroLife hero)
              putStrLn   "keep healing"
          pure (Just (snd (head nearbyTaverns)))
       else pure Nothing

healthMaintainPlanner :: VPlanner MyState
healthMaintainPlanner vstate gstate = do
    let board = gameBoard . stateGame $ gstate
        hero = stateHero gstate
        spi = vShortestPathInfo vstate
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
          headToClosestOf taverns vstate gstate
       else pure Nothing

mineObtainPlanner :: VPlanner MyState
mineObtainPlanner vstate gstate = do
    -- find closest not-obtained mine
    let board = gameBoard . stateGame $ gstate
        hero = stateHero gstate
        spi = vShortestPathInfo vstate
        targetMines = filter (\coord -> case atCoord board coord of
                                  MineTile s
                                      | s /= Just (heroId hero)
                                      , Just _ <- unsafeIndex spi coord -> True
                                  _ -> False)
                    . sMines
                    . vSummary
                    $ vstate
    io $ putStrLn $ "I found " ++ show (length targetMines) ++ " target mines"
    headToClosestOf targetMines vstate gstate

-- TODO: take wood cells into account
avoidPlayerPlanner :: VPlanner MyState
avoidPlayerPlanner vstate gstate = do
    let board = gameBoard . stateGame $ gstate
        hero = stateHero gstate
        summary = vSummary vstate
        allHeroes = gameHeroes . stateGame $ gstate
        allDirs = [Stay,North,South,West,East]
        threatingHeroes = filter
                            (\h -> heroId h /= heroId hero
                                && heroLife h >= heroLife hero)
                            allHeroes
        -- TODO: proper impl of hero attacking
        dangerousHeroCoords1 = [ applyDir d pos
                              | d <- allDirs
                              , (Pos pos) <- heroPos <$> threatingHeroes
                              ]
        dangerousHeroCoords = nub [ applyDir d pos
                                  | d <- allDirs
                                  , d /= Stay
                                  , pos <- dangerousHeroCoords1
                                  ]
        dangerousSpawningPoints = [ spawningPointOf summary hId
                                  | hId <- heroId <$> filter (\h -> heroLife h <= 21) allHeroes
                                  , hId /= heroId hero]
        dangerousCoords = dangerousHeroCoords ++ dangerousSpawningPoints
        possibleMoves = [ (d,applyDir d pos)
                        | let (Pos pos) = heroPos hero
                        , d <- allDirs
                        ]
        (dangerousMoves,nonDangerousMoves) = partition
                                               (\(_,coord) -> coord `elem` dangerousCoords)
                                               possibleMoves
    case dangerousMoves of
        [] -> pure Nothing -- every direction is safe.
        _ ->
            case nonDangerousMoves of
                [] -> do
                    io $ putStrLn "warning: no move is safe, proceeding to next planner"
                    pure Nothing
                _ -> do
                    io $ putStrLn "reaching dangerous places, trying to avoid"
                    takeRandomMove (map fst nonDangerousMoves)

takeRandomMove :: Typeable s => [Dir] -> Vdm s (Maybe Dir)
takeRandomMove [] = pure Nothing
takeRandomMove ds = do
    i <- io $ getRandomR (0,length ds-1)
    pure (Just (ds !! i))
