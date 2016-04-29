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
import Data.Universe
import qualified Data.IntMap as IM
import System.Random.MWC
import qualified Data.Array as Arr

{-# ANN module "HLint: ignore Reduce duplication" #-}

type VWeightedPlanner s = s -> GameState -> Vdm s (Maybe (Dir, Double))

type MyPlanner = VPlanner MyState
type MyWeightedPlanner = VWeightedPlanner MyState

-- run weighted planners in sequence, and pick one with maximum weight
pickOptimalPlan :: [VWeightedPlanner s] -> VPlanner s
pickOptimalPlan ps myState gState = do
    plans <- catMaybes <$> mapM (\p -> p myState gState) ps
    case plans of
        [] -> pure Nothing
        _ -> let (optimal,_) = maximumBy (compare `on` snd) plans
             in pure (Just optimal)

calcEnvEmergency :: MyState -> GameState -> Double
calcEnvEmergency ms gs = (fromIntegral heroHp :: Double)
                       / fromIntegral (20 * minimum opponentDistances)
  where
    heroHp = heroLife . stateHero $ gs :: Int
    -- remove the first one, which is our bot
    opponents = tail . gameHeroes . stateGame $ gs
    spi = vShortestPathInfo ms
    posToCoord (Pos coord) = coord
    opponentCoords = map (posToCoord . heroPos) opponents
    -- need all opponents to be reachable -- we assume this is true.
    opponentDistances =
        map (\coord -> piDist
                     . fromJust
                     $ spi Arr.! coord) opponentCoords :: [Int]

-- mask spawning point of opponents as wood tiles
getMaskedBoard :: Summary -> Board -> Board
getMaskedBoard s (Board sz b) = Board sz newB
  where
    newB = b Arr.// zip opponentSpawningPoints (repeat WoodTile)
    opponentSpawningPoints =
          IM.elems
        . IM.delete 0
        $ sSpawnPoints s

data MyState = VState
  { vStarted :: Bool
  , vSummary :: Summary
  , vShortestPathInfoArr :: Arr.Array Int ShortestPathInfo
  , vSafeShortestPathInfoArr :: Arr.Array Int ShortestPathInfo
  , vGen :: GenIO
  }

vShortestPathInfo :: MyState -> ShortestPathInfo
vShortestPathInfo s = vShortestPathInfoArr s Arr.! 0

instance Show MyState where
    show s = "VState "
          ++ "{ vStarted: " ++ show (vStarted s)
          ++ ", vSummary: " ++ show (vSummary s)
          ++ ", vShortestPathInfo: " ++ show (vShortestPathInfoArr s)
          ++ ", vGen: " ++ "<hidden>"
          ++ "}"

instance Default MyState where
    -- these 2 pieces of info (set to error)
    -- will be made available once the game is started
    def = VState
            False
            (error "summary not available")
            (error "spi not available")
            (error "safe spi not available")
            (error "random generator not available")

calcShortestPathInfoArr :: Board -> [Coord] -> Arr.Array Int ShortestPathInfo
calcShortestPathInfoArr board coords
    | length coords == 4 = Arr.listArray (0,3) (map (calcShortestPathInfo board) coords)
    | otherwise = error "coords must be a list of 4 elements"

myPP :: VPreprocessor MyState
myPP vs state = do
    unless (vStarted vs) $ do
        gen <- io createSystemRandom
        putVState vs
          { vStarted = True
          , vSummary = summarize (gameBoard . stateGame $ state)
          , vGen = gen
          }
    -- do path finding
    modifyVState
        (\s -> let board = gameBoard . stateGame $ state
                   maskedBoard = getMaskedBoard (vSummary s) board
                   getCoord (Pos v) = v
                   coords = map (getCoord . heroPos) (gameHeroes . stateGame $ state) :: [Coord]
               in s { vShortestPathInfoArr = calcShortestPathInfoArr board coords
                    , vSafeShortestPathInfoArr = calcShortestPathInfoArr maskedBoard coords})

myBot :: MyPlanner
myBot =
    fullRecoverPlanner `composePlanner`
    avoidPlayerPlanner `composePlanner`
    healthMaintainPlanner `composePlanner`
    mineObtainPlanner

headToClosestOf :: [Coord] -> MyPlanner
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

fullRecoverPlannerWithThreshold :: Int -> MyPlanner
fullRecoverPlannerWithThreshold hpThres _ gstate = do
    let board = gameBoard . stateGame $ gstate
        hero = stateHero gstate
        (Pos hPos) = heroPos hero
        nearbyTaverns = [ (c,dir)
                        | dir <- [North, South, West, East]
                        , let c = applyDir dir hPos
                        , Just TavernTile == atCoordSafe board c
                        ]
    if heroLife hero <= hpThres && not (null nearbyTaverns)
       then do
          io $ do
              putStrLn $ "hero life: " ++ show (heroLife hero)
              putStrLn   "keep healing"
          pure (Just (snd (head nearbyTaverns)))
       else pure Nothing

fullRecoverPlanner :: MyPlanner
fullRecoverPlanner = fullRecoverPlannerWithThreshold 95

healthMaintainPlanner :: MyPlanner
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

mineObtainPlanner :: MyPlanner
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
avoidPlayerPlanner :: MyPlanner
avoidPlayerPlanner vstate gstate = do
    -- TODO: escape path need to be vaild
    let board = gameBoard . stateGame $ gstate
        hero = stateHero gstate
        summary = vSummary vstate
        allHeroes = gameHeroes . stateGame $ gstate
        -- all heroes that can survive our attack
        threatingHeroes = filter
                            (\h -> heroId h /= heroId hero
                                && heroLife h >= heroLife hero)
                            allHeroes
        -- TODO: proper impl of hero attacking
        dangerousHeroCoords = nub $ do
            d <- universe
            (Pos pos) <- heroPos <$> threatingHeroes
            -- possible hero positions after one move
            hPos <- maybeToList (applyDir' board d pos)
            -- possible attacking positions one hero can have
            da <- universe
            guard (da /= Stay)
            pure (applyDir da hPos)
        dangerousSpawningPoints = nub $ do
            -- pick one dying hero other than me
            h <- filter (\h -> heroLife h <= 21) allHeroes
            guard $ heroId hero /= heroId h
            let hPos = spawningPointOf summary (heroId h)
            -- hero will respawn at coord hPos
            da <- universe
            pure (applyDir da hPos)
        dangerousCoords = nub $ dangerousHeroCoords ++ dangerousSpawningPoints
        possibleMoves = [ (d,applyDir d pos)
                        | let (Pos pos) = heroPos hero
                        , d <- universe
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
