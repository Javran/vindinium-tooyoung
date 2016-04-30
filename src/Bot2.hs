module Bot2 where

import Vindinium
import Vindinium.Vdm
import Vindinium.Board.Summary
import Vindinium.Board.ShortestPath
import Data.Foldable hiding (find)
import Data.Maybe
import Data.Function
import Data.Typeable
import Control.Monad
import Data.Default
import Data.List hiding (find)
import Control.Monad.Random
import Data.Monoid
import Data.Universe
import qualified Data.IntMap.Strict as IM
import Control.Monad.Primitive
import Control.Arrow
import qualified Data.Array as Arr
import System.Random.MWC
import Text.Printf
import Bot hiding (fullRecoverPlannerWithThreshold)

{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Use head" #-}
{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Use uncurry" #-}

calcEnvEmergency :: MyState -> GameState -> Double
calcEnvEmergency ms gs = (fromIntegral heroHp :: Double)
                       / fromIntegral (20 * minimum opponentDistances)
  where
    heroHp = heroLife . stateHero $ gs :: Int
    -- remove the first one, which is our bot
    opponents = tail . gameHeroes . stateGame $ gs
    spi = myShortestPathInfo gs ms
    posToCoord (Pos coord) = coord
    opponentCoords = map (posToCoord . heroPos) opponents
    -- need all opponents to be reachable -- we assume this is true.
    opponentDistances =
        map (\coord -> piDist
                     . fromJust
                     $ spi Arr.! coord) opponentCoords :: [Int]

-- run weighted planners in sequence, and pick one with maximum weight
pickOptimalPlan :: [VWeightedPlanner s] -> VPlanner s
pickOptimalPlan ps myState gState = do
    plans <- catMaybes <$> mapM (\p -> p myState gState) ps
    case plans of
        [] -> pure Nothing
        _ -> let (optimal,_) = maximumBy (compare `on` snd) plans
             in pure (Just optimal)

-- planner for debug
debugPlanner :: MyPlanner
debugPlanner mState gState = do
    let e = calcEnvEmergency mState gState
        find v = fromJust . IM.lookup v
        aap = vAutoAdjustedParam mState
        hToGs = map (\i -> find i (aapHealthToGold aap)) [1..2]
        hs = zip [1 :: Int ..2] hToGs
        dToGs = map (\i -> find i (aapDistToGold aap)) [1..6]
        ds = zip [1 :: Int ..6] dToGs
    io $ do
        putStrLn $ "environment emergency: " ++ show e
        putStrLn "health to gold:"
        forM_ hs $ \(k,v) -> printf "  %d: %f\n" k v
        putStrLn "dist to gold:"
        forM_ ds $ \(k,v) -> printf "  %d: %f\n" k v
    pure Nothing

myBot2 :: MyPlanner
myBot2 m g = do
    _ <- debugPlanner m g
    pickOptimalPlan
      [ fullRecoverPlannerWithThreshold 80
      , healthMaintainPlannerW
      , mineObtainPlannerW
      ] m g

fullRecoverPlannerWithThreshold :: Int -> MyWeightedPlanner
fullRecoverPlannerWithThreshold hpThres mstate gstate = do
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
          let find v = fromJust . IM.lookup v
              aap = vAutoAdjustedParam mstate
              hToG1 = find 1 (aapHealthToGold aap)
              dToG1 = find 1 (aapDistToGold aap)
              envEmergency = calcEnvEmergency mstate gstate
              score = fromIntegral (80 - heroLife hero) * hToG1 + envEmergency * dToG1
          -- give a score about this action
          io $ do
              putStrLn $ "fullRecoverPlanner with score " ++ show score
          pure (Just (snd (head nearbyTaverns), score))
       else pure Nothing

-- should not pass empty list
rndPickOne :: PrimMonad m => Gen (PrimState m) -> [a] -> m a
rndPickOne g xs = do
    let l = length xs
    ind <- uniformR (0,l-1) g
    pure (xs !! ind)

-- heading to one of the closest locations with probably random moves
rndHeadToClosestOf :: [Coord] -> MyState -> GameState -> Vdm MyState (Maybe (Dir,Coord))
rndHeadToClosestOf [] _ _ = do
    io $ putStrLn "rndHeadToClosestOf: no candidate available"
    pure Nothing
rndHeadToClosestOf cs@(_:_) vstate gstate = do
    -- NOTE: make sure fromJust is safe
    -- INVARIANT: cs is not empty (guaranteed by pattern matching)
    let spi = myShortestPathInfo gstate vstate
        getDist c = piDist (fromJust (unsafeIndex spi c))
        -- calculate shortest distance
        shortestDist = minimum (map getDist cs)
        -- filter candidate list to have only shortest ones
        targets = filter (\x -> getDist x == shortestDist) cs
        -- target = minimumBy (compare `on` getDist) cs
        rng = vGen vstate
    target <- io $ rndPickOne rng targets
    io $ putStrLn $ "rndHeadToClosestOf heading to: " ++ show target
    let pathM = findPathTo spi target
    case pathM of
        Just (p:_) -> pure (Just (p,target))
        _ -> do
            io $ putStrLn "headToClosestOf: failed on path finding"
            pure Nothing

healthMaintainPlannerW :: MyWeightedPlanner
healthMaintainPlannerW vstate gstate = do
    let board = gameBoard . stateGame $ gstate
        hero = stateHero gstate
        spi = myShortestPathInfo gstate vstate
        -- find near by tavern
        taverns =
              filter (\coord -> case atCoord board coord of
                        TavernTile | Just _ <- unsafeIndex spi coord -> True
                        _ -> False)
            . sTaverns
            . vSummary
            $ vstate
    dResult <- rndHeadToClosestOf taverns vstate gstate
    let find v = fromJust . IM.lookup v
        aap = vAutoAdjustedParam vstate
        hToG2 = find 2 (aapHealthToGold aap)
        dToG2 = find 2 (aapDistToGold aap)
        envEmergency = calcEnvEmergency vstate gstate
        score = envEmergency * dToG2 + fromIntegral (50 - heroLife hero) * hToG2

    io $ putStrLn $ "healthMaintain with score " ++ show score
    pure (dResult >>= \(d,_) -> Just (d,score))

mineObtainPlannerW :: MyWeightedPlanner
mineObtainPlannerW vstate gstate = do
    -- find closest not-obtained mine
    let board = gameBoard . stateGame $ gstate
        hero = stateHero gstate
        spi = myShortestPathInfo gstate vstate
        targetMines = filter (\coord -> case atCoord board coord of
                                  MineTile s
                                      | s /= Just (heroId hero)
                                      , Just _ <- unsafeIndex spi coord -> True
                                  _ -> False)
                    . sMines
                    . vSummary
                    $ vstate
    io $ putStrLn $ "mineObtain: I found " ++ show (length targetMines) ++ " target mines"
    dResult <- rndHeadToClosestOf targetMines vstate gstate
    case dResult of
        Nothing -> pure Nothing
        Just (d, target) -> do
            let find v = fromJust . IM.lookup v
                getDist c = piDist (fromJust (unsafeIndex spi c))
                -- calculate shortest distance
                targetDist = getDist target
                aap = vAutoAdjustedParam vstate
                dToG5 = find 2 (aapDistToGold aap)
                score = fromIntegral targetDist * dToG5
            io $ putStrLn $ "mineObtain with score " ++ show score
            pure (Just (d,score))
