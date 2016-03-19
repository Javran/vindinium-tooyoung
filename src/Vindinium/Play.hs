{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Play where

import Vindinium.Types
import qualified Data.Text as T
import Data.Monoid

import Data.Aeson
import Data.Maybe
import Vindinium.Vdm
import Vindinium.Api
import Vindinium.Board.ShortestPath
import Vindinium.Board.Summary
import Control.Monad

playGame :: GameMode -> VPlanner -> Vdm VdmState
playGame gm b = do
    url <- startUrl gm
    let obj = case gm of
            GMTraining mt mb ->
               object ( maybe [] (\i -> [("turns", toJSON i)]) mt
                     <> maybe [] (\bm -> [("map",  toJSON bm)]) mb
                      )
            GMArena -> object []
    s <- sendRequest url obj
    io $ do
        putStrLn $ "url is: " ++ (T.unpack . stateViewUrl $ s)
        appendFile "view_url" ((T.unpack . stateViewUrl $ s) ++ "\n")
    playLoop b s

playLoop :: VPlanner -> GameState -> Vdm VdmState
playLoop bot state =
    if (gameFinished . stateGame) state
        then getVState
        else do
            let turn = gameTurn . stateGame $ state
                maxTurn = gameMaxTurns . stateGame $ state
            io $ do
                putStrLn $ "Playing turn: " ++ show turn ++ " / " ++ show maxTurn
                -- putStrLn $ "Hero position" ++ show (heroPos . stateHero $ state)
                -- let hPos = heroPos . stateHero $ state
                let board = gameBoard . stateGame $ state
                pprBoard board
            vs <- getVState
            -- update map summary
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
            vs' <- getVState
            newState <- fromMaybe Stay <$> bot vs' state >>= move (statePlayUrl state)
            playLoop bot newState

startUrl :: GameMode -> Vdm T.Text
startUrl gm = do
    let v = case gm of
              GMTraining _ _ -> "training"
              GMArena        -> "arena"
    url <- vcUrl <$> askVConfig
    return $ url <> "/api/" <> v

move :: Url -> Dir -> Vdm GameState
move u d = sendRequest u $ object [("dir", toJSON d)]

sendRequest :: Url -> Value -> Vdm GameState
sendRequest u v =
    askVConfig >>=
    io . (request <$> vcKey <*> pure u <*> vcMgr <*> pure v)
