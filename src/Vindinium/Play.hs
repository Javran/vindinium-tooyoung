{-# LANGUAGE OverloadedStrings, ExplicitForAll, TypeFamilies #-}
module Vindinium.Play where

import Vindinium.Types
import qualified Data.Text as T
import Data.Monoid

import Data.Aeson
import Data.Maybe
import Vindinium.Vdm
import Vindinium.Api
import Data.Typeable

playGame :: Typeable s => GameMode -> VPreprocessor s -> VPlanner s -> Vdm s s
playGame gm pp b = do
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
    playLoop pp b s

playLoop :: Typeable s => VPreprocessor s -> VPlanner s -> GameState -> Vdm s s
playLoop pp bot state =
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
            pp vs state
            vs' <- getVState
            newState <- fromMaybe Stay <$> bot vs' state >>= move (statePlayUrl state)
            playLoop pp bot newState

startUrl :: GameMode -> Vdm s T.Text
startUrl gm = do
    let v = case gm of
              GMTraining _ _ -> "training"
              GMArena        -> "arena"
    url <- vcUrl <$> askVConfig
    return $ url <> "/api/" <> v

move :: Url -> Dir -> Vdm s GameState
move u d = sendRequest u $ object [("dir", toJSON d)]

sendRequest :: Url -> Value -> Vdm s GameState
sendRequest u v =
    askVConfig >>=
    io . (request <$> vcKey <*> pure u <*> vcMgr <*> pure v)
