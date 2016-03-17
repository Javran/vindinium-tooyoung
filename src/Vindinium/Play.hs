{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Play where

import Vindinium.Types
import qualified Data.Text as T
import Data.Monoid

import Data.Aeson
import Vindinium.Vdm
import Vindinium.Api

playGame :: GameMode -> (GameState -> Vdm Dir) -> Vdm VdmState
playGame gm b = do
    url <- startUrl gm
    let obj = case gm of
            GMTraining mt mb ->
               object ( maybe [] (\i -> [("turns", toJSON i)]) mt
                     <> maybe [] (\bm -> [("map",  toJSON bm)]) mb
                      )
            GMArena -> object []
    s <- sendRequest url obj
    io $ putStrLn $ "url is: " ++ (T.unpack . stateViewUrl $ s)
    playLoop b s

playLoop :: (GameState -> Vdm Dir) -> GameState -> Vdm VdmState
playLoop bot state =
    if (gameFinished . stateGame) state
        then getVState
        else do
            let turn = gameTurn . stateGame $ state
                maxTurn = gameMaxTurns . stateGame $ state
            io $ putStrLn $ "Playing turn: " ++ show turn ++ " / " ++ show maxTurn
            newState <- bot state >>= move (statePlayUrl state)
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
