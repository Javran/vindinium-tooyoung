{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Play
        ( playTraining
        , playArena
        , playTrainingEff
        , playArenaEff
        )
    where

import Vindinium.Types
import Vindinium.Api
import Network.HTTP.Client
import Network.HTTP.Types
import Control.Monad.Trans.Resource
import Data.Text (Text, unpack, pack)
import Control.Monad.IO.Class
import Control.Eff.Lift
import Data.Monoid

import Data.Aeson
import Vindinium.VdmEff

playTraining :: Maybe Int -> Maybe Board -> Bot -> Vindinium State
playTraining mt mb b = startTraining mt mb >>= playLoop b

playArena :: Bot -> Vindinium State
playArena b = startArena >>= playLoop b

playLoop :: Bot -> State -> Vindinium State
playLoop bot state =
    if (gameFinished . stateGame) state
        then return state
        else do
            newState <- bot state >>= move state
            playLoop bot newState

playTrainingEff :: Maybe Int -> Maybe Board -> (State -> VdmEff Dir) -> VdmEff VdmState
playTrainingEff mt mb b = startTrainingEff mt mb >>= playLoopEff b

playArenaEff :: (State -> VdmEff Dir) -> VdmEff VdmState
playArenaEff b = startArenaEff >>= playLoopEff b

playLoopEff :: (State -> VdmEff Dir) -> State -> VdmEff VdmState
playLoopEff bot state =
    if (gameFinished . stateGame) state
        then getVState
        else do
            let turn = gameTurn . stateGame $ state
                maxTurn = gameMaxTurns . stateGame $ state
            io $ putStrLn $ "Playing turn: " ++ show turn ++ " / " ++ show maxTurn
            newState <- bot state >>= moveEff state
            playLoopEff bot newState

startTrainingEff:: Maybe Int -> Maybe Board -> VdmEff State
startTrainingEff mi mb = do
    (Key key) <- vcKey <$> askVConfig
    url <- startUrlEff "training"
    let obj = object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                    <> maybe [] (\b -> [("map",  toJSON b)]) mb
                     )
    s <- lift $ request' key url obj
    io $ putStrLn $ "url is: " ++ (unpack . stateViewUrl $ s)
    return s

startArenaEff :: VdmEff State
startArenaEff = do
    (Key key) <- vcKey <$> askVConfig
    url <- startUrlEff "arena"
    let obj = object []
    s <- lift $ request' key url obj
    io $ putStrLn $ "url is: " ++ (unpack . stateViewUrl $ s)
    return s

startUrlEff :: Text -> VdmEff Text
startUrlEff v = do
    url <- vcUrl <$> askVConfig
    return $ (\x -> x <> "/api/" <> v) url

moveEff :: State -> Dir -> VdmEff State
moveEff s d = do
    (Key key) <- vcKey <$> askVConfig
    let url = statePlayUrl s -- TODO: what we need is just the URL
        obj = object [("dir", toJSON d)]
    lift $ request' key url obj

request' :: MonadResource m => Text -> Text -> Value -> m State
request' key url val = do
    initReq <- liftIO $ parseUrl $ unpack url
    let req = initReq
                { method = "POST"
                , requestHeaders =
                    [ (hContentType, "application/json")
                    , (hAccept,      "application/json")
                    , (hUserAgent,   "vindinium-starter-haskell")
                    ]
                , requestBody = jsonBody (injectKey val key)
                , responseTimeout = Nothing
                }

    liftIO $ do
        mgr <- newManager defaultManagerSettings
        (decodeBody . responseBody) <$> httpLbs req mgr

  where
    jsonBody = RequestBodyLBS . encode
    decodeBody body = case eitherDecode body of
            Left e  -> error $ "request: unable to decode state: " ++ e
            Right s -> s
    injectKey (Object a) k =
        let
            (Object b) = object [("key", toJSON k)]
        in
            Object (a <> b)

