{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Play
       ( playTrainingEff
       , playArenaEff
       )
    where

import Vindinium.Types
import Network.HTTP.Client
import Network.HTTP.Types
import Control.Monad.Trans.Resource
import Data.Text (Text, unpack)
import Control.Monad.IO.Class
import Control.Eff.Lift
import Data.Monoid

import Data.Aeson
import Vindinium.Vdm

playTrainingEff :: Maybe Int -> Maybe Board -> (State -> Vdm Dir) -> Vdm VdmState
playTrainingEff mt mb b = startTrainingEff mt mb >>= playLoopEff b

playArenaEff :: (State -> Vdm Dir) -> Vdm VdmState
playArenaEff b = startArenaEff >>= playLoopEff b

playLoopEff :: (State -> Vdm Dir) -> State -> Vdm VdmState
playLoopEff bot state =
    if (gameFinished . stateGame) state
        then getVState
        else do
            let turn = gameTurn . stateGame $ state
                maxTurn = gameMaxTurns . stateGame $ state
            io $ putStrLn $ "Playing turn: " ++ show turn ++ " / " ++ show maxTurn
            newState <- bot state >>= moveEff state
            playLoopEff bot newState

startTrainingEff:: Maybe Int -> Maybe Board -> Vdm State
startTrainingEff mi mb = do
    (Key key) <- vcKey <$> askVConfig
    url <- startUrlEff "training"
    let obj = object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                    <> maybe [] (\b -> [("map",  toJSON b)]) mb
                     )
    s <- lift $ request' key url obj
    io $ putStrLn $ "url is: " ++ (unpack . stateViewUrl $ s)
    return s

startArenaEff :: Vdm State
startArenaEff = do
    (Key key) <- vcKey <$> askVConfig
    url <- startUrlEff "arena"
    let obj = object []
    s <- lift $ request' key url obj
    io $ putStrLn $ "url is: " ++ (unpack . stateViewUrl $ s)
    return s

startUrlEff :: Text -> Vdm Text
startUrlEff v = do
    url <- vcUrl <$> askVConfig
    return $ (\x -> x <> "/api/" <> v) url

moveEff :: State -> Dir -> Vdm State
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
    injectKey _ _ = error "impossible"
