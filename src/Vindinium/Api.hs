{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Api
        ( startTraining
        , startArena
        , move
        )
    where

import Vindinium.Types

import Network.HTTP.Client
import Network.HTTP.Types

import Data.Text (Text, pack, unpack)
import Data.Aeson
import Data.Monoid

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

startTraining :: Maybe Int -> Maybe Board -> Vindinium State
startTraining mi mb = do
    url <- startUrl "training"
    let obj = object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                    <> maybe [] (\b -> [("map",  toJSON b)]) mb
                     )

    request url obj

move :: State -> Dir -> Vindinium State
move s d = do
    let url = statePlayUrl s
        obj = object [("dir", toJSON d)]
    request url obj


startArena :: Vindinium State
startArena = do
    url <- startUrl "arena"
    let obj = object []
    request url obj

startUrl :: Text -> Vindinium Text
startUrl v = liftM (\x -> x <> "/api/" <> v) $ asks settingsUrl

request :: Text -> Value -> Vindinium State
request url val = do
    key <- asks settingsKey

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
        withManager defaultManagerSettings $ \mgr ->
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



