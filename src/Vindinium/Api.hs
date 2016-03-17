{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Api where

import Vindinium.Types
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.Text as T
import Data.Aeson
import qualified Data.HashMap.Strict as HM

request :: Key -> Url -> Manager -> Value -> IO GameState
request key url mgr val = do
    initReq <- parseUrl $ T.unpack url
    let req = initReq
                { method = "POST"
                , requestHeaders =
                    [ (hContentType, "application/json")
                    , (hAccept,      "application/json")
                    , (hUserAgent,   "vindinium-tooyoung")
                    ]
                , requestBody = jsonBody (injectKey val key)
                , responseTimeout = Nothing
                }
    (decodeBody . responseBody) <$> httpLbs req mgr
  where
    jsonBody = RequestBodyLBS . encode
    decodeBody body = case eitherDecode body of
            Left e  -> error $ "request: unable to decode state: " ++ e
            Right s -> s
    injectKey (Object a) k = Object (HM.insert "key" (toJSON k) a)
    injectKey v _ = v
