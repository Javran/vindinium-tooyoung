{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Vindinium
import Vindinium.Vdm
import Bot
import Bot2
import Data.Default

import qualified Data.Text as T

type Cmd = (Settings, GameMode)

getSettings :: IO Settings
getSettings = do
    [keyRaw,urlRaw,bot] <- take 3 . lines <$> readFile "settings.conf"
    return (Settings (T.pack keyRaw) (T.pack urlRaw) bot)

trainingCmd :: Parser GameMode
trainingCmd = GMTraining <$> optional (option auto (long "turns"))
                         <*> pure Nothing

arenaCmd :: Parser GameMode
arenaCmd = pure GMArena

cmd :: Parser GameMode
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        ( progDesc "Run bot in arena mode" ))
    )

runCmd :: Settings -> GameMode -> IO ()
runCmd s gm = do
    let cfg = (vdmConfig <$> settingsKey <*> settingsUrl) s
        bDesc = settingsBot s
    bot <- case bDesc of
        "qr" -> pure myBot2
        "simple" -> pure myBot
        _ -> do
            putStrLn $ "unknown bot: " ++ bDesc
            putStrLn "using simple"
            pure myBot
    state <- runVdm cfg def $ playGame gm myPP bot
    print state

main :: IO ()
main = do
    s <- getSettings
    execParser opts >>= runCmd s
  where
    opts = info (cmd <**> helper) idm
