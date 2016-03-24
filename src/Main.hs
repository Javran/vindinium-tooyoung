{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Vindinium
import Vindinium.Vdm
import Bot
import Data.Default

import qualified Data.Text as T

type Cmd = (Settings, GameMode)

getSettings :: IO Settings
getSettings = do
    [keyRaw,urlRaw] <- take 2 . lines <$> readFile "settings.conf"
    return (Settings (T.pack keyRaw) (T.pack urlRaw))

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
    state <- runVdm cfg def $ playGame gm myPP myBot
    print state

main :: IO ()
main = do
    s <- getSettings
    execParser opts >>= runCmd s
  where
    opts = info (cmd <**> helper) idm
