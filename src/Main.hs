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
    [keyRaw,urlRaw] <- take 2 . lines <$> readFile "settings.conf"
    return (Settings (T.pack keyRaw) (T.pack urlRaw))

trainingCmd :: Parser GameMode
trainingCmd = GMTraining <$> optional (option auto (long "turns"))
                         <*> pure Nothing
                         <*> strOption (long "bot")

arenaCmd :: Parser GameMode
arenaCmd = GMArena <$> strOption (long "bot")

cmd :: Parser GameMode
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        ( progDesc "Run bot in arena mode" ))
    )

selectedBot :: GameMode -> String
selectedBot gm = case gm of
    GMTraining _ _ b -> b
    GMArena b -> b

runCmd :: Settings -> GameMode -> IO ()
runCmd s gm = do
    let cfg = (vdmConfig <$> settingsKey <*> settingsUrl) s
        bDesc = selectedBot gm
    bot <- case bDesc of
        "qr" -> pure myBot2
        "simpl" -> pure myBot
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
