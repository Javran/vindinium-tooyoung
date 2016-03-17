{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Vindinium
import Vindinium.Vdm
import Bot

import Data.String (fromString)
import Data.Text (pack)

-- data Cmd = Training Settings (Maybe Int) (Maybe Board)
--         | Arena Settings
--         deriving (Show, Eq)
type Cmd = (Settings, GameMode)

cmdSettings :: Cmd -> Settings
cmdSettings = fst

settings :: Parser Settings
settings = Settings <$> argument (pack <$> str) (metavar "KEY")
                    <*> (fromString <$> strOption (long "url" <> value "http://vindinium.org"))

trainingCmd :: Parser Cmd
trainingCmd = (,) <$> settings
                  <*> (GMTraining <$> optional (option auto (long "turns"))
                                  <*> pure Nothing)

arenaCmd :: Parser Cmd
arenaCmd = (,) <$> settings <*> pure GMArena

cmd :: Parser Cmd
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        (progDesc "Run bot in arena mode" ))
    )

runCmd :: Cmd -> IO ()
runCmd c  = do
    let cfg = (vdmConfig <$> settingsKey <*> settingsUrl) $ cmdSettings c
    s <- runVdm cfg VState $ playGame (snd c) randomBot
    print s

main :: IO ()
main =
    execParser opts >>= runCmd
  where
    opts = info (cmd <**> helper) idm
