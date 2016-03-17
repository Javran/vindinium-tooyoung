{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Vindinium
import Vindinium.Vdm
import Bot

import Data.String (fromString)
import Data.Text (pack)

data Cmd = Training Settings (Maybe Int) (Maybe Board)
         | Arena Settings
         deriving (Show, Eq)

cmdSettings :: Cmd -> Settings
cmdSettings (Training s _ _) = s
cmdSettings (Arena s) = s

settings :: Parser Settings
settings = Settings <$> (Key <$> argument (pack <$> str) (metavar "KEY"))
                    <*> (fromString <$> strOption (long "url" <> value "http://vindinium.org"))

trainingCmd :: Parser Cmd
trainingCmd = Training <$> settings
                       <*> optional (option auto (long "turns"))
                       <*> pure Nothing

arenaCmd :: Parser Cmd
arenaCmd = Arena <$> settings

cmd :: Parser Cmd
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        (progDesc "Run bot in arena mode" ))
    )

runCmd :: Cmd -> IO ()
runCmd c  = do
    let vdmConfig = (VConfig <$> settingsKey <*> settingsUrl) $ cmdSettings c
    s <- runVdm vdmConfig VState $
        case c of
            (Training _ t b) -> playTrainingEff t b randomBot
            (Arena _)        -> playArenaEff randomBot
    print s

main :: IO ()
main =
    execParser opts >>= runCmd
  where
    opts = info (cmd <**> helper) idm
