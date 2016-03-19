{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Vindinium.Types
  ( Settings(..)
  , Key, Url
  , GameState(..)
  , GameId
  , Game(..)
  , Hero(..)
  , Dir(..)
  , GameMode(..)
  , reverseDir
  , applyDir
  , module Vindinium.Board
  ) where

import Data.Text (Text, pack, unpack)

import Data.Aeson
import Control.Monad
import Vindinium.Board
import Control.DeepSeq
import GHC.Generics

type Key = Text
type Url = Text

data Settings = Settings
  { settingsKey :: Key
  , settingsUrl :: Url
  } deriving (Show, Eq)

data GameMode
  = GMTraining (Maybe Int) (Maybe Board)
  | GMArena
    deriving (Show)

data GameState = GState
  { stateGame    :: Game
  , stateHero    :: Hero
  , stateToken   :: Text
  , stateViewUrl :: Text
  , statePlayUrl :: Text
} deriving (Show, Eq)

type GameId = Text

data Game = Game
  { gameId       :: GameId
  , gameTurn     :: Int
  , gameMaxTurns :: Int
  , gameHeroes   :: [Hero]
  , gameBoard    :: Board
  , gameFinished :: Bool
  } deriving (Show, Eq)

data Hero = Hero
  { heroId        :: HeroId
  , heroName      :: Text
  , heroUserId    :: Maybe Text
  , heroElo       :: Maybe Int
  , heroPos       :: Pos
  , lastDir       :: Maybe Dir
  , heroLife      :: Int
  , heroGold      :: Int
  , heroMineCount :: Int
  , heroSpawnPos  :: Pos
  , heroCrashed   :: Bool
  } deriving (Show, Eq)

data Dir = Stay | North | South | East | West
  deriving (Show, Eq, Read, Generic)

instance NFData Dir

reverseDir :: Dir -> Dir
reverseDir d = case d of
    North -> South
    South -> North
    West -> East
    East -> West
    Stay -> error "cannot reverse Stay"

applyDir :: Dir -> Coord -> Coord
applyDir d v@(x,y) = case d of
    Stay -> v
    North -> (x-1,y)
    South -> (x+1,y)
    West -> (x,y-1)
    East -> (x,y+1)

instance FromJSON GameState where
    parseJSON (Object o) =
        GState <$> o .: "game"
               <*> o .: "hero"
               <*> o .: "token"
               <*> o .: "viewUrl"
               <*> o .: "playUrl"
    parseJSON _ = mzero

instance FromJSON Game where
    parseJSON (Object o) =
        Game <$> o .: "id"
             <*> o .: "turn"
             <*> o .: "maxTurns"
             <*> o .: "heroes"
             <*> o .: "board"
             <*> o .: "finished"
    parseJSON _ = mzero

instance FromJSON Hero where
    parseJSON (Object o) =
        Hero <$> o .: "id"
             <*> o .: "name"
             <*> o .:? "userId"
             <*> o .:? "elo"
             <*> o .: "pos"
             <*> o .:? "lastDir"
             <*> o .: "life"
             <*> o .: "gold"
             <*> o .: "mineCount"
             <*> o .: "spawnPos"
             <*> o .: "crashed"
    parseJSON _ = mzero


instance ToJSON Dir where
    toJSON = String . pack . show

instance FromJSON Dir where
    parseJSON (String s)
      | str <- unpack s
      , ((d,[]):_) <- reads str = pure d
    parseJSON _ = mzero

