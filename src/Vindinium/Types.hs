{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Types
  ( Settings(..)
  , Key, Url
  , GameState(..)
  , GameId
  , Game(..)
  , HeroId
  , Hero(..)
  , Board(..)
  , Tile(..)
  , Pos(..)
  , Dir(..)
  , GameMode(..)
  , pprBoard
  ) where

import qualified Data.Text as T
import Data.Text (Text, pack, unpack)

import Data.Aeson
import Control.Monad
import Data.Char
import Data.List.Split

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

type HeroId = Int

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

data Board = Board
  { boardSize  :: Int
  , boardTiles :: [Tile] -- TODO: efficient board data structure?
  } deriving (Show, Eq)

data Tile
  = FreeTile
  | WoodTile
  | TavernTile
  | HeroTile HeroId
  | MineTile (Maybe HeroId)
    deriving (Show, Eq)

data Pos = Pos
  { posX :: Int
  , posY :: Int
  } deriving (Show, Eq)

data Dir = Stay | North | South | East | West
    deriving (Show, Eq, Read)

instance ToJSON Board where
    toJSON b  = object [ "size"  .= boardSize b
                       , "tiles" .= printTiles (boardTiles b)
                       ]

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

instance FromJSON Pos where
    {-parseJSON (Object o) = Pos <$> o .: "x" <*> o .: "y"-}
    {-AA 20140204 These seem to be labelled around the wrong way in the JSON-}
    parseJSON (Object o) = Pos <$> o .: "y" <*> o .: "x"
    parseJSON _ = mzero

instance FromJSON Board where
    parseJSON (Object o) = parseBoard <$> o .: "size" <*> o .: "tiles"
    parseJSON _ = mzero

instance ToJSON Dir where
    toJSON = String . pack . show

instance FromJSON Dir where
    parseJSON (String s)
      | str <- unpack s
      , ((d,[]):_) <- reads str = pure d
    parseJSON _ = mzero

parseBoard :: Int -> String -> Board
parseBoard s t =
    Board s $ map parse (chunksOf 2 t)
  where

    parse "  " = FreeTile
    parse "##" = WoodTile
    parse "[]" = TavernTile
    parse (xs@[a,b])
        | a == '@' = HeroTile $ ord b - ord '0'
        | a == '$' = MineTile (case b of
                                   '-' -> Nothing
                                   _ -> Just (ord b - ord '0'))
        | otherwise = error $ "parse: unknown tile pattern " ++ show xs
    parse _ = error "impossible"

printTiles :: [Tile] -> Text
printTiles = foldMap (T.pack . printTile)

printTile :: Tile -> String
printTile FreeTile = "  "
printTile WoodTile = "##"
printTile (HeroTile i) = "@" ++ show i
printTile TavernTile = "[]"
printTile (MineTile Nothing) = "$-"
printTile (MineTile (Just i)) = "$" ++ show i

pprBoard :: Board -> IO ()
pprBoard (Board s t) = do
    let rows = chunksOf s t
    putStrLn $ replicate 10 '=' ++ "Board"
    mapM_ (putStrLn . concatMap printTile) rows
    putStrLn ""

