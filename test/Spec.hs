import Test.Hspec
import Vindinium.Board
import Vindinium.Types
import Vindinium.Board.ShortestPath
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.List

{-# ANN module "HLint: ignore Redundant do" #-}

boardSample :: Board
boardSample = parseBoard 14 $ concat
  [ "############################"
  , "############################"
  , "  ########################  "
  , "    ##@1##  ####  ##  ##    "
  , "                @4          "
  , "    ##  $-        $4  ##    "
  , "##[]##                ##[]##"
  , "##[]##                ##[]##"
  , "    ##  $-        $-  ##    "
  , "                    @3      "
  , "    ##@2##  ####  ##  ##    "
  , "  ########################  "
  , "############################"
  , "############################"
  ]

main :: IO ()
main = hspec $ do
    describe "Vindinium.Board" $ do
        it "reads right hero positions" $ do
            (boardSample `atCoord` (3,3)) `shouldBe` HeroTile 1
            (boardSample `atCoord` (10,3)) `shouldBe` HeroTile 2
            -- let result2 = calcShortestPathInfo boardSample (3,3)
            -- print (findPathTo result2 (2,3))
    describe "Vindinium.Board.ShortestPath" $ do
        it "finds shortest paths correctly" $ do
            let srcCoord = (3,3)
                spi = calcShortestPathInfo boardSample srcCoord
            forM_ [(x,y) | x <- [0..13], y <- [0..13]] $ \(x,y) -> do
                case findPathTo spi (x,y) of
                    Nothing -> () `shouldBe` () -- nothing to expect ..
                    Just dirs ->
                        foldl (flip applyDir) srcCoord dirs `shouldBe` (x,y)
    -- TODO: get all important object positions
    -- taverns / spawning point / mines / collect all non-blocks for preprocessing map
