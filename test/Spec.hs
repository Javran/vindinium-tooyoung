import Test.Hspec
import Vindinium.Board
import Vindinium.Types
import Vindinium.Board.ShortestPath
import Vindinium.Board.Summary
import qualified Data.IntMap as IM
import Control.Monad
import Data.List
import Data.Maybe
import System.Random.MWC

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}

boardSample :: Board
boardSample = parseBoard 14 $ concat
  [ "############################" -- x = 0
  , "############################"
  , "  ########################  "
  , "    ##@1##  ####  ##  ##    "
  , "                @4          " -- x = 4
  , "    ##  $-        $4  ##    "
  , "##[]##                ##[]##"
  , "##[]##                ##[]##"
  , "    ##  $-        $-  ##    "
  , "                    @3      " -- x = 9
  , "    ##@2##  ####  ##  ##    "
  , "  ########################  "
  , "############################"
  , "############################" -- x = 13
  --         ^^ y = 4  ^^ y = 9
  ]

main :: IO ()
main = hspec $ do
    -- create a random generator for testing
    rng <- runIO createSystemRandom

    describe "Vindinium.Board" $ do
        it "reads right hero positions" $ do
            (boardSample `atCoord` (3,3)) `shouldBe` HeroTile 1
            (boardSample `atCoord` (10,3)) `shouldBe` HeroTile 2
    describe "Vindinium.Board.ShortestPath" $ do
        it "finds shortest paths correctly" $ do
            let srcCoord = (3,3)
                spi = calcShortestPathInfo boardSample srcCoord
            forM_ [(x,y) | x <- [0..13], y <- [0..13]] $ \(x,y) -> do
                case findPathTo spi (x,y) of
                    Nothing -> () `shouldBe` () -- nothing to expect ..
                    Just dirs -> do
                        let finalPos = foldM (\c d -> applyDir' boardSample d c) srcCoord dirs
                        -- heros cannot step into things like taverns and mines
                        -- so we just check if the thing we want is nearby (distance <=1)
                        ((<= 1) . coordDist (x,y)) <$> finalPos `shouldBe` Just True
                        -- randomly choose 5 ways going to the destionation and try again
                        void . replicateM 5 $ do
                            mpath <- findRandomPathTo rng spi (x,y)
                            mpath `shouldSatisfy` isJust
                            let Just path = mpath
                                finalPos' = foldM (\c d -> applyDir' boardSample d c) srcCoord path
                            ((<= 1) . coordDist (x,y)) <$> finalPos' `shouldBe` Just True

    describe "Vindinium.Board.Summary" $ do
        it "gives correct summary" $ do
            let s = summarize boardSample
            s `shouldBe` Summary
              { sSpawnPoints = IM.fromList [(1,(3,3)),(2,(10,3)),(3,(9,10)),(4,(4,8))]
              , sTaverns = [(7,12),(7,1),(6,12),(6,1)]
              , sMines = [(8,9),(8,4),(5,9),(5,4)]
              }
