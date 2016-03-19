import Test.Hspec
import Vindinium.Board
import Vindinium.Types
import Vindinium.Board.ShortestPath
import Control.Monad
import qualified Data.Map.Strict as Map

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
            let result = findShortestPaths boardSample (3,3)
            forM_ [0..13] $ \row -> do
                forM_ [0..13] $ \col -> do
                    case Map.lookup (row,col) result of
                        Nothing -> putStr " "
                        Just Nothing -> putStr "?"
                        Just (Just (PathInfo d _)) -> putStr (case d of
                                                                  Stay -> "+"
                                                                  _ -> (take 1 . show $ d))
                putStrLn ""
    -- TODO: get all important object positions
    -- taverns / spawning point / mines / collect all non-blocks for preprocessing map
    -- TODO: shortest path search verification
