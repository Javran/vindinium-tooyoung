import Test.Hspec
import Vindinium.Board

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
    -- TODO: get all important object positions
    -- taverns / spawning point / mines / collect all non-blocks for preprocessing map
    -- TODO: shortest path search verification
