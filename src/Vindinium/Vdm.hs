{-# LANGUAGE TypeOperators, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, PartialTypeSignatures, NamedWildCards #-}
module Vindinium.Vdm where

import Vindinium.Types
import qualified Vindinium.Types as VT

import Data.Typeable
import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Lift
import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Void
import Network.HTTP.Client
import Data.Default
import Vindinium.Board.Summary
import Vindinium.Board.ShortestPath

data VdmState = VState
  { vStarted :: Bool
  , vSummary :: Summary
  , vShortestPathInfo :: ShortestPathInfo
  }  deriving (Show)

instance Default VdmState where
    -- these 2 pieces of info (set to error)
    -- will be made available once the game is started
    def = VState
            False
            (error "summary not available")
            (error "spi not available")

data VdmConfig = VConfig -- TODO: hide constructor?
  { vcKey :: Key
  , vcUrl :: T.Text
  , vcMgr :: Manager
    -- ^ leave this blank, runVdm is responsible for creating it properly
  }

-- accepts a VdmState (maintained locally) and a game state,
-- it might return a move or nothing if cannot decide for now
-- NOTE: VPlanner have access to VdmState since Vdm is a State monad
-- we leave it as an input value to save user from doing extraction themselves
type VPlanner s = VdmState -> GameState -> Vdm s (Maybe Dir)

instance Show VdmConfig where
    show (VConfig k u _) = show [ ("key" :: String, k)
                                , ("url", u)
                                ]

vdmConfig :: Key -> Url -> VdmConfig
vdmConfig k u = VConfig k u (error "manager not initialized")

type Vdm s a = Eff
  (  Reader VdmConfig
  :> State s
  :> Lift IO
  :> Void) a

-- type BotE = VT.GameState -> Vdm Dir

runVdm :: (Typeable s) => VdmConfig -> s -> Vdm s a -> IO (s, a)
runVdm c s m = do
    -- inject manager right before we start.
    mgr <- newManager defaultManagerSettings
    let newConf = c { vcMgr = mgr }
    runLift (runState s (runReader m newConf))

io :: MonadIO m => IO a -> m a
io = liftIO

askVConfig :: (Member (Reader VdmConfig) r) => Eff r VdmConfig
askVConfig = ask

getVState :: (Member (State s) r, Typeable s) => Eff r s
getVState = get

putVState :: (Member (State s) r, Typeable s) => s -> Eff r ()
putVState = put

modifyVState :: (Member (State s) r, Typeable s) => (s -> s) -> Eff r ()
modifyVState = modify
