{-# LANGUAGE TypeOperators, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, PartialTypeSignatures, NamedWildCards #-}
module Vindinium.Vdm where

import Vindinium.Types
import qualified Vindinium.Types as VT

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Lift
import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Void
import Network.HTTP.Client

data VdmState = VState
  deriving (Show)

data VdmConfig = VConfig -- TODO: hide constructor?
  { vcKey :: Key
  , vcUrl :: T.Text
  , vcMgr :: Manager
    -- ^ leave this blank, runVdm is responsible for creating it properly
  }

instance Show VdmConfig where
    show (VConfig k u _) = show [ ("key" :: String, k)
                                , ("url", u)
                                ]

vdmConfig :: Key -> Url -> VdmConfig
vdmConfig k u = VConfig k u (error "manager not initialized")

type Vdm a = Eff
  (  Reader VdmConfig
  :> State VdmState
  :> Lift IO
  :> Void) a

type BotE = VT.GameState -> Vdm Dir

runVdm :: VdmConfig -> VdmState -> Vdm a -> IO (VdmState, a)
runVdm c s m = do
    -- inject manager right before we start.
    mgr <- newManager defaultManagerSettings
    let newConf = c { vcMgr = mgr }
    runLift (runState s (runReader m newConf))

io :: MonadIO m => IO a -> m a
io = liftIO

askVConfig :: (Member (Reader VdmConfig) r) => Eff r VdmConfig
askVConfig = ask

getVState :: (Member (State VdmState) r) => Eff r VdmState
getVState = get

putVState :: (Member (State VdmState) r) => VdmState -> Eff r ()
putVState = put

modifyVState :: (Member (State VdmState) r) => (VdmState -> VdmState) -> Eff r ()
modifyVState = modify
