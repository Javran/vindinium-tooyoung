{-# LANGUAGE TypeOperators, FlexibleContexts, PartialTypeSignatures, NamedWildCards, TypeFamilies #-}
module Vindinium.VdmEff where

import Vindinium.Types hiding (State)
import qualified Vindinium.Types as VT

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Lift
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Void

data VdmState = VState deriving (Show)

data VdmConfig = VConfig
  { vcKey :: Key
  , vcUrl :: T.Text
  } deriving (Show)

type VdmEff a = Eff
  (  Reader VdmConfig
  :> State VdmState
  :> Lift (ResourceT IO)
  :> Void) a

type BotE = VT.State -> VdmEff Dir

runVdmEff :: VdmConfig -> VdmState -> VdmEff a -> IO (VdmState, a)
runVdmEff c s m = runResourceT (runLift (runState s (runReader m c)))

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
