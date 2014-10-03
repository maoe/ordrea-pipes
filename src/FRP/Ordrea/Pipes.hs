module FRP.Ordrea.Pipes
  ( networkToPipe
  ) where
import Control.Applicative
import Control.Monad (forever)
import Data.Monoid (mconcat)

import FRP.Ordrea
import Pipes
import qualified Pipes.Prelude as P

-- | Convert an ordrea event network to a pipe.
networkToPipe
  :: MonadIO m
  => (Event a -> SignalGen (Event b))
  -- ^ Ordrea network
  -> Pipe a b m r
networkToPipe network = do
  inputEvent <- liftIO newExternalEvent
  sample <- liftIO $ start $ do
    ev <- network =<< externalE inputEvent
    return $ eventToBehavior ev
  P.mapM (go inputEvent sample) >-> P.concat
  where
    go ext sample a = liftIO $ do
      triggerExternalEvent ext a
      sample

-- | Convert a pipe to an ordrea event network.
pipeToNetwork
  :: Pipe a b IO ()
  -> Event a
  -> SignalGen (Event b)
pipeToNetwork pipe ev = do
  outputEvent <- liftIO newExternalEvent
  pipeE <- generatorE $ go outputEvent <$> ev
  outputE <- externalE outputEvent
  return $ justE $ mconcat
    [ Nothing <$ pipeE
    , Just <$> outputE
    ]
  where
    go ext a = liftIO $ runEffect $
      yield a >-> pipe >-> for cat (lift . triggerExternalEvent ext)
