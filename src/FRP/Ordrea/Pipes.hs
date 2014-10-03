module FRP.Ordrea.Pipes
  ( networkToPipe
  ) where
import Control.Applicative
import Control.Monad (forever)

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
