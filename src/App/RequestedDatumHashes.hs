module App.RequestedDatumHashes where

import Data.Text (Text)
import Data.Set (Set)
import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Set as Set
import Control.Concurrent.MVar (modifyMVar_, readMVar)

type DatumHash = Text

type DatumHashes = Set DatumHash

type RequestedDatumHashes = MVar DatumHashes

modifyRequestedHashes :: MonadIO m => (DatumHashes -> DatumHashes) -> RequestedDatumHashes -> m ()
modifyRequestedHashes f requestedHashes =
  liftIO $ modifyMVar_ requestedHashes $ \hashSet ->
    pure $ f hashSet

add :: MonadIO m => [DatumHash] -> RequestedDatumHashes -> m ()
add newHashes = modifyRequestedHashes (`Set.union` (Set.fromList newHashes))

remove :: MonadIO m => [DatumHash] -> RequestedDatumHashes -> m ()
remove removedHashes = modifyRequestedHashes (`Set.difference` (Set.fromList removedHashes))

set :: MonadIO m => [DatumHash] -> RequestedDatumHashes -> m ()
set newHashes requestedHashes =
  liftIO $ modifyMVar_ requestedHashes $ \hashSet -> pure (Set.fromList newHashes)

get :: MonadIO m => RequestedDatumHashes -> m DatumHashes
get = liftIO . readMVar
-- TODO: tryReadMVar
