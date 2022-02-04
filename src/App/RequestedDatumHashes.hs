module App.RequestedDatumHashes (add, remove, set, get, RequestedDatumHashes) where

import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

type DatumHash = Text

type DatumHashes = Set DatumHash

type RequestedDatumHashes = MVar DatumHashes

modifyRequestedHashes :: MonadIO m => (DatumHashes -> DatumHashes) -> RequestedDatumHashes -> m ()
modifyRequestedHashes f requestedHashes =
    liftIO $
        modifyMVar_ requestedHashes $ \hashSet ->
            pure $ f hashSet

add :: MonadIO m => [DatumHash] -> RequestedDatumHashes -> m ()
add newHashes = modifyRequestedHashes (`Set.union` Set.fromList newHashes)

remove :: MonadIO m => [DatumHash] -> RequestedDatumHashes -> m ()
remove removedHashes = modifyRequestedHashes (`Set.difference` Set.fromList removedHashes)

set :: MonadIO m => [DatumHash] -> RequestedDatumHashes -> m ()
set newHashes requestedHashes =
    liftIO $ modifyMVar_ requestedHashes $ \_hashSet -> pure (Set.fromList newHashes)

get :: MonadIO m => RequestedDatumHashes -> m DatumHashes
get = liftIO . readMVar

-- TODO: tryReadMVar
