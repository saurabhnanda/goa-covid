module Env where

import Control.Monad.Reader
import System.Console.Concurrent
import UnliftIO (MonadIO, liftIO)
import Data.String.Conv
import Data.String
import Data.Pool
import Database.PostgreSQL.Simple
import UnliftIO

data Env = Env
  { envDbPool :: Pool Connection
  }

type AppM = ReaderT Env IO

sayLine :: (MonadIO m, Outputable s, IsString s, Semigroup s) => s -> m ()
sayLine s = liftIO $ outputConcurrent $ s <> "\n"

withDb :: (Connection -> AppM a) -> AppM a
withDb action = do
  pool <- envDbPool <$> ask
  withRunInIO $ \runInIO -> do
    withResource pool (runInIO . action)

runApp :: AppM a -> IO a
runApp action = do
  withPool $ \envDbPool -> do
    runReaderT action Env{..}


withPool :: (MonadUnliftIO m) => (Pool Connection -> m a) -> m a
withPool action = do
  pool <- liftIO $ createPool
    (connectPostgreSQL "dbname=goa-covid user=goa-covid password=goa-covid host=localhost")
    Database.PostgreSQL.Simple.close
    1 -- number of stipes
    (fromRational 20) --  number of seconds a connection should remain in pool
    10 -- max number of connections per stripe.
  finally (action pool) (liftIO $ destroyAllResources pool)
