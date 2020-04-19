module Env where

import Control.Monad.Reader
import System.Console.Concurrent
import UnliftIO (MonadIO, liftIO)
import Data.String.Conv
import Data.String
import Data.Pool
import Database.PostgreSQL.Simple
import UnliftIO
import Network.HTTP.Types as HT
import Network.HTTP.Client as HC
import Control.Retry as Retry
import Control.Monad.Catch as E (MonadMask, Handler(..))
import Control.Concurrent.TokenBucket
import qualified Data.ByteString as BS
import GHC.Word

data Env = Env
  { envDbPool :: Pool Connection
  , envTokenBucket :: !TokenBucket
  , envBurstSize :: !Word64
  , envInvRate :: !Word64
  }

type AppM = ReaderT Env IO

sayLine :: (MonadIO m, Outputable s, IsString s, Semigroup s) => s -> m ()
sayLine s = liftIO $ outputConcurrent $ s <> "\n"

withDb :: (Connection -> AppM a) -> AppM a
withDb action = do
  pool <- envDbPool <$> ask
  withRunInIO $ \runInIO -> do
    withResource pool (runInIO . action)

withRateLimiting :: AppM a -> AppM a
withRateLimiting action = do
  Env{..} <- ask
  liftIO $ tokenBucketWait envTokenBucket envBurstSize envInvRate
  action

withEnv :: (Env -> IO a) -> IO a
withEnv action = do
  withPool $ \envDbPool -> do
    envTokenBucket <- newTokenBucket
    let envBurstSize = 10
        envInvRate = round (1e6 / 0.6667)
    action Env{..}

runApp :: AppM a -> IO a
runApp action = withEnv $ \env ->
  runReaderT action env

withPool :: (MonadUnliftIO m) => (Pool Connection -> m a) -> m a
withPool action = do
  pool <- liftIO $ createPool
    (connectPostgreSQL "dbname=goa-covid user=goa-covid password=goa-covid host=localhost")
    Database.PostgreSQL.Simple.close
    1 -- number of stipes
    (fromRational 20) --  number of seconds a connection should remain in pool
    10 -- max number of connections per stripe.
  finally (action pool) (liftIO $ destroyAllResources pool)

zohoRetryPolicy :: RetryPolicy
zohoRetryPolicy =
  (Retry.exponentialBackoff 1000000) <> (Retry.limitRetries 10)


data ThrottlingException = ThrottlingException deriving (Eq, Show)

instance Exception ThrottlingException

retryOnTemporaryNetworkErrors :: (MonadIO m, E.MonadMask m, m ~ AppM, StringConv a BS.ByteString) => m (Response a) -> m (Response a)
retryOnTemporaryNetworkErrors action = Retry.recovering
  zohoRetryPolicy
  [const $ E.Handler httpExceptionHandler, const $ E.Handler throttlingHandler]
  (const wrappedAction)
  where
    wrappedAction = do
      res <- withRateLimiting action
      if BS.isInfixOf "Request limit exceeded" (toS $ responseBody res)
        then throwIO ThrottlingException
        else pure res

    throttlingHandler (e :: ThrottlingException) = do
      sayLine ( "Too many requests" :: String)
      pure True

    httpExceptionHandler (e :: HttpException) = do
      sayLine ("Retry" :: String)
      pure $ case e of
        InvalidUrlException _ _ -> False
        HttpExceptionRequest req c ->
          case c of
            -- Never retry. Seems like a logical bug in the requst
            -- or a permanent issue with the networking environment
            TooManyRedirects _ -> False
            OverlongHeaders -> False
            InvalidStatusLine _ -> False
            InvalidHeader _ -> False
            InternalException _ -> False
            TlsNotSupported -> False
            WrongRequestBodyStreamSize _ _ -> False
            InvalidProxyEnvironmentVariable _ _ -> False
            InvalidProxySettings _ -> False

            -- Always retry. Even if this was a POST/PUT/DELETE request, because
            -- all of these errors indicate that the request didn't event reach
            -- the remove server.
            ConnectionTimeout -> True
            ConnectionFailure _ -> True
            ProxyConnectException _ _ _ -> True
            InvalidDestinationHost _ -> True

            -- Retry ONLY in the case of GET requests. Retrying in the case of
            -- POST/PUT/DELETE might result in the action being performed again,
            -- resulting in unexpected results.
            ResponseTimeout -> isGet req
            NoResponseDataReceived -> isGet req
            ResponseBodyTooShort _ _ -> isGet req
            InvalidChunkHeaders -> isGet req
            IncompleteHeaders -> isGet req
            HttpZlibException _ -> isGet req
            ConnectionClosed -> isGet req
            StatusCodeException resp _ ->
              case (HT.statusCode $ HC.responseStatus resp) of -- (resp ^. W.responseStatus . W.statusCode) of
                408 -> True -- request timeout
                409 -> True -- Conflict
                412 -> True -- precondition failed
                417 -> True -- Expectation failed
                420 -> True -- Enhance your calm
                429 -> True -- Too many requests
                _ -> False

isGet :: Request -> Bool
isGet req = HT.methodGet == (HC.method req)
