module Env where

import Control.Monad.Reader
import System.Console.Concurrent
import UnliftIO (MonadIO, liftIO)
import Data.String.Conv
import Data.String

data Env = Env

type AppM = ReaderT Env IO

sayLine :: (MonadIO m, Outputable s, IsString s, Semigroup s) => s -> m ()
sayLine s = liftIO $ outputConcurrent $ s <> "\n"
