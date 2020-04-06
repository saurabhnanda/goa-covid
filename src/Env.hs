module Env where

import Control.Monad.Reader

data Env = Env

type AppM = ReaderT Env IO
