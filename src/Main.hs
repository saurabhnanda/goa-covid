module Main where

import qualified Login
import Servant
import Servant.Server
import Servant.API
import Servant.Server.Generic
import Servant.API.Generic
import Env
import Data.Proxy
import Control.Monad.Reader
import UnliftIO (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp

type Routes =
  (ToServant Login.Routes AsApi)

server :: ServerT Routes AppM
server = (toServant Login.server)

routesProxy :: Proxy Routes
routesProxy = Proxy

main :: IO ()
main = do
  let env = Env
      appToHandler :: AppM a -> Handler a
      appToHandler action = liftIO $ runReaderT action env
  run 8000 $
    serve routesProxy $
    hoistServer routesProxy appToHandler server
