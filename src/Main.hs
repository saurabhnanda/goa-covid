module Main where

-- import qualified Login
import qualified ZohoScraper
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
import System.Console.Concurrent

type Routes =
  (ToServant ZohoScraper.Routes AsApi)

server :: ServerT Routes AppM
server = (toServant ZohoScraper.server)

routesProxy :: Proxy Routes
routesProxy = Proxy

startApp :: IO ()
startApp = withConcurrentOutput $ do
  withPool $ \envDbPool -> do
    let appToHandler :: AppM a -> Handler a
        appToHandler action = liftIO $ runReaderT action Env{..}
    putStrLn "Starting app..."
    run 8000 $
      serve routesProxy $
      hoistServer routesProxy appToHandler server

stopApp :: IO ()
stopApp = putStrLn "Stopping app..."

main :: IO ()
main = startApp
