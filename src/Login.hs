module Login where

import Servant
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic
import Env
import Servant.HTML.Lucid
import Lucid
import GHC.Generics

data Routes route = Routes
  { rLogin :: route :- "staff" :> "login" :> (Get '[HTML] (Html ()))
  } deriving (Generic)

server :: Routes (AsServerT AppM)
server = Routes
  { rLogin = login
  }

login :: AppM (Html ())
login = pure $ do
  div_ "works"
