{-# LANGUAGE DeriveAnyClass #-}
module Login where

-- import Servant
-- import Servant.API
-- import Servant.API.Generic
-- import Servant.Server.Generic
-- import Env
-- import Servant.HTML.Lucid
-- import Lucid as L
-- import Lucid.Html5 as L
-- import GHC.Generics
-- import Data.Maybe
-- import           Web.FormUrlEncoded          (FromForm)
-- import Data.Text (Text)
-- import qualified Data.Map.Strict as Map
-- import Control.Monad.Validate
-- import qualified Data.Text as T

-- data FormErrors = FormErrors (Map.Map Field [Text]) deriving (Eq, Show)

-- instance Semigroup FormErrors where
--   (<>) (FormErrors a) (FormErrors b) = FormErrors $ Map.unionWith (<>) a b

-- mkErr :: Field -> Text -> FormErrors
-- mkErr k v = FormErrors $ Map.singleton k [v]

-- mkErrs :: Field -> [Text] -> FormErrors
-- mkErrs k vs = FormErrors $ Map.singleton k vs

-- noErrors :: FormErrors
-- noErrors = FormErrors Map.empty

-- hasError :: FormErrors -> Field -> Bool
-- hasError (FormErrors e) n = Map.member n e

-- lookupErrors :: FormErrors -> Field -> [Text]
-- lookupErrors (FormErrors e) n =
--   fromMaybe [] $ Map.lookup n e

-- -- type FormError = Text

-- data Field = Phone deriving (Eq, Show, Ord)

-- data PhoneForm = PhoneForm
--   { phone :: !(Maybe Text)
--   } deriving (Eq, Show, Generic, FromForm)



-- data Routes route = Routes
--   { rGetPhoneForm :: route :- "staff" :> "login" :> (Get '[HTML] (Html ()))
--   , rPostPhoneForm :: route :- "staff" :> "login" :> ReqBody '[FormUrlEncoded] PhoneForm :> (Get '[HTML] (Html ()))
--   } deriving (Generic)

-- server :: Routes (AsServerT AppM)
-- server = Routes
--   { rGetPhoneForm = getPhoneForm
--   , rPostPhoneForm = undefined
--   }

-- htmlShell inner = do
--   doctypehtml_ $ do
--     head_ $ do
--       meta_ [ charset_ "utf-8" ]
--       meta_ [ name_ "viewport"
--             , content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
--             ]
--       link_ [ rel_ "stylesheet",
--               href_  "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css",
--               integrity_ "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh",
--               crossorigin_ "anonymous"
--             ]
--     body_ $ do
--       inner
--       script_ [ src_ "https://code.jquery.com/jquery-3.4.1.slim.min.js"
--               , integrity_ "sha384-J6qa4849blE2+poT4WnyKhv5vZF5SrPo0iEjwBvKU7imGFAV0wwj1yYfoRSJoZ+n"
--               , crossorigin_ "anonymous"] $ do
--         ("" :: String)
--       script_ [ src_ "https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js"
--               , integrity_ "sha384-Q6E9RHvbIyZFJoft+2mJbHaEWldlvI9IOYy5n3zV9zzTtmI3UksdQRVvoxMfooAo"
--               , crossorigin_ "anonymous "] $ do
--         ("" :: String)
--       script_ [ src_ "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js"
--               , integrity_ "sha384-wfSDF2E50Y2D1uUdj0O3uMBJnjuUD4Ih7YwaYd1iqfktj0Uod8GCExl3Og8ifwB6"
--               , crossorigin_ "anonymous"] $ do
--         ("" :: String)

-- getPhoneForm :: AppM (Html ())
-- getPhoneForm = pure $ htmlShell $ generatePhoneForm (PhoneForm { phone = Nothing }) noErrors

-- postPhoneForm :: PhoneForm -> AppM (Html ())
-- postPhoneForm f = do
--   (runValidateT validatePhoneForm f) >>= \case
--     Left e -> pure $ htmlShell $ generatePhoneForm f e
--     Right r -> pure $ htmlShell $ div_ "establish session"


-- validatePhoneForm :: (MonadValidate FormErrors m)
--                   => PhoneForm
--                   -> m PhoneForm
-- validatePhoneForm f@PhoneForm{..} =
--   case phone of
--     Nothing -> refute $ mkErr Phone "Please enter your registered phone number"
--     Just _ -> pure f

-- isInvalid :: With p => FormErrors -> Field -> p -> p
-- isInvalid e n inner =
--   if hasError e n
--   then with inner [ class_ " is-invalid " ]
--   else inner

-- -- invalidFeedback :: FormErrors -> Field ->
-- invalidFeedback e n =
--   case lookupErrors e n of
--     [] -> mempty
--     xs -> do
--       div_ [ class_ "invalid-feedback" ] $ do
--         toHtml $ T.intercalate "; " xs

-- generatePhoneForm :: PhoneForm -> FormErrors -> Html ()
-- generatePhoneForm PhoneForm{..} e = do
--   div_ [ class_ "container-fluid" ] $ do
--     div_ [ class_ "row justify-content-center" ] $ do
--       div_ [ class_ "col-10" ] $ do
--         h1_ "Specify title"
--         form_ [ action_ "/staff/login" ] $ do
--           div_ [ class_ "form-group" ] $ do
--             label_ "Phone number"
--             isInvalid e Phone $ input_ [ type_ "tel", class_ "form-control", maxlength_ "10", value_ (fromMaybe "" phone) ]
--             small_ "Please use the phone number authorised by Goa Government"
--             div_ [ class_ "invalid-feedback" ] "Something comes here"
--           button_ [ type_ "submit", class_ "btn btn-primary"] "Generate OTP"


