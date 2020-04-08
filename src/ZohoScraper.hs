{-# LANGUAGE PartialTypeSignatures #-}
module ZohoScraper where

import Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Client.TLS
import Data.Aeson as Aeson
import Text.HTML.TagSoup
import Data.List as DL
import Text.HTML.TagSoup.Match
import Data.Maybe
import Data.String.Conv
import Data.Time
import qualified Web.Cookie as C
import Debug.Trace
import URI.ByteString
import Control.Lens
import Servant
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic
import Env
import Servant.Multipart
import Data.Text (Text)
import UnliftIO (liftIO, catch, SomeException)
import Control.Monad.Catch (throwM)

data InvitationEmail = InvitationEmail
  { invHtml :: !Text
  , invTo :: !Text
  , invSubject :: !Text
  } deriving (Eq, Show)

instance FromMultipart Mem InvitationEmail where
  fromMultipart x = InvitationEmail
    <$> lookupInput "html" x
    <*> lookupInput "to" x
    <*> lookupInput "subject" x

data Routes route = Routes
  { rInvitationEmailHandler :: route :- "invitationEmailHandler" :> MultipartForm Mem InvitationEmail :> (Post '[JSON] String)
  } deriving (Generic)

server :: Routes (AsServerT AppM)
server = Routes
  { rInvitationEmailHandler = handleInivitationEmail
  }

handleInivitationEmail InvitationEmail{..} = (flip catch) errhandler $ do
  if "invite" `T.isInfixOf` invSubject
    then do liftIO $ doSignup (toS $ parseInvitationEmail invHtml)
            sayLine $ "Success (" <> invTo <> ", " <> invSubject <> ")"
            pure "ok"
    else do sayLine $ "Ignored (" <> invTo <> ", " <> invSubject <> ")"
            pure "ignored"
  where
    errhandler (e :: SomeException) = do
      sayLine $ "Error in handling (" <> invTo <> ", " <> invSubject <> "): " <> (toS $ show e)
      throwM e

commonHeders :: RequestHeaders
commonHeders =
  [ (hCookie, "Entire cookie string came here. It has now been purposely REMOVED before open-sourcing this code.")
  , ("X-ZCSRF-TOKEN", "Entire CSRF token came here. It has been purpose REMOVED before open-sourcing this code.")
  ]

withHeaders :: Request -> Request
withHeaders req = req { requestHeaders = (requestHeaders req) <> commonHeders }

setBody :: BSL.ByteString -> Request -> Request
setBody body req = req { requestBody = RequestBodyLBS body }

-- addHeaders :: Headers -> Request -> Request
addHeaders h req = req { requestHeaders = (requestHeaders req) <> h }

setMethod :: Method -> Request -> Request
setMethod m req = req { method = m }

makePost :: Request -> IO _
makePost req = do
  mgr <- getGlobalManager
  (flip httpLbs) mgr $
    setMethod "POST" $
    withHeaders $
    req

createUser :: Text -> IO _
createUser email = do
  req <- (parseRequest "https://forms.zoho.com/REMOVED/users")
  makePost $ setBody (Aeson.encode pload) $ req
  where
    pload = Aeson.object
      [ "emailaddresses" Aeson..= [ email ]
      ]


parseInvitationEmail :: Text -> Text
parseInvitationEmail email = do
  -- email <- BS.readFile "src/email.html"
  let tags = parseTags email
  fromAttrib "href" $ DL.head $ DL.filter (tagOpenLit "a" (DL.any isRelevantLink)) tags
  where
    isRelevantLink (_, l) =
      ("accounts.zoho.com" `T.isInfixOf` l) &&
      ("digest=" `T.isInfixOf` l) &&
      (Prelude.not $ "r=true" `T.isInfixOf` l)


doSignup u = fetchSignupForm u >>= (submitSignupForm u)

fetchSignupForm u = do
  mgr <- getGlobalManager
  req <- parseRequest u
  let finalReq = setMethod "GET" req
  res <- (flip httpLbs) mgr finalReq
  t <- getCurrentTime
  let (cj, _) = updateCookieJar res finalReq t $ createCookieJar []
  pure $ responseCookieJar res

-- submitSignupForm :: IO _
submitSignupForm u cj = do
  mgr <- getGlobalManager
  req <- parseRequest "https://accounts.zoho.com/accounts/accinvite.ac"
  t <- getCurrentTime
  let finalReq = addHeaders [ (hReferer, toS u)
                            , (hContentType, "application/x-www-form-urlencoded; charset=UTF-8")
                            , (hAccept, "*/*")
                            , ("Sec-Fetch-Dest", "empty")
                            , ("Sec-Fetch-Mode", "cors")
                            , ("Sec-Fetch-Size", "same-origin")
                            , ("Origin", "https://accounts.zoho.com")
                            , ("X-Requested-With", "XMLHttpRequest")
                            , ("Referrer Policy",  "no-referrer-when-downgrade")
                            , ("Accept-Language", "en-US,en;q=0.9")
                            , ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36 OPR/67.0.3575.115")

                            ] $
                 setMethod "POST" $
                 setBody (toS $ joinedpload iamcsr) $ fst $ insertCookiesIntoRequest req cj t
      iamcsr = fromJust $ DL.lookup "iamcsr" $ C.parseCookies $ fromJust $ DL.lookup hCookie $ requestHeaders finalReq
  (flip httpLbs) mgr finalReq
  where
    joinedpload iamcsr = BS.intercalate "&" $
                         DL.map (\(k, mv) -> k <> "=" <> fromMaybe "" mv) $
                         pload iamcsr
    digest_ =
      case parseURI laxURIParserOptions (toS u) of
        Left e -> Prelude.error $ show e
        Right x -> fromJust $ DL.lookup "digest" $ x ^. queryL . queryPairsL
    pload iamcsr = toQuery
      [ ("firstname" :: BSL.ByteString, "Saurabh" :: BSL.ByteString)
      , ("lastname", "Nanda")
      , ("password", "pilot@9911")
      , ("iamcsrcoo", toS iamcsr)
      , ("servicename", "ZohoForms")
      , ("digest", toS digest_)
      , ("is_ajax", "true")
      ]

