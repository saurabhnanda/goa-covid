{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Monad (void, forM_, forM, mapM)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import UnliftIO
import Data.CaseInsensitive as CI(original)
import qualified Data.List.Split as Split

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

printInvitationLink inv@InvitationEmail{..} = (flip catch) errhandler $ do
  if "invite" `T.isInfixOf` invSubject
    then do sayLine $ parseInvitationEmail invHtml
            pure "ok"
    else do sayLine $ "Ignored (" <> invTo <> ", " <> invSubject <> ")"
            pure "ignored"
  where
    errhandler (e :: SomeException) = do
      sayLine $ "Error in handling (" <> invTo <> ", " <> invSubject <> "): " <> (toS $ show e)
      throwM e


handleInivitationEmail inv@InvitationEmail{..} = (flip catch) errhandler $ do
  if | "invite" `T.isInfixOf` invSubject -> do
         let invLink = parseInvitationEmail invHtml
         saveInvitationLink invTo invLink
         resp <- doSignup inv (toS invLink)
         saveInvitationResponse invTo resp
         sayLine $ "Success (" <> invTo <> ", " <> invSubject <> ")"
         pure "ok"
     | "Code" `T.isInfixOf` invSubject -> do
         sayLine $ T.intercalate "\n" $ [invSubject, invHtml]
         pure "ok"
     | otherwise -> do
         sayLine $ "Ignored (" <> invTo <> ", " <> invSubject <> ")"
         pure "ignored"
  where
    errhandler (e :: SomeException) = do
      sayLine $ "Error in handling (" <> invTo <> ", " <> invSubject <> "): " <> (toS $ show e)
      throwM e

saveInvitationLink :: Text -> Text -> AppM ()
saveInvitationLink username lnk = void $ withDb $ \conn -> do
  liftIO $ execute conn qry (username, lnk)
  where
    qry = "update users set invitation_link=? where username=?"

saveInvitationResponse :: Text -> Response BSL.ByteString -> AppM ()
saveInvitationResponse username resp = void $ withDb $ \conn -> do
  liftIO $ execute conn qry (pload, username)
  where
    qry = "update users set invitation_response=? where username=?"
    pload = Aeson.object
      [ "headers" Aeson..= DL.map (\(k, v) -> (toS $ CI.original k, toS v) :: (Text, Text)) (responseHeaders resp)
      , "body" Aeson..= (toS $ responseBody resp :: Text)
      ]

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

makeGet :: Request -> IO _
makeGet req = do
  mgr <- getGlobalManager
  (flip httpLbs) mgr $
    setMethod "GET" $
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


doSignup inv u = (liftIO $ fetchSignupForm u) >>= (submitSignupForm inv u)

fetchSignupForm u = do
  mgr <- getGlobalManager
  req <- parseRequest u
  let finalReq = setMethod "GET" req
  res <- (flip httpLbs) mgr finalReq
  t <- getCurrentTime
  let (cj, _) = updateCookieJar res finalReq t $ createCookieJar []
  pure $ responseCookieJar res

-- submitSignupForm :: AppM _
submitSignupForm InvitationEmail{..} u cj = do
  mgr <- liftIO getGlobalManager
  req <- parseRequest "https://accounts.zoho.com/accounts/accinvite.ac"
  t <- liftIO getCurrentTime
  password <- fetchPassword
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
                 setBody (toS $ joinedpload iamcsr password) $ fst $ insertCookiesIntoRequest req cj t
      iamcsr = fromJust $ DL.lookup "iamcsr" $ C.parseCookies $ fromJust $ DL.lookup hCookie $ requestHeaders finalReq
  liftIO $ (flip httpLbs) mgr finalReq
  where
    joinedpload iamcsr password = BS.intercalate "&" $
                                  DL.map (\(k, mv) -> k <> "=" <> fromMaybe "" mv) $
                                  pload iamcsr password
    digest_ =
      case parseURI laxURIParserOptions (toS u) of
        Left e -> Prelude.error $ show e
        Right x -> fromJust $ DL.lookup "digest" $ x ^. queryL . queryPairsL

    fetchPassword = withDb $ \conn -> do
      r <- (liftIO $ query conn "SELECT password from users where username = ?" (Only invTo))
      case r of 
        [(Only password) :: Only Text] -> pure password
        _ -> Prelude.error "sql error"

    phone = DL.head $ T.splitOn "@" invTo
    
    pload iamcsr password = toQuery
      [ ("firstname" :: BSL.ByteString, "Please" :: BSL.ByteString)
      , ("lastname", "Provide")
      , ("password", toS password)
      , ("iamcsrcoo", toS iamcsr)
      , ("servicename", "ZohoForms")
      , ("digest", toS digest_)
      , ("is_ajax", "true")
      ]

data User = User
  { userRole :: !Text
  , userId :: !Text
  , userEmail :: !Text
  , userStatus :: !Text
  } deriving (Eq, Show)

data UserList = UserList { unUserList :: ![User] }  deriving (Eq, Show)

instance FromJSON UserList where
  parseJSON = withObject "Expecting object to parse into UserList"$ \o -> do
    unUserList <- o .: "users"
    pure UserList{..}

instance FromJSON User where
  parseJSON = withObject "Expecting object to parse into User"$ \o -> do
    userRole <- o .: "role"
    userId <- o .: "userid"
    userEmail <- o .: "email"
    userStatus <- o .: "status"
    pure User{..}

fetchUserList :: IO [User]
fetchUserList = do
  req <- parseRequest "https://forms.zoho.com/REMOVED/users"
  res <- makeGet req
  case eitherDecode (responseBody res) of
    Left e -> Prelude.error e
    Right r -> pure $ unUserList r

shareForm :: Text -> [Text] -> IO ()
shareForm formId emails = do
  users <- fetchUserList
  req <- parseRequest $ "https://forms.zoho.com/REMOVED/form/" <> toS formId <> "/share/private/users"
  void $ makePost $ setBody (Aeson.encode pload) req
  where
    pload = Aeson.object [ "emailids" Aeson..= emails ]


users :: [(Text, Text)]
users =
  [ ("9123456789", "REMOVED")
  ]


createPendingUsers :: Text -> AppM _
createPendingUsers batchKey = withDb $ \conn -> liftIO $ do
  rows :: [(Text, Text)] <- query_ conn qry
  forM_ rows $ \(username, password) -> do
    createUser username
    execute conn qry2 (batchKey, username)
  where
    qry = "SELECT username, password from users where not account_created"
    qry2 = "UPDATE users set batch_key=?, account_created=true where username=?"


data SheetRow = SheetRow
  { rowPhone :: !Text
  , rowSubdivision :: !Text -- village name
  , rowDivision :: !Text -- district / taluka name
  } deriving (Eq, Show)

newtype FormId = FormId Int deriving (Eq, Show, Generic, FromField, ToField)
newtype UserId = UserId Int deriving (Eq, Show, Generic, FromField, ToField)
data AppErr = ErrFormNotFound
            | ErrUserNotFound
            deriving (Eq, Show)

-- formIdForSheetRow :: SheetRow -> IO (Maybe FormId)
-- formIdForSheetRow row = _todo

-- userIdForSheetRow :: SheetRow -> IO (Maybe FormId)
-- userIdForSheetRow row = _todo

fetchZohoUserIds :: AppM [Either Text Text]
fetchZohoUserIds = do
  us :: [(UserId, Text)] <- getUserIds
  zohoUsers <- liftIO $ fetchUserList
  forM us $ \(appUid, username) -> do
    case ((flip DL.find) zohoUsers $ \User{..} -> userEmail == username) of
      Nothing -> do
        updateAccountCreatedFlag appUid
        pure $ Left username
      Just User{..} -> do
        updateZohoUserId appUid userId
        pure $ Right username
  where
    updateAccountCreatedFlag appUid  = withDb $ \conn -> do
      void $ liftIO $ execute conn "update users set account_created = false where id = ?" (Only appUid)

    updateZohoUserId appUid zohoUserId = withDb $ \conn -> do
      void $ liftIO $ execute conn "update users set zoho_user_id = ? where id = ?" (zohoUserId, appUid)

    getUserIds = withDb $ \conn -> do
      liftIO $ query_ conn "select id, username from users where zoho_user_id is null"

changeRolesForUsers :: [Text] -> AppM _
changeRolesForUsers conn = do
  zuids <- getUserIds
  forM zuids $ \ zuid -> liftIO $ do
    req <- parseRequest $ "https://forms.zoho.com/REMOVED/user/" <> zuid <> "/role"
    makePost $ setBody (Aeson.encode pload) req
  where
    getUserIds = withDb $ \conn -> do
      liftIO $ query_ conn "select zoho_user_id from users where not is_respondent limit 10"

    pload = Aeson.object [ "role" Aeson..= ("respondent" :: Text)]

    udpateFlag zuid = withDb $ \conn -> do
      liftIO $ void $ execute conn "update users set is_respondent=true where zoho_user_id=? in (?)" (Only zuid)

-- shareFormsToUsers :: Connection -> FormId -> IO ()
-- shareFormsToUsers conn fid = do
--   linkName <- getFormLink
--   users :: [(UserId, Text)] <- getUsers
--   forM_ (Split.chunksOf 20 users) $ \us -> do
--     shareForm fid $ DL.map snd us
--     updateFlag $ DL.map fst us
--   where
--     updateFlag us = do
--       void $ execute conn "update users_forms set is_shared=true where user_id in (?)" (Only (In us))

--     getFormLink = do
--       (query conn "select link_name from forms where id = ? and not is_shared" fid) >>= \case
--         [Only (linkName :: Text)] -> pure linkName
--         x -> Prelude.error $ "Some error in fetching form's link_name: " <> show x

--     getUsers = do
--       query conn "select id, username from users u, users_forms uf where u.id = uf.user_id and uf.form_id=?" (Only fid)

-- shareForm :: FormId -> [Text] -> IO ()
-- shareForm formId emails = do
--   req <- parseRequest $ "https://forms.zoho.com/REMOVED/form/" <> toS formId <> "/share/private/users"
--   void $ makePost $ setBody (Aeson.encode pload) req
--   where
--     pload = Aeson.object [ "emailids" Aeson..= emails ]

-- storeUserMappingInDb :: Connection -> SheetRow -> IO (UserId, FormId)
-- storeUserMappingInDb conn row@SheetRow{..} = do
--   (formIdForSheetRow row) >>= \case
--     Nothing -> pure $ Left ErrFormNotFound
--     Just fid -> do
--       (userIdForSheetRow row) >>= \case
--         Nothing -> pure $ Left ErrUserNotFound
--         Just uid -> do
--           cnt <- execute conn "update users_forms set form_id = ? where user_id = ?" (fid, uid)
--           if cnt==0
--             then void $ execute conn "insert into users_forms(user_id, form_id) values (?, ?) " (uid, fid)
--             else pure ()
--           pure (uid, fid)
