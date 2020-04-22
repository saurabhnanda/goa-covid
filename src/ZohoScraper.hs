{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiWayIf #-}
module ZohoScraper where

import Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client
import Network.HTTP.Types as HT
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
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad (void, forM_, forM, mapM)
import Database.PostgreSQL.Simple
import UnliftIO
import Data.CaseInsensitive as CI(original)
import qualified Data.List.Split as Split
import Control.Lens
import Data.Aeson.Lens
import qualified Streamly.Prelude as S
import qualified Streamly as S
import Data.Functor.Identity
import GHC.Exts (fromList, toList)
import Control.Monad.Reader (local)
-- import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as C8
import qualified Data.Char as Char
import qualified SheetScraper as Sheet
import qualified Network.Google as Google hiding (setBody)
import Control.Monad.Reader (ask)
import Debug.Trace
import qualified Stats
import Servant.HTML.Lucid
import Lucid (Html(..), toHtmlRaw)
import qualified Data.Text.IO as T
import qualified System.Directory as Dir
import Control.Concurrent.TokenBucket

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
  , rStats :: route :- Get '[HTML] (Html ())
  , rWaddoStats :: route :- "forms" :> Capture "formId" Int :> Get '[HTML] (Html ())
  , rNavyStats :: route :- "navy" :> Get '[HTML] (Html ())
  } deriving (Generic)

server :: Routes (AsServerT AppM)
server = Routes
  { rInvitationEmailHandler = handleInivitationEmail
  , rStats = stats
  , rWaddoStats = waddoStats
  , rNavyStats = navyStats
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
         let accessCode = parseAccessCodeEmail invHtml
         sayLine $ "Access code: " <> invTo <> " => " <> toS accessCode
         googleEnv <- envGoogle <$> ask
         Google.runResourceT $
           Google.runGoogle googleEnv $
           Sheet.publishAccessCode invTo accessCode
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

makePost :: (MonadIO m) => Request -> m _
makePost req = liftIO $ do
  mgr <- getGlobalManager
  (flip httpLbs) mgr $
    setMethod "POST" $
    withHeaders $
    req

makePut :: (MonadIO m) => BSL.ByteString -> Request -> m _
makePut pload req = liftIO $ do
  mgr <- getGlobalManager
  (flip httpLbs) mgr $
    setMethod "PUT" $
    setBody pload $
    withHeaders $
    req


makeGet :: MonadIO m => Request -> m _
makeGet req = liftIO $ do
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


parseAccessCodeEmail :: Text -> Text
parseAccessCodeEmail email =
  -- email <-  BS.readFile "access_code.html"
  let tags = parseTags email
  in fromTagText $ DL.head $ DL.filter (tagText isCode) tags
  where
    isCode x = DL.all Char.isDigit $ (toS x :: String)

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


changeRolesForUsers :: AppM _
changeRolesForUsers = local (\r -> r{envBurstSize = 50, envInvRate = round (1e6 / 8)}) $ do
  zuids :: [(Only ZohoUserId)] <- getZohoUserIds
  results <- S.toList $ S.maxThreads 50 $ S.asyncly $
             S.filter isInterestingError $
             (flip S.mapMaybeM) (S.fromList zuids) $ \(Only zuid) -> do
    eRes :: (Either SomeException (Response BSL.ByteString)) <- try $ do
      sayLine $ "trying " <> unZohoUserId zuid
      req <- parseRequest $ "https://forms.zoho.com/REMOVED/user/" <> toS (unZohoUserId zuid) <> "/role"
      retryOnTemporaryNetworkErrors $ makePost $ setBody (Aeson.encode pload) req
    case eRes of
      Left e -> pure $ Just $ toS $ show e
      Right res -> do
        if | ok200 == responseStatus res -> do
               updateFlag zuid
               pure Nothing
           | status400 == responseStatus res -> do
               case eitherDecode (responseBody res) of
                 Left e -> pure $ Just $ "Error in decoding response " <> (toS $ show e)
                 Right (v :: Aeson.Value) ->
                   case v ^? (key "error") . _String of
                     Nothing -> pure $ Just "unknown error"
                     Just e ->
                       if | "already assigned" `T.isInfixOf` e -> do
                              updateFlag zuid
                              pure Nothing
                          | otherwise -> pure $ Just e
           | otherwise -> do
               pure $ Just $ toS $ responseBody res
  pure results
  where
    isInterestingError e = True
      -- case e of
      --   Nothing -> False
      --   Just x -> Prelude.not (("transfer" `T.isInfixOf` x) || ("not accessed" `T.isInfixOf` x))

    getZohoUserIds = withDb $ \conn -> do
      liftIO $ query_ conn "select zoho_user_id from users where zoho_user_id is not null and not is_respondent"

    pload = Aeson.object [ "role" Aeson..= ("respondent" :: Text)]

    updateFlag zuid = withDb $ \conn -> do
      liftIO $ void $ execute conn "update users set is_respondent=true where zoho_user_id=?" (Only zuid)

shareForm :: FormLinkName -> [Text] -> AppM _
shareForm flink emails = do
  existingEmails <- fetchExistingEmails
  let finalEmails = (DL.\\) (DL.nub emails) existingEmails
  req <- mkFormRequest flink $ Just "share/private/users"
  S.toList $
    S.maxThreads 5 $
    S.asyncly $
    S.mapMaybeM (go req) $
    S.fromList $
    Split.chunksOf 20 $
    finalEmails
  where
    go req es = do
      res <- retryOnTemporaryNetworkErrors $
             makePost $
             setBody (Aeson.encode $ Aeson.object [ "emailids" Aeson..= es ]) $
             addHeaders [ajaxHeader] $
             req

      if (responseStatus res) == ok200
        then do updateDb es
                pure Nothing
        else pure $ Just $ (es, responseBody res)

    fetchExistingEmails = withDb $ \conn -> do
      liftIO $ DL.map (\(Only x) -> x) <$>
        (query conn "select username from users_forms where link_name = ?" (Only flink))
        -- (query conn "select u.username from users_forms uf, users u, forms f where uf.user_id = u.id and uf.form_id=f.id and f.link_name=? and u.username in ?" (flink, In emails))
    updateDb es = withDb $ \conn -> liftIO $ do
      forM_ es $ \e -> do
        execute conn "insert into users_forms(username, link_name) values(?, ?)" (e, flink)
      -- liftIO $ execute conn "insert into users_forms(form_id, user_id) select f.id, u.id from forms f, users u where f.link_name=? and u.username in ?" (flink, In es)


-- shareFormsToUsers :: FormLinkName -> [Text] -> AppM ()
-- shareFormsToUsers formName = do
--   linkName <- getFormLink
--   users :: [(ZohoUserId, Text)] <- getUsers
--   forM_ (Split.chunksOf 20 users) $ \us -> do
--     shareForm fid $ DL.map snd us
--     updateFlag $ DL.map fst us
--   where

--     getFormLink = do
--       (query conn "select link_name from forms where id = ? and not is_shared" fid) >>= \case
--         [Only (linkName :: Text)] -> pure linkName
--         x -> Prelude.error $ "Some error in fetching form's link_name: " <> show x

--     getUsers = do
--       query conn "select id, username from users u, users_forms uf where u.id = uf.user_id and uf.form_id=?" (Only fid)

-- shareForm :: FormId -> [Text] -> IO ()
-- shareForm formId emails = do
--   req <- parseRequest $ "https://forms.zoho.com/REMOVED/form/" <> show (unFormId formId) <> "/share/private/users"
--   void $ makePost $ setBody (Aeson.encode pload) req
--   where
--     pload = Aeson.object [ "emailids" Aeson..= emails ]

-- storeUserMappingInDb :: Connection -> SheetRow -> IO (ZohoUserId, FormId)
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


-- createZohoUsersWhereMissed :: AppM _
-- createZohoUsersWhereMissed = withDb $ \conn -> do
--   rows :: [Only Text] <- liftIO $ query_ conn "select username from users where zoho_user_id is null"
--   forM rows createUser

superAdminUserId :: ZohoUserId
superAdminUserId = ZohoUserId "489260000000002001"

-- baseFormId :: ZohoFormId
-- baseFormId = ZohoFormId "489260000000017190"

baseFormLink :: FormLinkName
baseFormLink = FormLinkName "ToBeDuplicated"


mkFormRequest :: (MonadThrow m) => FormLinkName -> Maybe Text -> m _
mkFormRequest (FormLinkName fname) mfragment =
  let f = maybe fname (\x -> fname <> "/" <> x) mfragment
  in parseRequest $ "https://forms.zoho.com/REMOVED/form/" <> toS f

ajaxHeader :: HT.Header
ajaxHeader = ("X-Requested-With", "XMLHttpRequest")

fetchForm :: FormLinkName -> AppM Value
fetchForm fname = do
  req <- mkFormRequest fname Nothing
  res <- retryOnTemporaryNetworkErrors $
         makeGet $
         addHeaders [ (hAccept, "application/zoho.forms.medium-v1+pageJson")
                    , ajaxHeader
                    ] $
         req
  if (responseStatus res) /= ok200
    then Prelude.error $ show res
    else pure $ fromJust $ (responseBody res) ^? (key "form")


changeFormOwnership :: FormLinkName -> ZohoUserId -> IO _
changeFormOwnership fname zuid = do
  req <- mkFormRequest fname (Just "admin")
  makePut (Aeson.encode pload) $
    addHeaders [ ajaxHeader, (hContentType, "application/json")] $
    req
  where
    pload = Aeson.object [ "user_id" Aeson..= (unZohoUserId zuid) ]


fetchAllForms :: IO _
fetchAllForms = do
  req <- parseRequest "https://forms.zoho.com/REMOVED/forms"
  res <- makeGet $ addHeaders [ajaxHeader, (hAccept, "zohoforms/small+json")] $ req
  pure $ (responseBody res) ^.. (key "forms") . (key "form") . values

changeOwnershipOfAllForms :: IO _
changeOwnershipOfAllForms = do
  fs <- fetchAllForms
  S.toList $
    S.maxThreads 20 $
    S.asyncly $
    S.mapMaybeM go $
    S.fromList $
    fs
  where
    go :: Value -> IO _
    go f = do
      case f ^? (key "link_name") . _String of
        Nothing -> Prelude.error $ "form without link name " <> show f
        Just l -> do
          sayLine l
          res <- changeFormOwnership (FormLinkName l) superAdminUserId
          if (responseStatus res == ok200)
            then pure Nothing
            else case (fmap (T.isInfixOf "current form owner as the new owner") $ (responseBody res) ^? (key "error") . _String) of
                   Just True -> pure Nothing
                   _ -> pure $ Just (l, responseStatus res, responseBody res)

deleteAllUnknownForms :: AppM _
deleteAllUnknownForms = do
  fs <- liftIO $ fetchAllForms
  S.toList $
    S.maxThreads 20 $
    S.asyncly $
    S.mapMaybeM (liftIO . deleteForm) $
    S.mapMaybeM go $
    S.fromList fs
  where
    go :: Value -> AppM (Maybe FormLinkName)
    go f = do
      case f ^? (key "link_name") . _String  of
        Nothing -> Prelude.error $ "form without link " <> show f
        Just flink ->
          if flink == unFormLinkName baseFormLink || flink == "Part1RibandarforTraining1" || flink == "TaleigaoForTraining"
          then do sayLine $ "SKIPPING " <> flink
                  pure Nothing
          else do
            (withDb $ \conn -> liftIO $ query @_ @(Only Int) conn "select id from forms where link_name=?" (Only flink)) >>= \case
              [] -> do
                sayLine $ "to be deleted " <> flink <> " - " <> (fromMaybe ""  $ f ^? (key "display_name") . _String)
                pure $ Just $ FormLinkName flink
              _ -> pure Nothing


deleteAllForms :: AppM _
deleteAllForms = do
  fs <- liftIO $ fetchAllForms
  S.toList $
    S.maxThreads 20 $
    S.asyncly $
    S.mapMaybeM (liftIO . deleteForm) $
    S.map go $
    S.fromList fs
  where
    go :: Value -> FormLinkName
    go f =
      FormLinkName $ fromJust $ f ^? (key "link_name") . _String

deleteForm :: FormLinkName -> IO _
deleteForm flink = do
  req <- mkFormRequest flink (Just "trash")
  res <- makePost $
         addHeaders [ajaxHeader] req
  if responseStatus res == ok200
    then pure $ Nothing
    else pure $ Just res


duplicateForm :: Text -> AppM (Either _ (Int, ZohoFormId, FormLinkName, Text))
duplicateForm fname = do
  req <- liftIO $ mkFormRequest baseFormLink Nothing
  res <- retryOnTemporaryNetworkErrors $ liftIO $
         makePost $
         setBody (Aeson.encode pload) $
         addHeaders [ajaxHeader] $
         req
  if | (responseStatus res) == created201 ->
         do f <- fetchForm $ FormLinkName $ fromJust $ (responseBody res) ^? (key "form") . (key "link_name") . _String
            (Right . Prelude.head) <$> (saveKnownForm f)
     | otherwise ->
         pure $ Left res

  where
    pload = Aeson.object [ "form" Aeson..= Aeson.object [ "display_name" Aeson..= fname ]]
    saveKnownForm f = withDb $ \conn -> do
      liftIO $ query conn "insert into forms(zoho_form_id, link_name, form_name) values(?, ?, ?) returning id, zoho_form_id, link_name, form_name"
        ( fromJust $ f ^? (key "form_id") . _String
        , fromJust $ f ^? (key "link_name") . _String
        , fromJust $ f ^? (key "display_name") . _String
        )

fetchKnownFormByName :: Text -> AppM (Maybe (Int, ZohoFormId, FormLinkName, Text))
fetchKnownFormByName fname = withDb $ \conn -> do
  listToMaybe <$> (liftIO $ query conn "select id, zoho_form_id, link_name, form_name from forms where form_name=?" (Only fname))


fetchKnownFormById :: Int -> AppM (Maybe (Int, ZohoFormId, FormLinkName, Text))
fetchKnownFormById fid = withDb $ \conn -> do
  listToMaybe <$> (liftIO $ query conn "select id, zoho_form_id, link_name, form_name from forms where id=?" (Only fid))

changeWaddoNames :: FormLinkName -> [Text] -> AppM _
changeWaddoNames flink waddoNames = do
  f <- fetchForm flink
  req <- mkFormRequest flink Nothing
  res <- retryOnTemporaryNetworkErrors $
    makePut (Aeson.encode $ pload f) $
    addHeaders [(hAccept, "application/zoho.forms.medium-v2+pageJson"), ajaxHeader] $
    req
  if (responseStatus res) == ok200
    then pure Nothing
    else pure $ Just res
  where
    emptyObject = Aeson.Object $ fromList []
    jsonChoices = (flip DL.map) waddoNames $ \t ->
      Aeson.object [ "id" Aeson..= ("" :: Text)
                   , "value" Aeson..= t
                   , "formula_val" Aeson..= ("" :: Text)
                   ]
    pload f =
      let fields = f ^..  (key "pages") . (key "page") . (nth 0) . key ("fields") . values
          newFields = (flip DL.map) fields $ \v ->
            if v ^? (key "sequence_number") . _Number == Just 2
            then v & (key "choices") .~ (Aeson.toJSON jsonChoices)
            else emptyObject
          withoutKeys = Aeson.Object $ f ^. _Object & sans ("enc_scheduled_status") . (sans "save_button_label") . (sans "show_save_button")
          newForm = withoutKeys & (key "pages") . (key "page") . (nth 0) . key ("fields") .~ (Aeson.toJSON newFields)
      in Aeson.object [ "form" Aeson..= newForm ]


createReport :: FormLinkName -> Text -> AppM _
createReport flink displayName = do
  req <- parseRequest "https://forms.zoho.com/REMOVED/reports"
  res <- makePost $
         setBody (Aeson.encode pload) $
         addHeaders [ajaxHeader] $
         req
  if responseStatus res == created201
    then do sayLine $ "REPORT CREATED: " <> displayName
            let reportLink = (fromJust $ (responseBody res) ^? (key "report") . (key "link_name") . _String)
                reportZohoId = show $ (fromJust $ (responseBody res) ^? (key "report") . (key "report_id") . _Integer)
            void $ updateDb reportLink reportZohoId
            pure Nothing
    else pure $ Just (flink, res)
  where
    updateDb reportLink reportZohoId = withDb $ \conn -> do
      liftIO $ execute conn "insert into reports (link_name, display_name, form_link_name, zoho_report_id) values(?, ?, ?, ?)" (reportLink, displayName, flink, reportZohoId)
    pload = Aeson.object
      [ "report" Aeson..= Aeson.object
        [ "display_name" Aeson..= displayName
        , "related_form" Aeson..= (unFormLinkName flink)
        ]
      ]


fetchKnownForms :: AppM [(Int, ZohoFormId, FormLinkName, Text)]
fetchKnownForms = withDb $ \conn -> do
  liftIO $ query_ conn "select id, zoho_form_id, link_name, form_name from forms"

fetchKnownReports :: AppM [(ReportLinkName, Text)]
fetchKnownReports = withDb $ \conn -> do
  liftIO $ query_ conn "select link_name, display_name from reports"

createAllReports :: AppM _
createAllReports = local (\r -> r{envBurstSize = 3, envInvRate = round (1e6 / 0.08)}) $ do
  pendingReports <- fetchPendingReports
  S.toList $
    S.maxThreads 10 $
    S.asyncly $
    S.mapMaybeM (\(flink, fname) -> createReport flink fname) $
    S.fromList $
    pendingReports
  where
    fetchPendingReports = withDb $ \conn -> do
      liftIO $ query_ conn "select link_name, form_name from forms f where not exists (select 1 from reports r where r.form_link_name=f.link_name)"


fetchReportCounts :: AppM _
fetchReportCounts = local (\r -> r{envBurstSize = 3, envInvRate = round (1e6 / 0.08)}) $ do
  knownReports <- fetchKnownReports
  S.toList $
    S.maxThreads 10 $
    S.asyncly $
    S.mapMaybeM go $
    S.fromList knownReports
  where
    go (rlink, displayName) = do
      req <- parseRequest $
             "https://forms.zoho.com/REMOVED/report/" <> (toS $ unReportLinkName rlink) <> "/records?start=1&pageSize=10"
      res <- makeGet $
             addHeaders [(hAccept, "application/json"), ajaxHeader] $
             req
      if responseStatus res /= ok200
        then pure $ Just (rlink, res)
        else do let cnt = (fromJust $ (responseBody res) ^? (key "report") . (key "total_records") . _Integer)
                sayLine $ "TOTAL RECORDS - " <> displayName <> " - " <> (toS $ show cnt)
                updateDb rlink cnt
                pure Nothing
    updateDb rlink cnt = withDb $ \conn -> do
      liftIO $ execute conn "update reports set total_records = ? where link_name = ?" (cnt, rlink)


fetchWaddoCounts :: FormLinkName -> [Text] -> AppM _
fetchWaddoCounts flink waddoNames = local (\r -> r{envBurstSize = 3, envInvRate = round (1e6 / 1)}) $ do
  S.toList $
    S.maxThreads 5 $
    S.asyncly $
    S.mapMaybeM go $
    S.fromList waddoNames
  where
    go waddoName = do
      req <- parseRequest $
             "https://forms.zoho.com/REMOVED/report/" <> (toS $ unFormLinkName flink) <> "_Report/records"
      res <- retryOnTemporaryNetworkErrors $ makePost $
             setBody (Aeson.encode $ pload waddoName) $
             addHeaders [(hAccept, "application/json"), ajaxHeader, (hContentType, "application/json")] $
             req
      if responseStatus res /= ok200
        then pure $ Just (flink, res)
        else do let cnt = (fromJust $ (responseBody res) ^? (key "report") . (key "total_records") . _Integer)
                sayLine $ "WADDO COUNT - " <> waddoName <> " - " <> (toS $ show cnt)
                updateDb waddoName cnt
                pure Nothing
    pload waddoName = Aeson.object
      [ "entries" Aeson..= Aeson.object
        [ "criteria" Aeson..= Aeson.object
          [ "filter" Aeson..= Aeson.object
            [ "Dropdown" Aeson..= Aeson.object
              [ "operator" Aeson..= ("EQUALS" :: Text)
              , "value" Aeson..= waddoName
              ]
            ]
          ]
        ]
      ]
    updateDb waddoName cnt = withDb $ \conn -> do
      liftIO $ execute conn "update waddos set record_count = ? where form_link_name=? and waddo_name=?" (cnt, flink, waddoName)


--
fetchEntryCounts :: Bool -> AppM _
fetchEntryCounts flag = local (\r -> r{envBurstSize = 3, envInvRate = round (1e6/2)}) $ do
  knownForms <- fetchKnownForms
  S.toList $
    S.asyncly $
    S.concatMapM go2 $
    S.maxThreads 5 $
    S.asyncly $
    S.mapM go $
    S.fromList $
    DL.reverse $
    knownForms
  where
    go2 result = case result of
      Left e -> pure $ S.fromList [e]
      Right (_, flink, waddoNames) ->
        case flag of
          True -> S.fromList <$> (fetchWaddoCounts flink waddoNames)
          False -> pure $ S.fromList []
    go (_, _, rlink, displayName) = do
      req <- parseRequest $
             "https://forms.zoho.com/REMOVED/report/" <> (toS $ unFormLinkName rlink) <> "_Report/records?start=1&pageSize=10"
      res <- retryOnTemporaryNetworkErrors $ makeGet $
             addHeaders [(hAccept, "application/json"), ajaxHeader] $
             req
      if responseStatus res /= ok200
        then pure $ Left (rlink, res)
        else do let cnt = (fromJust $ (responseBody res) ^? (key "report") . (key "total_records") . _Integer)
                    waddoNames = (responseBody res) ^.. (key "report") . (key "fields") . (key "0") . (key "choices") . values . (key "value") . _String
                sayLine $ "TOTAL RECORDS - " <> displayName <> " - " <> (toS $ show cnt)
                updateDb rlink waddoNames cnt
                pure $ Right (displayName, rlink, waddoNames)
    updateDb flink waddoNames cnt = withDb $ \conn -> do
      liftIO $ do
        execute conn "update forms set total_records = ? where link_name = ?" (cnt, flink)
        forM_ waddoNames $ \waddoName -> do
          (query conn "select id from waddos where waddo_name = ? and form_link_name = ?" (waddoName, flink)) >>= \case
            [] -> void $ execute conn "insert into waddos(form_link_name, waddo_name) values(?, ?)" (flink, waddoName)
            [_ :: Only Int] -> pure ()


-- statsHelper :: AppM _
statsHelper rows = (flip catch) errhandler $ do
  lastUpdated <- liftIO $ T.readFile "last-updated"
  let total = DL.sum $ DL.map (^. _2) rows
      blockGroups = DL.sortOn (^. _1) $
                    DL.map withBlockTotals $
                    DL.groupBy (\x y -> x ^. _2 == y ^. _2) $
                    DL.sortOn (^. _2) $
                    DL.map splitName rows
  pure $ Stats.page lastUpdated blockGroups total
  where
    splitName :: (Text, Int, FormId) -> (Text, Text, Int, FormId)
    splitName (x, cnt, zid) =
      case T.split (\x -> x=='(' || x==')') x of
        formName:blockName:_ -> (T.strip formName, blockName, cnt, zid)
        _ -> (x, "Unknown block", cnt, zid)

    withBlockTotals :: [(Text, Text, Int, FormId)] -> (Text, Int, [(Text, Int, FormId)])
    withBlockTotals grp =
      let blockName = (DL.head grp) ^. _2
          blockTotal = DL.sum $ DL.map (^. _3) grp
          rows = DL.map (\(formName, blockName, cnt, zid) -> (formName, cnt, zid)) grp
      in (blockName, blockTotal, DL.sortOn (^. _1) rows)

    errhandler (e::SomeException) = pure $ toHtmlRaw $ show e


stats :: AppM _ -- [(Text, Int, [(Text, Int)])]
stats = do
  fetchStats >>= statsHelper
  where
    fetchStats :: AppM [(Text, Int, FormId)]
    fetchStats = withDb $ \conn -> do
      liftIO $ query_ conn "select form_name, total_records, id from forms where form_name not ilike '%naval%' order by reverse(form_name)"

navyStats = do
  fetchStats >>= statsHelper
  where
    fetchStats :: AppM [(Text, Int, FormId)]
    fetchStats = withDb $ \conn -> do
      liftIO $ query_ conn "select form_name, total_records, id from forms where form_name ilike '%naval%' order by reverse(form_name)"

waddoStats :: Int -> AppM _ -- [(Text, Int, [(Text, Int)])]
waddoStats formId = do
  rows <- fetchStats
  form <- fromJust <$> (fetchKnownFormById formId)
  lastUpdated <- liftIO $ T.readFile "last-updated"
  let total = DL.sum $ DL.map snd rows
  pure $ Stats.waddoPage lastUpdated form total rows
  where
    fetchStats :: AppM [(Text, Int)]
    fetchStats = withDb $ \conn -> do
      liftIO $ query conn "select w.waddo_name, w.record_count from waddos w, forms f where f.link_name=w.form_link_name and f.id=? order by w.record_count" (Only formId)


deactivateAllUsers :: AppM _
deactivateAllUsers = local (\r -> r{envBurstSize = 10, envInvRate = round (1e6/3)}) $ do
  zohoUserIds <- fetchUsers
  S.toList $
    S.maxThreads 10 $
    S.asyncly $
    S.mapMaybeM deactivateUser $
    S.fromList $
    -- DL.take 10
    zohoUserIds
  where
    fetchUsers = withDb $ \conn -> do
      liftIO $ query_ conn "select u.username, zoho_user_id, f.link_name from users u left join users_forms f on u.username=f.username where u.zoho_user_id is not null and  ((f.link_name is null) or (f.link_name not ilike '%navalbase')) and u.is_inactive=false"


deactivateUser :: (Text, ZohoUserId, Maybe FormLinkName) -> AppM _
deactivateUser (email, zuid, flink) = do
  req <- parseRequest $ "https://forms.zoho.com/REMOVED/user/" <> toS (unZohoUserId zuid) <> "/status"
  res <- retryOnTemporaryNetworkErrors $ makePost $
         setBody (Aeson.encode pload) $
         addHeaders [ajaxHeader] $
         req
  if | (responseStatus res) == ok200 -> do
         void updateDb
         sayLine $ "Deactivated " <> email <> " having access to " <> (maybe "no forms" unFormLinkName flink)
         pure Nothing
     | (responseStatus res) == status400 ->
         if (responseBody res) ^? (key "error") . _String == Just "This user is already inactive."
         then do sayLine $ "Already deactivated " <> email
                 pure Nothing
         else pure $ Just (email, res)
     | otherwise -> pure $ Just (email, res)
  where
    pload = Aeson.object
      [ "status" Aeson..= ("inactive" :: Text) ]
    updateDb = withDb $ \conn -> do
      liftIO $ execute conn "update users set is_inactive=true where zoho_user_id=?" (Only zuid)


downloadAllCsvReports :: FilePath -> IO  _
downloadAllCsvReports outDir = do
  linkNames <- ((DL.map T.strip) . T.lines) <$> (T.readFile "report-link-names.txt")
  tb <- newTokenBucket
  S.drain $
    S.asyncly $
    S.maxThreads threadCount $
    S.mapM (go tb) $
    S.fromList linkNames
  pure ()
  where
    threadCount = 5 :: Int
    go tb flink = do
      let outFile = (outDir <> "/" <> (toS flink) <> ".csv")
      (Dir.doesFileExist outFile) >>= \case
        True -> sayLine $ "Already exists " <> outFile
        False -> do
          tokenBucketWait tb 2 (round $ 1e6/0.08)
          (downloadCsvReport (FormLinkName flink) outFile) >>= \case
            Left (e :: SomeException) -> sayLine $ "Error in downloading " <> flink <> ": " <> (toS $ show e)
            Right _ -> pure ()


downloadCsvReport :: FormLinkName -> FilePath -> _
downloadCsvReport flink outFile = do
  req <- parseRequest $ "https://forms.zoho.com/exportdata?portalname=REMOVED&reportlinkname=" <>
         (toS $ unFormLinkName flink) <> "_Report&exporttype=csv&filename=" <> (toS $ unFormLinkName flink) <> ".csv"
  mgr <- getGlobalManager
  let finalReq = addHeaders [ajaxHeader] $
                 withHeaders req
  try $ withResponse finalReq mgr $ \breaderRes -> do
    Dir.removePathForcibly outFile
    S.drain $
      S.trace writeChunk $
      S.unfoldrM getNextChunk (responseBody breaderRes)
  where
    getNextChunk breader = do
      responseChunk <- brRead breader
      if responseChunk == mempty
        then do sayLine $ "Written " <> unFormLinkName flink
                pure Nothing
        else pure $ Just (responseChunk, breader)
    writeChunk responseChunk = do
      BS.appendFile outFile responseChunk
