{-# LANGUAGE PartialTypeSignatures #-}
module Control
  ( module Control
  , module ZohoScraper
  , module Env
  )
where

import ZohoScraper
import Env
import SheetScraper
import qualified Streamly.Prelude as S
import qualified Streamly as S
import Network.Google hiding (setBody)
import Network.Google.Auth
import Network.HTTP.Client.TLS
import Network.Google.Sheets.Types
import Data.List as DL
import Control.Lens
import qualified Data.Text as T
import Data.Ord
import Data.Maybe
import qualified Data.List.Split as Split
import Control.Monad
import UnliftIO
import Database.PostgreSQL.Simple
import Data.Either
import Data.String.Conv

spreadsheetIds =
  [ ("Salcette", "spreadsheet_id_was_here")
  , ("Tiswadi", "spreadsheet_id_was_here")
  , ("Bardez", "spreadsheet_id_was_here")
  , ("Bicholim", "spreadsheet_id_was_here")
  , ("Sattari", "spreadsheet_id_was_here")
  , ("Pernem", "spreadsheet_id_was_here")
  , ("Dharbandora", "spreadsheet_id_was_here")
  , ("Mormugoa", "spreadsheet_id_was_here")
  , ("Ponda", "spreadsheet_id_was_here")
  , ("Quepem", "spreadsheet_id_was_here")
  , ("Sanguem", "spreadsheet_id_was_here")
  , ("Canacona", "spreadsheet_id_was_here")
  ]

createEnv = do
  creds <- fromFilePath "gauth.json"
  mgr <- getGlobalManager
  (allow (spreadsheetsScope ! driveFileScope)) <$> (newEnvWith creds noLogger mgr)

createForms blockName = do
  env <- createEnv
  let sprId = fromJust $ DL.lookup blockName spreadsheetIds
  waddoMapping <- runResourceT $ runGoogle env $ fetchVillageWaddoMapping sprId
  runApp $
    S.toList $
    S.maxThreads 40 $
    S.asyncly $
    S.mapMaybeM prepareForm $
    S.fromList $
    -- DL.take 1 $ 
    waddoMapping
  where
    prepareForm g = do
      let villageName = (DL.head g) ^. _2
          waddoNames = DL.nub $ DL.sortBy (comparing T.toLower) $ DL.map (T.strip . (^. _3)) g
          formName = villageFormName blockName villageName
      (duplicateFormIfRequired formName) >>= \case
        Left e -> pure $ Just e
        Right (_, _, flink, _) -> do
          (changeWaddoNames flink waddoNames) >>= \case
            Nothing -> do
              sayLine $ "Waddo names changed: " <> formName <> "\n" <> (T.intercalate "\n" waddoNames)
              pure Nothing
            Just r -> pure $ Just r

    duplicateFormIfRequired formName = do
      fetchKnownFormByName formName >>= \case
        Just x -> do
          sayLine $ "Already duplicated: " <> formName
          pure $ Right x
        Nothing -> do
          sayLine $ "Duplicating form: " <> formName
          duplicateForm formName >>= \case
            Left e -> pure $ Left e
            Right x -> do
              sayLine $ "Duplicated: " <> formName
              pure $ Right x

villageFormName blockName villageName = villageName <> " (" <> blockName <> ")"

credsFor x = ("p" <> x <> "@covid.vacationlabs.com", "covid@" <> (T.takeEnd 4 x))

createMissingUsers :: T.Text -> IO ()
createMissingUsers blockName = do
  env <- createEnv
  let sprId = fromJust $ DL.lookup blockName spreadsheetIds
  phones <-  runResourceT $ runGoogle env $ fetchAllPhones sprId
  runApp $ do
    results <- forM phones $ \phone -> do
      let (username, password) = credsFor phone
      withDb $ \conn -> liftIO $ try $ do
        execute conn "insert into users (username, password) values (?, ?)" (username, password)
    let (errs, successes) = partitionEithers results
        finalErrs = (flip DL.concatMap) errs $ \(e :: SqlError) ->
          if T.isInfixOf "duplicate key" (toS $ show e)
          then []
          else [e]
    sayLine $ "(Existing, Inserted, Errors): " <> show (DL.length errs - DL.length finalErrs, DL.length successes, DL.length finalErrs)
  pure ()

mapUsersToVillages blockName = do
  env <- createEnv
  let sprId = fromJust $ DL.lookup blockName spreadsheetIds
  rows :: [[(Int, T.Text, [T.Text])]] <-  runResourceT $ runGoogle env $ fetchVillageUserMapping sprId
  runApp $  S.toList $
    S.maxThreads 10 $
    S.asyncly $
    S.mapM go $
    S.fromList $
    rows
  where
    -- go :: [(Int, T.Text, [T.Text])] -> _ 
    go grp = do
      let (_, villageName, _) = (DL.head grp)
      (fetchKnownFormByName (villageFormName blockName villageName)) >>= \case
        Nothing -> pure ["Unknown form " <> toS villageName]
        Just (_, _, flink, _) -> do
          let allphones = DL.concatMap (^. _3) grp
              emails = DL.map (fst . credsFor) allphones
          shareForm flink emails
