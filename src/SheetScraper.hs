{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module SheetScraper where

import Network.Google.Resource.Sheets.Spreadsheets.Get
import Network.Google hiding (setBody)
import Control.Lens
import Network.Google.Auth
import Network.Google.Sheets.Types
import Network.Google.Resource.Sheets.Spreadsheets.Values.Get
import Control.Lens
import Data.Aeson.Lens
import Data.Text (Text)
import GHC.TypeLits
import Data.Aeson as Aeson
import Data.List as DL
import Data.Either
import Data.Ord
import Data.Text as T
import Network.Google.Resource.Sheets.Spreadsheets.Values.Append
import Env (sayLine)

-- import Network.Google.Drive.Types

type AppScopes =
  '["https://www.googleapis.com/auth/drive.file"]


withAppScopes x = allow (spreadsheetsScope ! driveFileScope) x

-- fetchVillageWaddoMapping :: (HasScope AppScopes a, a ~ Aeson.Value)
--                          => Text
--                         -> m [[a]]
fetchVillageWaddoMapping sprId = do
  vrange <- send $ spreadsheetsValuesGet sprId "Block%20%3E%20Village%20%3E%20Waddo"
  let (errs, rows) = partitionEithers $ DL.zipWith extract (DL.tail $ vrange ^. vrValues) [1..]
  pure $ DL.groupBy (\x y -> (x ^. _2) == (y ^. _2)) $ DL.sortOn (^. _2) rows
  where
    extract r i = case r of
      (String _):(String _):(String village):(String waddo):_ -> Right (i, village, waddo)
      x -> Left (i, r)


fetchAllPhones sprId = do
  vrange <- send $ spreadsheetsValuesGet sprId "Teams"
  let (errs, rows) = partitionEithers $ DL.zipWith extract (DL.tail $ vrange ^. vrValues) [1..]
  pure $ DL.concat rows
  where
    extract r i = case r of
      (String _):(String _):(String _):ps ->
        Right $ DL.concatMap cleanup ps
      x ->
        Left (i, r)

    cleanup (Aeson.String x) =
      if T.strip x==""
      then []
      else [T.strip x]


-- fetchVillageUserMapping :: _ -> _ [[(Int, Text, [Text])]]
fetchVillageUserMapping sprId = do
  vrange <- send $ spreadsheetsValuesGet sprId "Teams"
  let (errs, rows) = partitionEithers $ DL.zipWith extract (DL.tail $ vrange ^. vrValues) [1..]
  pure $ DL.groupBy (\x y -> (x ^. _2) == (y ^. _2)) $ DL.sortOn (^. _2) rows
  where
    extract r i = case r of
      (String _):(String village):(String _):ps ->
        Right (i, village, DL.concatMap cleanup ps)
      x ->
        Left (i, r)

    cleanup (Aeson.String x) =
      if T.strip x==""
      then []
      else [T.strip x]


publishAccessCode username accessCode = do
  let vrange = valueRange
               & vrValues .~ [[Aeson.String username, Aeson.String accessCode]]
      pload = (spreadsheetsValuesAppend "spreadsheet_id_was_here" vrange "Sheet1")
              & svaValueInputOption ?~ "USER_ENTERED"
              & svaInsertDataOption ?~ "INSERT_ROWS"
  sayLine $ show vrange
  send pload
