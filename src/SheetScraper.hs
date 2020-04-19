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

-- import Network.Google.Drive.Types

type AppScopes =
  '["https://www.googleapis.com/auth/drive.file"]

noLogger :: _ -> _ -> IO ()
noLogger = const $ const $ pure ()

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


fetchVillageUserMapping sprId = do
  vrange <- send $ spreadsheetsValuesGet sprId "Teams"
  let (errs, rows) = partitionEithers $ DL.zipWith extract (DL.tail $ vrange ^. vrValues) [1..]
  pure $ DL.groupBy (\x y -> (x ^. _2) == (y ^. _2)) $ DL.sortOn (^. _2) rows
  -- pure (errs, rows)
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
