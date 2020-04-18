module CsvExporter where

import ZohoScraper (downloadAllCsvReports)
import System.Environment

main :: IO ()
main = do
  putStrLn "starting"
  outDir <- getEnv "OUTDIR"
  downloadAllCsvReports outDir
