module Stats where

import Lucid
import Lucid.Html5
import Data.Text (Text)
import Control.Monad
import qualified Data.List as DL
import qualified Data.Text as T
import Control.Lens
import Data.String (fromString)
import Env (FormId(..))
import Data.String.Conv

htmlShell inner = do
  doctypehtml_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]
      meta_ [ name_ "viewport"
            , content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
            ]
      link_ [ rel_ "stylesheet",
              href_  "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css",
              integrity_ "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh",
              crossorigin_ "anonymous"
            ]
      title_ "Goa Covid Household Survey | Supported by Vacation Labs | Conducted by Department of Health, Govt of Goa"
    body_ $ do
      inner
      script_ [ src_ "https://code.jquery.com/jquery-3.4.1.slim.min.js"
              , integrity_ "sha384-J6qa4849blE2+poT4WnyKhv5vZF5SrPo0iEjwBvKU7imGFAV0wwj1yYfoRSJoZ+n"
              , crossorigin_ "anonymous"] $ do
        ("" :: String)
      script_ [ src_ "https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js"
              , integrity_ "sha384-Q6E9RHvbIyZFJoft+2mJbHaEWldlvI9IOYy5n3zV9zzTtmI3UksdQRVvoxMfooAo"
              , crossorigin_ "anonymous "] $ do
        ("" :: String)
      script_ [ src_ "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js"
              , integrity_ "sha384-wfSDF2E50Y2D1uUdj0O3uMBJnjuUD4Ih7YwaYd1iqfktj0Uod8GCExl3Og8ifwB6"
              , crossorigin_ "anonymous"] $ do
        ("" :: String)

pageShell inner = htmlShell $ do
  div_ [ class_ "container-fluid"] $ do
    div_ [ class_ "row justify-content-center" ] $ do
      div_ [ class_ "col-12 col-sm-12 col-md-6", style_ "padding-top: 10px;"] $ do
        a_ [href_ "https://www.vacationlabs.com/", target_ "blank"]$ do
          img_ [ src_ "https://www.vacationlabs.com/wp-content/uploads/2013/01/logo-250x291.png", title_ "Vacation Labs" ]
        hr_ []
        inner

page lastUpdated (blockGroups :: [(Text, Int, [(Text, Int, FormId)])]) total = pageShell $ do
  div_ [class_ "jumbotron"] $ do
    div_ [class_ "container-fluid"] $ do
      h1_ $ toHtml $ (humanize total <> " survey responses")
      p_ [class_ "lead"]  $ toHtml ("12 Talukas" :: Text)
      p_ [class_ "lead"]  $ toHtml ("574 Sub-divisions (villages/cities)" :: Text)
      p_ [class_ "lead"]  $ toHtml ("4,000+ waddos / municipal wards" :: Text)
      p_ [class_ "lead"]  $ toHtml ("6,700+ surveyors. " :: Text)
      hr_ []
      p_ [class_ "small"] $ do
        toHtmlRaw $ ("Goa Covid-19 Household survey. Conducted by Department of Health, Goa Govt. Supported by <a href='https://www.vacationlabs.com'>Vacation Labs</a>. " :: Text)
        br_ []
        toHtml lastUpdated


  h2_ "Block-wise totals"
  table_ [ class_ "table table-striped" ] $ do
    thead_ [ class_ "thead-dark" ]$ do
      tr_ $ do
        th_ $ toHtml ("Block name *" :: Text)
        th_ [style_ "text-align: right;"] $ toHtml ("Total survey responses **" :: Text)
    tbody_ $ do
      forM_ blockGroups $ \(blockName, blockTotal, _) -> do
        tr_ $ do
          td_ $ toHtml $ blockName <> " (total)"
          td_ [style_ "text-align: right;"] $ toHtml $ humanize blockTotal
    tfoot_ [style_ "font-size: 80%;"]$ do
      tr_ $ do
        td_ $ toHtml ("* Corresponds approximately to a Taluka or a very large city" :: Text)
        td_ $ toHtml ("** This is live data (with a time lag), and is subject to revision. We will remove duplicate entries, incorrect entries, etc at the end of the survey time period." :: Text)


  h2_ "Village/City-wise totals"

  table_ [ class_ "table table-striped" ] $ do
    thead_ [ class_ "thead-dark" ]$ do
      tr_ $ do
        th_ $ toHtml ("Form name *" :: Text)
        th_ [style_ "text-align: right;"] $ toHtml ("Total survey responses **" :: Text)
    tbody_ $ do
      forM_ blockGroups $ \(blockName, blockTotal, rows) -> do
        tr_ $ do
          td_ [style_ "font-weight: bold;"] $ toHtml $ blockName <> " (total)"
          td_ [style_ "font-weight: bold; text-align: right;"] $ toHtml $ humanize blockTotal
        forM_ rows $ \(fname, cnt, zid) -> do
          tr_ $ do
            td_ [ style_ "text-align: left; padding-left: 2em;"] $ do
              a_ [ href_ ("/forms/" <> (toS $ show $ unFormId zid)), target_ "blank"] $ do
                toHtml fname
            td_ [ style_ "text-align: right;"]$ toHtml $ humanize cnt
    tfoot_ [style_ "font-size: 80%;"]$ do
      tr_ $ do
        td_ $ toHtml ("* Corresponds approximately to a Waddo (sub-division of a village), or municipal ward, or in some cases even a large housing society"  :: Text)
        td_ $ toHtml ("** This is live data (with a time lag), and is subject to revision. We will remove duplicate entries, incorrect entries, etc at the end of the survey time period." :: Text)



humanize :: Int -> T.Text
humanize d = fromString $ if d < 0
                  then "(-) " <> formatPositiveDecimal (-d)
                  else formatPositiveDecimal d
  where
    formatPositiveDecimal x =  let (beforeDecimal, afterDecimal) = over _1 (DL.reverse . (DL.drop 1) . addCommas . DL.reverse) $ DL.span (/= '.') (show x)
                               in beforeDecimal <> afterDecimal
    addCommas :: String -> String
    addCommas s = let (x, y) = DL.splitAt 3 s
                  in if y==""
                     then ',':x
                     else (',':x) <> (addCommas y)


waddoPage lastUpdated (_, _, flink, formDisplayName) total rows = pageShell $ do
  div_ [class_ "jumbotron"] $ do
    div_ [class_ "container-fluid"] $ do
      h1_ $ toHtml formDisplayName
      p_ [class_ "lead"]  $ toHtml $ humanize total <> " survey responses"
      hr_ []
      p_ [class_ "small"] $ do
        toHtmlRaw $ ("Goa Covid-19 Household survey. Conducted by Department of Health, Goa Govt. Supported by <a href='https://www.vacationlabs.com'>Vacation Labs</a>. " :: Text)
        br_ []
        toHtml lastUpdated

  h2_ "Totals split by Waddo / Municipal area / Building name"

  table_ [ class_ "table table-striped" ] $ do
    thead_ [ class_ "thead-dark" ]$ do
      tr_ $ do
        th_ $ toHtml ("Form name *" :: Text)
        th_ [style_ "text-align: right;"] $ toHtml ("Total survey responses **" :: Text)
    tbody_ $ do
      forM_ rows $ \(waddoName, cnt) -> do
        tr_ $ do
          td_ [] $ toHtml waddoName
          td_ [style_ "text-align: right;"] $ toHtml $ humanize cnt
    tfoot_ [style_ "font-size: 80%;"]$ do
      tr_ $ do
        td_ $ toHtml ("* Corresponds approximately to a Waddo (sub-division of a village), or municipal ward, or in some cases even a large housing society"  :: Text)
        td_ $ toHtml ("** This is live data (with a time lag), and is subject to revision. We will remove duplicate entries, incorrect entries, etc at the end of the survey time period." :: Text)
