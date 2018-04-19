{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Clay as C
import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
import GHC.Generics (Generic)
import Lucid
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (middleware, scotty, get, json, html, param, ActionM, text)

dbName :: String
dbName = "mytracker.db"

data Tracker = Tracker 
  { site :: T.Text
  , page :: T.Text
  , hits :: Int
  }deriving (Show, Generic)
  
instance FromRow Tracker where
  fromRow = Tracker <$> field <*> field <*> field
  
instance ToJSON Tracker

  
selectAll :: IO [Tracker]
selectAll = do
  conn <- SQL.open dbName
  results <- SQL.query_ conn "SELECT * FROM Tracker" :: IO [Tracker]
  SQL.close conn
  return results
  
    
divCss = C.table C.# C.byClass "tablecss" C.? do
  C.borderCollapse  C.collapse
  
tdCss = C.td C.# C.byClass "tdcss" C.? do
  C.border           C.solid (C.px 1) C.black

renderTracker :: L.Text -> ActionM ()
renderTracker txt = do
  text txt
{-
selectOne site page = do 
  conn <- SQL.open dbName
  let selectReq = "SELECT * FROM Tracker WHERE site = (?) AND page = (?)"
  r <- query conn selectReq (Only ("test string 2" :: String)) :: IO [Name]
  SQL.close conn
  return r-}

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  middleware simpleCors

  get "/json" $ do
   trackers <- liftIO selectAll
   json trackers
   
  get "/tracker"  $ html $ renderText $ do
    doctype_
    html_ $ do
		body_ $ do
			  "test"
			  
   
  get "/" $ html $ renderText $ do
    doctype_
    html_ $ do
        header_ $ do
          style_ $ L.toStrict $ C.render divCss
          style_ $ L.toStrict $ C.render tdCss
        body_  $ do
			  h1_ "Tracker"
			  table_[class_ "tablecss"] $ do
				tr_ $ do
					td_[class_ "tdcss"] "site 1"
					td_[class_ "tdcss"] "page 1"
					td_[class_ "tdcss"] "42"
				tr_ $ do
					td_[class_ "tdcss"] "site 1"
					td_[class_ "tdcss"] "page 2"
					td_[class_ "tdcss"] "420"
				tr_ $ do
					td_[class_ "tdcss"] "site 2"
					td_[class_ "tdcss"] "page 1"
					td_[class_ "tdcss"] "98"
				tr_ $ do
					td_[class_ "tdcss"] "site 2"
					td_[class_ "tdcss"] "page 2"
					td_[class_ "tdcss"] "98"
			  a_[href_ "/json"] "get json"
					
			
					
				      
			  


