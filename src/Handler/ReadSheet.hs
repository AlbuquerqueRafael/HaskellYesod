{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, DeriveAnyClass, ScopedTypeVariables, GADTs, MultiParamTypeClasses #-}

module Handler.ReadSheet where 

---------------------------------------------------------------------------------
import Network.Google.Resource.Sheets.Spreadsheets.Get
import Network.Google.Sheets
import Network.Google

import Control.Lens           ((.~), (<&>), (^.))
import Data.Text              (Text, pack, unpack)
import System.IO as T
import Data.Aeson.Types
import Import
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName, unSqlBackendKey, toSqlKey)

sheetId :: String
sheetId = "1s7DYZ2EmIL8gXIABRighKC8PHEjckMY1YONrN9eW-Pc"

rangeSheet :: String
rangeSheet = "Sheet1!A:K"

---------------------------------------------------------------------------------
-- | 
-- This gets the Information about an spreadsheet.
-- In order to be able to run these examples you need to
-- create a service acount from google's developers console
-- and copy the dowloaded json file to ~/.config/gcloud/application_default_credentials.json.
--
-- you must also share with your service the spreadsheet that you want to get the info of.
-- In order to do this you must share the sheet with the email address of your service
-- which is in your downloaded service config file.
--
-- after doing above step just pass the sreadsheet id to the function.
exampleGetSheet :: Text -> IO Spreadsheet
exampleGetSheet sheetID = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) Prelude.. (envScopes .~ spreadsheetsScope)
  runResourceT Prelude.. runGoogle env $
    send (spreadsheetsGet sheetID )

-- |
-- you pass the sheet id and a range (eg. "sheet1!A1:C3") in that sheet 
-- and it retreives the values in the specified range
exampleGetValue :: Text -> Text -> IO ValueRange 
exampleGetValue sheetID range = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) Prelude.. (envScopes .~ spreadsheetsScope)
  runResourceT Prelude.. runGoogle env $
    send  (spreadsheetsValuesGet sheetID range )

parseToActivity :: [Value] -> Activity
parseToActivity list = Activity 
    (removeValue (list !! 0)) 
    (removeValue (list !! 1))
    (removeValue (list !! 2))
    (removeValue (list !! 3))
    (removeValue (list !! 4))
    (removeValue (list !! 5))
    ([removeValue (list !! 6)])
    (removeValue (list !! 7))
    (removeValue (list !! 8))
    (removeValue (list !! 9))
    (if Prelude.length list > 10 then [removeValue (list !! 10)] else [""])

parseAllElements :: [[Value]] -> [Activity]
parseAllElements [] = []
parseAllElements (x:xs) = [parseToActivity x] Prelude.++ parseAllElements xs

removeValue :: Value -> String
removeValue (String a) = Data.Text.unpack(a)

saveActivity :: Activity -> HandlerT App IO (Key Activity)
saveActivity activity = do
    runDB $ insert $ activity

saveAllActivities :: [Activity] -> HandlerT App IO ()
saveAllActivities [] = return ()
saveAllActivities (x:xs) = do
    saveActivity x
    saveAllActivities xs

getActivitiesR :: Import.Handler Value
getActivitiesR = do
    valueR <- liftIO $ exampleGetValue (Data.Text.pack(sheetId)) (Data.Text.pack(rangeSheet))
    let activities = parseAllElements (Prelude.tail (valueR^.vrValues))
    saveAllActivities activities
    ms <- runDB $ selectList [] [] :: Import.Handler [Entity Activity]
    returnJson $ ms