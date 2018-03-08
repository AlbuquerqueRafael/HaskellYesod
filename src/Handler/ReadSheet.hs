{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, DeriveAnyClass, ScopedTypeVariables, GADTs, MultiParamTypeClasses #-}

module Handler.ReadSheet where

import Network.Google.Resource.Sheets.Spreadsheets.Get
import Network.Google.Sheets
import Network.Google

import Control.Lens           ((.~), (<&>), (^.))
import Data.Text              (Text, pack, unpack)
import System.IO as T
import Data.Aeson.Types
import Import
import Handler.ExcelDate
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName, unSqlBackendKey, toSqlKey)

sheetId :: String
sheetId = "1s7DYZ2EmIL8gXIABRighKC8PHEjckMY1YONrN9eW-Pc"

rangeSheet :: String
rangeSheet = "Sheet1!A:K"

newtype ResponseMessage = ResponseMessage { message :: String } deriving (Show, Generic)

instance FromJSON ResponseMessage where
    parseJSON (Import.Object v) = ResponseMessage <$> v .: "message"
    parseJSON _ = empty

instance ToJSON ResponseMessage where
    toJSON (ResponseMessage message) = object ["message" .= message]

rmWorked :: ResponseMessage
rmWorked  = ResponseMessage { message = "Spreadsheet values updated successfully" }

rmError :: ResponseMessage
rmError = ResponseMessage { message = "Something wrong happened. Spreadsheet not updated." }

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
    (stringToDate (removeValue (list !! 3)))
    (stringToDate (removeValue (list !! 4)))
    (stringToDate (removeValue (list !! 5)))
    ([removeValue (list !! 6)])
    (removeValue (list !! 7))
    (stringToDate (removeValue (list !! 8)))
    (stringToDate (removeValue (list !! 9)))
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
    ms <- runDB $ selectList [] [] :: Import.Handler [Entity Activity]
    returnJson $ ms

getActivityR :: Import.Handler Value
getActivityR = do
    activityId <- runInputGet $ ireq textField "activityId"
    ms <- runDB $ selectList [ActivityActivityId ==. Data.Text.unpack(activityId)] [] :: Import.Handler [Entity Activity]
    returnJson $ ms

getUpdateActivitiesR :: Import.Handler Value
getUpdateActivitiesR = do
    runDB $ deleteWhere ([] :: [Filter Activity])
    valueR <- liftIO $ exampleGetValue (Data.Text.pack(sheetId)) (Data.Text.pack(rangeSheet))
    let activities = parseAllElements (Prelude.tail (valueR^.vrValues))
    saveAllActivities activities
    returnJson (toJSON rmWorked)
