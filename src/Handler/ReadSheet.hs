{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, DeriveAnyClass #-}

module Handler.ReadSheet where 

---------------------------------------------------------------------------------
import Network.Google.Resource.Sheets.Spreadsheets.Get
import Network.Google.Sheets
import Network.Google

import Control.Lens           ((.~), (<&>), (^.))
import Data.Text              (Text, pack, unpack)
import System.IO              (stdout)
import Data.Aeson.Types
import Import

data Activity = Activity { 
    idActivity :: String,
    nome :: String,
    descricao :: String,
    dataHoraLiberacao :: String,
    dataHoraLimiteEnvioNormal :: String,
    dataHoraLimiteEnvioAtraso :: String,
    monitores :: [String],
    corretor :: String,
    dataInicioCorrecao :: String,
    dataEntregaCorrecao :: String,
    linksVideoAulas :: [String]
} deriving (Show, Generic, ToJSON)

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
parseToActivity list = Activity {
    idActivity = removeValue (list !! 0), -- Pega o elemento do índice 0
    nome = removeValue (list !! 1), -- Pega o elemento do índice 1
    descricao = removeValue (list !! 2), -- Pega o elemento do índice 2
    dataHoraLiberacao = removeValue (list !! 3), -- Pega o elemento do índice 3
    dataHoraLimiteEnvioNormal = removeValue (list !! 4), -- Pega o elemento do índice 4
    dataHoraLimiteEnvioAtraso = removeValue (list !! 5), -- Pega o elemento do índice 5
    monitores = [removeValue (list !! 6)], -- Pega o elemento do índice 6
    corretor = removeValue (list !! 7), -- Pega o elemento do índice 7
    dataInicioCorrecao = removeValue (list !! 8), -- Pega o elemento do índice 8
    dataEntregaCorrecao = removeValue (list !! 9), -- Pega o elemento do índice 9
    linksVideoAulas = if Prelude.length list > 10 then [removeValue (list !! 10)] else [""] -- Pega o elemento do índice 10
}

parseAllElements :: [[Value]] -> [Activity]
parseAllElements [] = []
parseAllElements (x:xs) = [parseToActivity x] Prelude.++ parseAllElements xs

removeValue :: Value -> String
removeValue (String a) = Data.Text.unpack(a)

getActivitiesR :: Import.Handler Value
getActivitiesR = do
    valueR <- liftIO $ exampleGetValue (Data.Text.pack(sheetId)) (Data.Text.pack(rangeSheet))
    let activities = parseAllElements (Prelude.tail (valueR^.vrValues))
    returnJson $ activities