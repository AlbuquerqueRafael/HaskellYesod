{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Handler.ReadXLSX where

import Codec.Xlsx
import Import
import ExcelDate
import qualified Data.ByteString.Lazy as L
import Control.Lens
import Data.Text as T
import Data.Aeson (ToJSON)

data Activity = Activity { 
    idActivity :: String,
    nome :: String,
    descricao :: String,
    dataHoraLiberacao :: UTCTime,
    dataHoraLimiteEnvioNormal :: UTCTime,
    dataHoraLimiteEnvioAtraso :: UTCTime,
    monitores :: [String],
    corretor :: String,
    dataInicioCorrecao :: UTCTime,
    dataEntregaCorrecao :: UTCTime,
    linksVideoAulas :: [String]
 } deriving (Show, Generic, ToJSON)

spreadsheetColumns :: Int
spreadsheetColumns = 12

fileName :: FilePath
fileName = "Cronograma_T1.xlsx"

spreadsheetName :: Text
spreadsheetName = "Sheet1"

parseAllElements :: [[String]] -> [Activity]
parseAllElements [] = []
parseAllElements (x:xs) = [parseToActivity x] Prelude.++ parseAllElements xs

readLine :: Int -> Int-> [[String]] -> IO [[String]]
readLine line column list = do
    bs <- L.readFile Handler.ReadXLSX.fileName
    let value = toXlsx bs ^? ixSheet spreadsheetName Prelude..
                ixCell (line, column) Prelude.. cellValue Prelude.. _Just
    if (column == 1 && (convertCellValueToString value) == Nothing) then return (Prelude.init list)
    else
        if (column == spreadsheetColumns) then readLine (line + 1) 1 (list Prelude.++ [[]])
        else readLine line (column + 1) (Prelude.init list Prelude.++ [Prelude.last list Prelude.++ [removingMaybeType value]])

getCellValue :: CellValue -> String
getCellValue (CellText a) = T.unpack(a)
getCellValue (CellDouble a) = show (dateFromDouble a)
getCellValue (CellBool _) = ""
getCellValue (CellRich _) = ""

removingMaybeType :: Maybe (CellValue) -> String
removingMaybeType (Just a) = getCellValue a
removingMaybeType Nothing = ""

convertCellValueToString :: Maybe(CellValue) -> Maybe(String)
convertCellValueToString input = fmap (getCellValue) (input)

parseToActivity :: [String] -> Activity
parseToActivity list = Activity {
    idActivity = list!!0, -- Pega o elemento do índice 0
    nome = list!!1, -- Pega o elemento do índice 1
    descricao = list!!2, -- Pega o elemento do índice 2
    dataHoraLiberacao = (read (list!!3)) :: UTCTime, -- Pega o elemento do índice 3
    dataHoraLimiteEnvioNormal = (read (list!!4)) :: UTCTime, -- Pega o elemento do índice 4
    dataHoraLimiteEnvioAtraso = (read (list!!5)) :: UTCTime, -- Pega o elemento do índice 5
    monitores = [list!!6], -- Pega o elemento do índice 6
    corretor = list!!7, -- Pega o elemento do índice 7
    dataInicioCorrecao = (read (list!!8)) :: UTCTime, -- Pega o elemento do índice 8
    dataEntregaCorrecao = (read (list!!9)) :: UTCTime, -- Pega o elemento do índice 9
    linksVideoAulas = [list!!10] -- Pega o elemento do índice 10
}

getActivitiesR :: Import.Handler Value
getActivitiesR = do
    list <- liftIO $ readLine 2 1 [[]]
    let activities = parseAllElements list
    returnJson $ activities