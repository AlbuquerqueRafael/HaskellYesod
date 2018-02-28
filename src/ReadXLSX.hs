{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module ReadXLSX where

import Codec.Xlsx
import ExcelDate
import qualified Data.ByteString.Lazy as L
import Control.Lens
import Data.Typeable
import Data.Text as T

fileName :: FilePath
fileName = "../Cronograma_T1.xlsx"

spreadsheetName :: Text
spreadsheetName = "Sheet1"

printElements :: [String] -> IO()
printElements = mapM_ putStrLn

printListElements :: [[String]] -> IO()
printListElements [[]] = return ()
printListElements (x:xs) = do printElements x
                              putStrLn "----------------------------------------------------------"
                              printListElements xs

main :: IO ()
main = do
    list <- readLine 2 1 [[]]
    printListElements list

readLine :: Int -> Int-> [[String]] -> IO [[String]]
readLine line column list = do
    bs <- L.readFile fileName
    let value = toXlsx bs ^? ixSheet spreadsheetName .
                ixCell (line, column) . cellValue . _Just
    if (column == 1 && (convertCellValueToString value) == Nothing) then return (Prelude.init list)
    else
        if (column == 12) then readLine (line + 1) 1 (list ++ [[]])
        else readLine line (column + 1) (Prelude.init list ++ [Prelude.last list ++ [removingMaybeType value]])

getCellValue :: CellValue -> String
getCellValue (CellText a) = T.unpack(a)
getCellValue (CellDouble a) = show (dateFromDouble a)

removingMaybeType :: Maybe (CellValue) -> String
removingMaybeType (Just a) = getCellValue a
removingMaybeType Nothing = ""

convertCellValueToString:: Maybe(CellValue) -> Maybe(String)
convertCellValueToString input = fmap (getCellValue) (input)