{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, DeriveAnyClass #-}
module Handler.Submission where

import System.Process
import Import
import System.Directory
import System.IO as T
import Control.Exception

data ResponseMessage = ResponseMessage { message :: String} deriving (Show, Generic)

instance FromJSON ResponseMessage where
    parseJSON (Object v) = ResponseMessage <$> v .: "message"
    parseJSON _ = empty

instance ToJSON ResponseMessage where
    toJSON (ResponseMessage message) = object ["message" .= message]

rm_worked :: ResponseMessage
rm_worked  = ResponseMessage { message = "File was successfully uploaded"}

rm_error :: ResponseMessage
rm_error = ResponseMessage { message = "Something wrong happened"}

try' :: IO a ->  IO (Either IOException a)
try' =  Control.Exception.try

postSubmissionR :: Import.Handler Value
postSubmissionR = do
  listNumber <- runInputPost $ ireq textField "listNumber"
  result <- runInputPost $ iopt fileField "studentSubmission"

  case result of Just fileInfo -> do
                    let mounthInitalPath = "resource/" <> listNumber <> "/students/"
                    let destination = (unpack $ mounthInitalPath <> (fileName fileInfo) )
                    liftIO $ fileMove fileInfo destination
                    initCorrection destination listNumber
                    initCommand "fsdfs"
                    sendResponseStatus status200 $ toJSON rm_worked
                 Nothing -> do
                    sendResponseStatus status400 $ toJSON rm_error


initCorrection :: FilePath -> Text -> HandlerT App IO ()
initCorrection filePath listNumber = do
  fileContent <- liftIO $ T.readFile filePath
  writeToFile fileContent listNumber

writeToFile :: String -> Text -> HandlerT App IO ()
writeToFile fileContent listNumber = do
  liftIO $ T.writeFile "resource/lista7/MultisetMap.hs" fileContent

initCommand :: String -> HandlerT App IO ()
initCommand registration = do
  let cmd = "ghci ./resource/lista7/GenerateFile.hs -e \"main \\\"" Prelude.++ registration Prelude.++ "\\\"\""
  result <- liftIO $ try' (callCommand cmd)
  case result of
    Left ex -> liftIO $ T.putStrLn "started ok"
    Right () -> liftIO $ T.putStrLn "started ok"
