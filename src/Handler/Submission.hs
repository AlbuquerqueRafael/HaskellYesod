{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE #-}
-- {-# LANGUAGE #-}
-- {-# LANGUAGE #-}

module Handler.Submission where

import System.Process
import Import
import System.Directory
import System.IO as T
import Control.Exception
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName, unSqlBackendKey, toSqlKey)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson (decode, fromJSON)

newtype ResponseMessage = ResponseMessage { message :: String } deriving (Show, Generic)

instance FromJSON ResponseMessage where
    parseJSON (Object v) = ResponseMessage <$> v .: "message"
    parseJSON _ = empty

instance ToJSON ResponseMessage where
    toJSON (ResponseMessage message) = object ["message" .= message]

rmWorked :: ResponseMessage
rmWorked  = ResponseMessage { message = "File was successfully uploaded" }

rmError :: ResponseMessage
rmError = ResponseMessage { message = "Something wrong happened" }

try' :: IO a ->  IO (Either IOException a)
try' =  Control.Exception.try

postSubmissionR :: Import.Handler ()
postSubmissionR = do
  listNumber <- runInputPost $ ireq textField "listNumber"
  result <- runInputPost $ iopt fileField "studentSubmission"
  studentId <- runInputPost $ ireq textField "studentId"

  case result of Just fileInfo -> do
                    let mounthInitalPath = "resource/" <> listNumber <> "/students/"
                    let destination = unpack $ mounthInitalPath <> fileName fileInfo
                    liftIO $ fileMove fileInfo destination
                    initCorrection destination listNumber
                    initCommand (unpack studentId)
                    sendResponseStatus status200 $ toJSON rmWorked
                 Nothing -> sendResponseStatus status400 $ toJSON rmError


initCorrection :: FilePath -> Text -> HandlerT App IO ()
initCorrection filePath listNumber = do
  fileContent <- liftIO $ T.readFile filePath
  writeToFile fileContent (unpack listNumber)

writeToFile :: String -> String -> HandlerT App IO ()
writeToFile fileContent listNumber =
  liftIO $ T.writeFile ("resource/" Prelude.++ listNumber Prelude.++ "/MultisetMap.hs") fileContent

initCommand :: String -> HandlerT App IO ()
initCommand registration = do
  let cmd = "ghci ./resource/lista7/GenerateFile.hs -iresource/lista7 -e \"main \\\"" Prelude.++ registration Prelude.++ "\\\"\""
  result <- liftIO $ try' (callCommand cmd)
  case result of
      Left ex -> liftIO $ T.print ex
      Right () -> do
                    contents <- liftIO $ Prelude.readFile $ "./resource/lista7/results/" Prelude.++ registration Prelude.++ ".json"
                    let readJson = decode $ C.pack contents :: Maybe Submission
                    case readJson of
                        Just (Submission studentId failed passed total exceptions listName) -> do
                            ms <- runDB $ selectList [SubmissionStudentId ==. studentId, SubmissionListName==. listName] [LimitTo 1] :: Import.Handler [Entity Submission]
                            -- Checks if the student already did a submition. If yes, update(replace) the data. Else, insert
                            if (Prelude.length ms) == 0
                              then do
                                runDB $ insert $ Submission studentId failed passed total exceptions listName
                                liftIO $ T.print "Worked"
                              else do
                                let submissionID = (unSqlBackendKey ( unSubmissionKey (entityKey (ms !! 0) ) ))
                                runDB $ replace (toSqlKey submissionID :: Key Submission) $ Submission studentId failed passed total exceptions listName
                                liftIO $ T.print "Worked"

                            liftIO $ T.print "Worked"
                        _ -> do
                          liftIO $ T.print "Not Worked"


getSubmissionR :: Import.Handler Value
getSubmissionR = do
  ms <- runDB $ selectList [] [] :: Import.Handler [Entity Submission]
  return $ object ["submissions" .= ms]
