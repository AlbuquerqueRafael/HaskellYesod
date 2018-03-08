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
import System.IO as T
import Control.Exception
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName, unSqlBackendKey, toSqlKey)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson (decode)

newtype ResponseMessage = ResponseMessage { message :: String } deriving (Show, Generic)

instance FromJSON ResponseMessage where
    parseJSON (Object v) = ResponseMessage <$> v .: "message"
    parseJSON _ = empty

instance ToJSON ResponseMessage where
    toJSON (ResponseMessage message) = object ["message" .= message]

newtype StatusDateSubmission = StatusDateSubmission { status :: Integer } deriving (Show, Generic)

instance FromJSON StatusDateSubmission where
    parseJSON (Object v) = StatusDateSubmission <$> v .: "status"
    parseJSON _ = empty

instance ToJSON StatusDateSubmission where
    toJSON (StatusDateSubmission status) = object ["status" .= status]

rmWorked :: ResponseMessage
rmWorked  = ResponseMessage { message = "File was successfully uploaded" }

rmError :: ResponseMessage
rmError = ResponseMessage { message = "Something wrong happened" }

rmCompilationProblems :: ResponseMessage
rmCompilationProblems = ResponseMessage { message = "The submission has compilation problems" }

rmUploadProblems :: ResponseMessage
rmUploadProblems = ResponseMessage { message = "The submission has upload problems" }

rmEarlySubmission :: ResponseMessage
rmEarlySubmission = ResponseMessage { message = "The submission is not open yet" }

rmOutOfTime :: ResponseMessage
rmOutOfTime = ResponseMessage { message = "The submission is out of time" }

try' :: IO a ->  IO (Either IOException a)
try' =  Control.Exception.try

earlySubmission :: StatusDateSubmission
earlySubmission = StatusDateSubmission { status = 0}

okSubmission :: StatusDateSubmission
okSubmission = StatusDateSubmission { status = 1}

delayedSubmission :: StatusDateSubmission
delayedSubmission = StatusDateSubmission { status = 2 }

outOfTime :: StatusDateSubmission
outOfTime = StatusDateSubmission { status = 3 }


postSubmissionR :: Import.Handler ()
postSubmissionR = do
  listNumber <- runInputPost $ ireq textField "listNumber"
  result <- runInputPost $ iopt fileField "studentSubmission"
  studentId <- runInputPost $ ireq textField "studentId"

  statusSubmission <- liftIO $ (hasSubmissionDelay (unpack listNumber)) 

  case statusSubmission of Just ( StatusDateSubmission fileInfo) -> do
                              liftIO $ T.print fileInfo
                              sendResponseStatus status400 $ toJSON rmError
                           Nothing -> sendResponseStatus status400 $ toJSON rmError



  -- case result of Just fileInfo -> do
  --                   let mounthInitalPath = "resource/" <> listNumber <> "/students/"
  --                   let destination = unpack $ mounthInitalPath <> fileName fileInfo
  --                   liftIO $ fileMove fileInfo destination
  --                   initCorrection destination listNumber
  --                   initCommand (unpack studentId) (unpack listNumber)
  --                   sendResponseStatus status200 $ toJSON rmWorked
  --                Nothing -> sendResponseStatus status400 $ toJSON rmError


hasSubmissionDelay listNumber = do
  ms <- runDB $ selectList [ActivityActivityId ==. listNumber] [] :: Import.Handler [Entity Activity]
  let dataHoraLimiteEnvioNormal = (activityDataHoraLimiteEnvioNormal (entityVal  (ms !! 0) ) )
  let dataHoraLimiteEnvioAtraso = (activityDataHoraLimiteEnvioAtraso (entityVal  (ms !! 0) ) )
  let dataHoraLiberacao = (activityDataHoraLiberacao (entityVal  (ms !! 0) ) )
  now <- liftIO getCurrentTime

  if (now < dataHoraLiberacao)
    then do
      return  earlySubmission
  else if (now >= dataHoraLiberacao && now <= dataHoraLimiteEnvioNormal)
    then do
      return  okSubmission
  else if (now >= dataHoraLimiteEnvioNormal && now <= dataHoraLimiteEnvioAtraso)
    then do
      return delayedSubmission
  else do
    return outOfTime




initCorrection :: FilePath -> Text -> HandlerT App IO ()
initCorrection filePath listNumber = do
  fileContent <- liftIO $ T.readFile filePath
  writeToFile fileContent (unpack listNumber)

writeToFile :: String -> String -> HandlerT App IO ()
writeToFile fileContent listNumber =
  liftIO $ T.writeFile ("resource/" Prelude.++ listNumber Prelude.++ "/MultisetMap.hs") fileContent

initCommand :: String -> String -> HandlerT App IO ()
initCommand registration listNumber = do
  let cmd = "ghci ./resource/" Prelude.++ listNumber Prelude.++ "/GenerateFile.hs -iresource/" Prelude.++ listNumber Prelude.++ " -e \"main \\\"" Prelude.++ registration Prelude.++ "\\\"\""
  result <- liftIO $ try' (callCommand cmd)
  case result of
      Left _ -> sendResponseStatus status200 $ toJSON rmCompilationProblems
      Right () -> do
                    contents <- liftIO $ Prelude.readFile $ "./resource/lista7/results/" Prelude.++ registration Prelude.++ ".json"
                    let readJson = decode $ C.pack contents :: Maybe Submission
                    case readJson of
                        Just (Submission studentId failed passed total exceptions listName delay) -> do
                            ms <- runDB $ selectList [SubmissionStudentId ==. studentId, SubmissionListName==. listName] [LimitTo 1] :: Import.Handler [Entity Submission]
                            -- Checks if the student already did a submition. If yes, update(replace) the data. Else, insert
                            _ <- if (Prelude.length ms) == 0
                                   then do
                                     _ <- runDB $ insert $ Submission studentId failed passed total exceptions listName delay
                                     sendResponseStatus status200 $ toJSON rmWorked
                                 else do
                                   let submissionID = (unSqlBackendKey ( unSubmissionKey (entityKey (ms !! 0) ) ))
                                   _ <- runDB $ replace (toSqlKey submissionID :: Key Submission) $ Submission studentId failed passed total exceptions listName delay
                                   sendResponseStatus status200 $ toJSON rmWorked
                            sendResponseStatus status200 $ toJSON rmWorked
                        _ -> do
                          sendResponseStatus status400 $ toJSON rmUploadProblems


getSubmissionR :: Import.Handler Value
getSubmissionR = do
  listName <- runInputGet $ ireq textField "listName"
  studentId <- runInputGet $ ireq textField "studentId"
  ms <- runDB $ selectList [SubmissionStudentId ==. studentId, SubmissionListName ==. listName] [] :: Import.Handler [Entity Submission]
  return $ object ["submission" .= ms]
