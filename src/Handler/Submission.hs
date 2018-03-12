{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Handler.Submission where

import System.Process
import Import
import System.IO as T
import Control.Exception
import Database.Persist.Sql  (unSqlBackendKey, toSqlKey)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson (decode)

newtype ResponseMessage = ResponseMessage { message :: String } deriving (Show, Generic)

instance FromJSON ResponseMessage where
    parseJSON (Object v) = ResponseMessage <$> v .: "message"
    parseJSON _ = empty

instance ToJSON ResponseMessage where
    toJSON (ResponseMessage message) = object ["message" .= message]

newtype StatusSubmission = StatusSubmission { status :: Integer } deriving (Show, Generic)

instance FromJSON StatusSubmission where
    parseJSON (Object v) = StatusSubmission <$> v .: "status"
    parseJSON _ = empty

instance ToJSON StatusSubmission where
    toJSON (StatusSubmission status) = object ["status" .= status]

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

rmActivityNotFound :: ResponseMessage
rmActivityNotFound = ResponseMessage { message = "Activity name not found" }

rmSubmissionResultNotAvailable :: ResponseMessage
rmSubmissionResultNotAvailable = ResponseMessage { message = "Submission result not available yet" }

try' :: IO a ->  IO (Either IOException a)
try' =  Control.Exception.try

earlySubmission :: StatusSubmission
earlySubmission = StatusSubmission { status = 0 }

okSubmission :: StatusSubmission
okSubmission = StatusSubmission { status = 1 }

delayedSubmission :: StatusSubmission
delayedSubmission = StatusSubmission { status = 2 }

outOfTime :: StatusSubmission
outOfTime = StatusSubmission { status = 3 }

activityNotFound :: StatusSubmission
activityNotFound = StatusSubmission { status = 4 }

postSubmissionR :: Import.Handler ()
postSubmissionR = do
    listNumber <- runInputPost $ ireq textField "listNumber"
    result <- runInputPost $ iopt fileField "studentSubmission"
    studentId <- runInputPost $ ireq textField "studentId"

    statusSubmission <- hasSubmissionDelay (unpack listNumber)

    let StatusSubmission submissionStatusResult = statusSubmission

    if submissionStatusResult == status earlySubmission
    then
        sendResponseStatus status400 $ toJSON rmEarlySubmission
    else if submissionStatusResult ==  status okSubmission
    then
        initSubmission result listNumber studentId False
    else if submissionStatusResult ==  status delayedSubmission
    then
        initSubmission result listNumber studentId True
    else if submissionStatusResult == status activityNotFound
    then
        sendResponseStatus status404 $ toJSON rmActivityNotFound
    else
        sendResponseStatus status400 $ toJSON rmOutOfTime


hasSubmissionDelay :: String -> Import.Handler StatusSubmission
hasSubmissionDelay listNumber = do
    ms <- runDB $ selectList [ActivityNome ==. listNumber] [] :: Import.Handler [Entity Activity]

    if Prelude.null ms
    then
        return activityNotFound
    else do
        let dataHoraLimiteEnvioNormal = activityDataHoraLimiteEnvioNormal (entityVal  (Prelude.head ms))
        let dataHoraLimiteEnvioAtraso = activityDataHoraLimiteEnvioAtraso (entityVal  (Prelude.head ms))
        let dataHoraLiberacao = activityDataHoraLiberacao (entityVal  (Prelude.head ms))
        now <- liftIO getCurrentTime

        if now < dataHoraLiberacao
            then
            return earlySubmission
        else if now >= dataHoraLiberacao && now <= dataHoraLimiteEnvioNormal
        then
            return okSubmission
        else if now >= dataHoraLimiteEnvioNormal && now <= dataHoraLimiteEnvioAtraso
        then
            return delayedSubmission
        else
            return outOfTime

initSubmission :: Maybe FileInfo -> Text -> Text -> Bool -> Import.Handler ()
initSubmission result listNumber studentId delay =
  case result of Just fileInfo -> do
                    let mounthInitalPath = "resource/" <> listNumber <> "/students/"
                    let destination = unpack $ mounthInitalPath <> fileName fileInfo
                    liftIO $ fileMove fileInfo destination
                    initCorrection destination listNumber
                    initCommand (unpack studentId) (unpack listNumber) delay
                    sendResponseStatus status200 $ toJSON rmWorked
                 Nothing -> sendResponseStatus status400 $ toJSON rmError


initCorrection :: FilePath -> Text -> HandlerT App IO ()
initCorrection filePath listNumber = do
  fileContent <- liftIO $ T.readFile filePath
  writeToFile fileContent (unpack listNumber)

writeToFile :: String -> String -> HandlerT App IO ()
writeToFile fileContent listNumber =
  liftIO $ T.writeFile ("resource/" Prelude.++ listNumber Prelude.++ "/MultisetMap.hs") fileContent

initCommand :: String -> String -> Bool -> HandlerT App IO ()
initCommand registration listNumber correctDelay = do
  let cmd = "ghci ./resource/" Prelude.++ listNumber Prelude.++ "/GenerateFile.hs -iresource/" Prelude.++ listNumber Prelude.++ " -e \"main \\\"" Prelude.++ registration Prelude.++ "\\\"\""
  result <- liftIO $ try' (callCommand cmd)
  case result of
      Left _ -> sendResponseStatus status400 $ toJSON rmCompilationProblems
      Right () -> do
                    contents <- liftIO $ Prelude.readFile $ "./resource/lista7/results/" Prelude.++ registration Prelude.++ ".json"
                    let readJson = decode $ C.pack contents :: Maybe Submission
                    case readJson of
                        Just (Submission studentId failed passed total exceptions listName delay) -> do
                            ms <- runDB $ selectList [SubmissionStudentId ==. studentId, SubmissionListName==. listName] [LimitTo 1] :: Import.Handler [Entity Submission]
                            -- Checks if the student already did a submition. If yes, update(replace) the data. Else, insert
                            _ <- if Prelude.null ms
                                   then do
                                     _ <- runDB $ insert $ Submission studentId failed passed total exceptions listName correctDelay
                                     sendResponseStatus status200 $ toJSON rmWorked
                                 else do
                                   let submissionID = unSqlBackendKey (unSubmissionKey (entityKey (Prelude.head ms)))
                                   _ <- runDB $ replace (toSqlKey submissionID :: Key Submission) $ Submission studentId failed passed total exceptions listName correctDelay
                                   sendResponseStatus status200 $ toJSON rmWorked
                            sendResponseStatus status200 $ toJSON rmWorked
                        _ ->
                          sendResponseStatus status400 $ toJSON rmUploadProblems

getSubmissionR :: Import.Handler Value
getSubmissionR = do
    listName <- runInputGet $ ireq textField "listName"
    studentId <- runInputGet $ ireq textField "studentId"

    statusSubmission <- hasSubmissionDelay (unpack listName)

    let StatusSubmission submissionStatusResult = statusSubmission

    if submissionStatusResult == status outOfTime
    then do
        ms <- runDB $ selectList [SubmissionStudentId ==. studentId, SubmissionListName ==. listName] [] :: Import.Handler [Entity Submission]
        return $ object ["submission" .= ms]
    else if submissionStatusResult == status activityNotFound
    then
        sendResponseStatus status404 $ toJSON rmActivityNotFound
    else
        sendResponseStatus status400 $ toJSON rmSubmissionResultNotAvailable



-- getAllSubmissionsR :: Import.Handler Value
-- getAllSubmissionsR = do
--     ms <- runDB $ selectList [] [] :: Import.Handler [Entity Submission]
--     return $ object ["submissions" .= ms]
