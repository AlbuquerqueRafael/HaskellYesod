{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, DeriveAnyClass #-}
module Handler.Submission where

import Import

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

postSubmissionR :: Handler Value
postSubmissionR = do
  listNumber <- runInputPost $ ireq textField "listNumber"
  result <- runInputPost $ iopt fileField "studentSubmission"

  case result of Just fileInfo -> do
                    let mounthInitalPath = "resource/" <> listNumber <> "/"
                    let destination = (unpack $ mounthInitalPath <> (fileName fileInfo) )
                    liftIO $ fileMove fileInfo destination
                    sendResponseStatus status200 $ toJSON rm_worked
                 Nothing -> do
                    sendResponseStatus status400 $ toJSON rm_error
