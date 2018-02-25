{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Home where

import Import

newtype ResMessage = ResMessage { message :: String } deriving (Show, Generic)

instance FromJSON ResMessage where
    parseJSON (Object v) = ResMessage <$> v .: "message"
    parseJSON _ = empty

instance ToJSON ResMessage where
    toJSON (ResMessage message) = object ["message" .= message]


rmError :: ResMessage
rmError = ResMessage { message = "Not found" }

getHomeR :: Handler Value
getHomeR = sendResponseStatus status404 $ toJSON rmError
