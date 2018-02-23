{-# LANGUAGE OverloadedStrings #-}

module Handler.Submission where

import Import
import System.IO as T
import Control.Applicative
import Data.Aeson
import Data.Text (Text)

data Person = Person { name :: Text, age :: String } deriving Show

-- We expect a JSON object, so we fail at any non-Object value.
instance FromJSON Person where
    parseJSON (Object v) = Person <$> v .: "name" <*> v .: "age"
    parseJSON _ = empty

instance ToJSON Person where
    toJSON (Person name age) = object ["name" .= name, "age" .= age]


postSubmissionR :: Handler Value
postSubmissionR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    comment <- (requireJsonBody :: Handler Person)
    -- get the json body as Foo (assumes FromJSON instance)
    let teste = age comment
    liftIO $ T.writeFile "test.txt" teste
    returnJson teste

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
