{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module GenerateFile (main) where


import RunTests as MT
import Test.HUnit
import Data.List.Split
import GHC.Generics
import System.IO as T
import Data.Text (unpack)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON, FromJSON, Value, toJSON)
import Control.Monad.Trans.Reader as Reader
import           Control.Monad.IO.Class  (liftIO)

data Result = Submission { studentId :: String, failed :: Int,
                    passed :: Int, total :: Int, exceptions :: Int, listName :: String, delay :: Bool} deriving (Show, Generic, ToJSON, FromJSON)


main :: String -> String -> IO ()
main listNumber studentId = do
  testsResult <- MT.testsResult
  generateResult listNumber studentId testsResult



generateResult :: String -> String -> Counts -> IO ()
generateResult listNumber studentId testsResult  = do

  let fileName = "./resource/" Prelude.++ listNumber Prelude.++ "/results/" Prelude.++ studentId Prelude.++ ".json"
  let total = cases testsResult
  let passed = tried testsResult - errors testsResult - failures testsResult
  let exceptions = errors testsResult
  let failed = failures testsResult
  let output = Submission { studentId = studentId, failed = failed, passed = passed,
                          total = total,  exceptions = exceptions, listName = listNumber, delay = True}

  I.writeFile fileName (encodeToLazyText output)
  -- T.putStrLn "Write file in result directory json worked"
