{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module GenerateFile (main) where

import TestIntersection as In
import TestUnion as Un
import TestRemove as Rm
import TestInsert as Insert
import TestInclusion as Inclusion
import TestMinus as Minus
import TestSize as Size
import TestSearch as Search
import TestSum as Sum
import Test.HUnit
import Data.List.Split
import GHC.Generics
import System.IO as T
import Data.Text (unpack)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON, FromJSON, Value, toJSON)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Control.Monad.Trans.Reader as Reader
import           Control.Monad.IO.Class  (liftIO)
import Import

-- data Result = Result { studentId :: String, failed :: Int,
--                     passed :: Int, total :: Int, exceptions :: Int} deriving (Show, Generic, ToJSON, FromJSON)


main :: String -> IO ()
main studentId = do
  testsResult <- runTestTT $ test $ mconcat [ Insert.tests, Inclusion.tests, Rm.tests, Minus.tests, Size.tests, Search.tests, Sum.tests, In.tests, Un.tests]
  generateResult studentId testsResult



generateResult :: String -> Counts -> HandlerT App IO ()
generateResult studentId testsResult = do

  let fileName = "./resource/lista7/results/" Prelude.++ studentId Prelude.++ ".json"
  let total = cases testsResult
  let passed = tried testsResult - errors testsResult - failures testsResult
  let exceptions = errors testsResult
  let failed = failures testsResult
  let output = jesus { studentId = studentId, failed = failed, passed = passed,
                          total = total,  exceptions = exceptions, listName = "lista7"}

  outputId <- runDB $ insertEntity output
  -- insert $ Result studentId failed passed total exceptions

  -- resultId <- insert  output
  -- I.writeFile fileName (encodeToLazyText output)
  -- T.putStrLn "Write file in result directory json worked"
