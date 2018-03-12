module RunTests(testsResult) where

import Test.HUnit
import TestIntersection as In
import TestUnion as Un
import TestRemove as Rm
import TestInsert as Insert
import TestInclusion as Inclusion
import TestMinus as Minus
import TestSize as Size
import TestSearch as Search
import TestSum as Sum

testsResult = do
  result <- runTestTT $ test $ mconcat [ Insert.tests, Inclusion.tests, Rm.tests, Minus.tests, Size.tests, Search.tests, Sum.tests, In.tests, Un.tests]
  return result
