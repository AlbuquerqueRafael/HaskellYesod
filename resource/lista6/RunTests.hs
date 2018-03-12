module RunTests(testsResult) where

import Test.HUnit
import BagInsertTest(tests_insert)
import BagRemoveTest(tests_remove)
import BagSearchTest(tests_search)
import BagUnionTest(tests_union)
import BagIntersectionTest(tests_intersection)
import BagInclusionTest(tests_inclusion)
import BagSummyTest(tests_summy)
import BagSizeTest(tests_size)
import BagMinusTest(tests_minus)

testsResult = do
  result <- runTestTT $ test $ mconcat [tests_insert, tests_remove, tests_search,
                                      tests_union, tests_intersection, tests_inclusion,
                                      tests_summy, tests_size, tests_minus]
  return result
