module BagSummyTest(tests_summy) where

import qualified Data.List as List
import Test.HUnit
import Student

test_summy_1 = "Summy Bag with empty a Bag" ~:
               "Summy should return the Bag" ~:
               (summy [(1, 1)] []) ~?= [(1, 1)]

test_summy_2 = "Summy empty Bag with a Bag" ~:
               "Summy should return the Bag" ~:
               (summy [] [(1, 1)]) ~?= [(1, 1)]

test_summy_3 = "Summy of two equal Bags" ~:
               "Summy should the bag with double of quantity of elements" ~:
               (summy [(1, 1)] [(1, 1)]) ~?= [(1, 2)]

test_summy_4 = "Summy of two equal Bags" ~:
               "Summy should the bag with double of quantity of elements" ~:
               List.sort (summy [(1, 1), (2, 2), (3, 3)] [(2, 2), (3, 3), (1, 1)]) ~?= List.sort [(1, 2), (2, 4), (3, 6)]

test_summy_5 = "Summy of Disjunct Bags" ~:
               "Summy should return a Bag with the elements of the Disjunct Bags" ~:
               List.sort (summy [(2, 1)] [(1, 1)]) ~?= List.sort [(2, 1), (1, 1)]

test_summy_6 = "Summy of Bag A [(2, 1), (1, 1)] and Bag B [(1, 1)]" ~:
               "Summy should return Bag [(2, 1), (1, 2)]" ~:
               List.sort (summy [(2, 1), (1, 1)] [(1, 1)]) ~?= List.sort [(2, 1), (1, 2)]

test_summy_7 = "Summy of Bag A [(1, 1)] and Bag B [(1, 1), (2, 1)]" ~:
               "Summy should return Bag [(2, 1), (1, 2)]" ~:
               List.sort (summy [(1, 1)] [(1, 1), (2, 1)]) ~?= List.sort [(2, 1), (1, 2)]

tests_summy :: [Test]
tests_summy = [
    test_summy_1, test_summy_2, test_summy_3,
    test_summy_4, test_summy_5, test_summy_6,
    test_summy_7]
