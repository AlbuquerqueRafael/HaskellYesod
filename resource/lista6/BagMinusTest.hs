module BagMinusTest(tests_minus) where

import qualified Data.List as List
import Test.HUnit
import Student

test_minus_1 = "Test get minus between a no-empty Bag A and an empty Bag B" ~:
                      "Get minus between a no-empty Bag A and an empty Bag B should return Bag A"  ~:
                      (minus [(2, 1)] []) ~?= [(2, 1)]

test_minus_2 = "Test get minus between an empty Bag A and a no-empty Bag B" ~:
                      "Get minus between an empty A and a no-empty Bag B should return []"  ~:
                      (minus [] [(2, 1)]) ~?= []

test_minus_3 = "Test get minus between Bag A and Bag A" ~:
                      "Get minus between Bag A and Bag A should return []"  ~:
                      minus [(2, 1), (1, 1)] [(2, 1), (1, 1)] ~?= []

test_minus_4 = "Test get minus between Bag A and Bag A" ~:
                      "Get minus between Bag A and Bag A should return []"  ~:
                      minus [(1, 1), (2, 1)] [(2, 1), (1, 1)] ~?= []

test_minus_5 = "Test get minus between Bags A and B with no intersection" ~:
                      "Get minus between Bags with no intersection should return Bag A"  ~:
                      (minus [(2, 1)] [(1, 2)]) ~?= [(2, 1)]

test_minus_6 = "Test get minus between Bag A and Bag B where B is a subset of A" ~:
                      "Get minus between Bag A and Bag B where B is a subset of A should return A with no elements in B."  ~:
                      (minus [(2, 1), (1, 1)] [(1, 1)]) ~?= [(2, 1)]

test_minus_7 = "Test get minus between Bag A and Bag B where A is a subset of B" ~:
                      "Get minus between Bag A and Bag B where A is a subset of B should return []"  ~:
                      (minus [(2, 1)] [(2, 1), (1, 1)]) ~?= []

test_minus_8 = "Test get minus between Bag A and Bag B where A has the element (2, 3) and B has the element (2, 1)" ~:
                      "Get minus between Bag A and Bag B should return [(2, 2)]"  ~:
                      (minus [(2, 3)] [(2, 1), (1, 1)]) ~?= [(2, 2)]

test_minus_9 = "Test get minus between Bag A and Bag B where A has the element ('a', 1) and B has the element ('a', 2)" ~:
                      "Get minus between Bag A and Bag B should return [('b', 1)]"  ~:
                      (minus [('b', 1), ('a', 1)] [('a', 2)]) ~?= [('b', 1)]

tests_minus :: [Test]
tests_minus = [ test_minus_1, test_minus_2, test_minus_3,
                test_minus_4, test_minus_5, test_minus_6,
                test_minus_7, test_minus_8, test_minus_9]
