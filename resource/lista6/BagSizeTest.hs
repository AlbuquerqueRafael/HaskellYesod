module BagSizeTest(tests_size) where

import Test.HUnit
import Student

test_size_1 = "Test Size empty Bag" ~:
              "Size of Empty Bag should be 0" ~:
              size [] ~?= 0

test_size_2 = "Test Size Bag with one element" ~:
              "Size of Bag with one element should be 1" ~:
              size [(1, 1)] ~?= 1

test_size_3 = "Test Size Bag with 2 elements" ~:
              "Size of Bag with 2 element should be 2" ~:
              size [(1, 1), (2, 1)] ~?= 2

test_size_4 = "Test Size Bag with one element 3 times" ~:
              "Size of Bag with one element 3 times should be 3" ~:
              size [(1, 3)] ~?= 3

test_size_5 = "Test Size Bag [(1, 3), (2, 5), (7, 2)]" ~:
              "Size of Bag [(1, 3), (2, 5), (7, 2)] should be 10" ~:
              size [(1, 3), (2, 5), (7, 2)] ~?= 10

tests_size :: [Test]
tests_size = [
    test_size_1, test_size_2, test_size_3,
    test_size_4, test_size_5]
