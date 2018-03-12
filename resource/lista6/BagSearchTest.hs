module BagSearchTest(tests_search) where

import Test.HUnit
import Student

test_search_1 = "Test search Number from Empty Bag" ~:
                "Search for a Number in a empty Bag should return 0"  ~:
                (search 1 []) ~?= 0

test_search_2 = "Test search String from Empty Bag" ~:
                "Search for a String in a empty Bag should return 0"  ~:
                (search "1" []) ~?= 0

test_search_3 = "Test search List from Empty Bag" ~:
                "Search for a List in a empty Bag should return 0"  ~:
                (search [1, 2, 3] []) ~?= 0

test_search_4 = "Test search Number not in the Bag" ~:
                "Search for a Number not in the Bag should return 0" ~:
                (search 1 [(2, 1)]) ~?= 0

test_search_5 = "Test search String not in the Bag" ~:
                "Search for a String not in the Bag should return 0" ~:
                (search "a" [("aa", 1)]) ~?= 0

test_search_6 = "Test search List not in the Bag" ~:
                "Search for a List not in the Bag should return 0" ~:
                (search [2, 1] [([1, 2], 1)]) ~?= 0

test_search_7 = "Test search Number in the Bag" ~:
                "Search for a Number in the Bag should return the right quantity" ~:
                (search 1 [(1, 2), (2, 1)]) ~?= 2

test_search_8 = "Test search Number in the Bag" ~:
                "Search for a Number in the Bag should return the right quantity" ~:
                (search 2 [(1, 2), (2, 1)]) ~?= 1

test_search_9 = "Test search String in the Bag" ~:
                "Search for a String in the Bag should return the right quantity" ~:
                (search "a" [("a", 3), ("aa", 1), ("aaa", 2)]) ~?= 3

test_search_10 = "Test search String in the Bag" ~:
                "Search for a String in the Bag should return the right quantity" ~:
                (search "b" [("a", 2), ("c", 9), ("b", 12)]) ~?= 12

test_search_11 = "Test search List in the Bag" ~:
                "Search for a List in the Bag should return the right quantity" ~:
                (search [1, 2] [([2, 1], 1), ([2, 2], 1), ([1, 2], 6)]) ~?= 6

test_search_12 = "Test search List in the Bag" ~:
                "Search for a List in the Bag should return the right quantity" ~:
                (search [3, 3] [([3], 1), ([3, 3, 3], 5), ([3, 3], 2)]) ~?= 2

tests_search :: [Test]
tests_search = [
    test_search_1, test_search_2, test_search_3,
    test_search_4, test_search_5, test_search_6,
    test_search_7, test_search_8, test_search_9,
    test_search_10, test_search_11, test_search_12]
