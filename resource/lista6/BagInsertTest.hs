module BagInsertTest(tests_insert) where

import qualified Data.List as List
import Test.HUnit
import Student

test_insert_1 = "Test Insert Number in Empty Bag" ~:
                "Should be able to Insert Number in empty Bag" ~:
                (insert 1 []) ~?= [(1, 1)]

test_insert_2 = "Test Insert String in Empty Bag" ~:
                "Should be able to Insert String in empty Bag" ~:
                (insert "a" []) ~?= [("a", 1)]

test_insert_3 = "Test Insert List in Empty Bag" ~:
                "Should be able to Insert List in empty Bag" ~:
                (insert [1, 2] []) ~?= [([1, 2], 1)]

test_insert_4 = "Test Insert Number already in the Bag" ~:
                "Should be able to Insert Number already in Bag" ~:
                (insert 1 [(1, 1)]) ~?= [(1, 2)]

test_insert_5 = "Test Insert Number already in the Bag" ~:
                "Should be able to Insert Number already in Bag" ~:
                List.sort (insert 1 [(2, 1), (1, 1)]) ~?= List.sort [(2, 1), (1, 2)]

test_insert_6 = "Test Insert String already in the Bag" ~:
                "Should be able to Insert String already in Bag" ~:
                (insert "a" [("a", 2)]) ~?= [("a", 3)]

test_insert_7 = "Test Insert String already in the Bag" ~:
                "Should be able to Insert String already in Bag" ~:
                List.sort (insert "ab" [("ab", 1), ("a", 2)]) ~?= List.sort [("ab", 2), ("a", 2)]

test_insert_8 = "Test Insert List already in the Bag" ~:
                "Should be able to Insert List already in Bag" ~:
                (insert [1] [([1], 4)]) ~?= [([1], 5)]

test_insert_9 = "Test Insert List already in the Bag" ~:
                "Should be able to Insert List already in Bag" ~:
                List.sort (insert [10, 2] [([10, 2], 1), ([2, 10], 1)]) ~?= List.sort [([10, 2], 2), ([2, 10], 1)]

test_insert_10 = "Test Insert Number not in the Bag" ~:
                 "Should be able to Insert different Number in Bag" ~:
                 List.sort (insert 1 [(2, 1)]) ~?= List.sort [(2, 1), (1, 1)]

test_insert_11 = "Test Insert Number not in the Bag" ~:
                 "Should be able to Insert different Number in Bag" ~:
                 List.sort (insert 3 [(2, 1), (1, 1)]) ~?= List.sort [(2, 1), (1, 1), (3, 1)]

test_insert_12 = "Test Insert String not in the Bag" ~:
                 "Should be able to Insert different String in Bag" ~:
                 List.sort (insert "a" [("b", 1)]) ~?= List.sort [("b", 1), ("a", 1)]

test_insert_13 = "Test Insert String not in the Bag" ~:
                 "Should be able to Insert different String in Bag" ~:
                 List.sort (insert "gu" [("leo", 1), ("vini", 1)]) ~?= List.sort [("leo", 1), ("vini", 1), ("gu", 1)]

test_insert_14 = "Test Insert List not in the Bag" ~:
                 "Should be able to Insert different List in Bag" ~:
                 List.sort (insert [0] [([0, 1], 1)]) ~?= List.sort [([0, 1], 1), ([0], 1)]

test_insert_15 = "Test Insert List not in the Bag" ~:
                 "Should be able to Insert different List in Bag" ~:
                 List.sort (insert [2, 1] [([1, 2], 1), ([2, 2], 1)]) ~?= List.sort [([1, 2], 1), ([2, 2], 1), ([2, 1], 1)]

tests_insert :: [Test]
tests_insert = [
                test_insert_1, test_insert_2, test_insert_3,
                test_insert_4, test_insert_5, test_insert_6,
                test_insert_7, test_insert_8, test_insert_9,
                test_insert_10, test_insert_11, test_insert_12,
                test_insert_13, test_insert_14, test_insert_15]
