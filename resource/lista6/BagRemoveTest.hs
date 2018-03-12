module BagRemoveTest(tests_remove) where

import qualified Data.List as List
import Test.HUnit
import Student

test_remove_1 = "Test remove Number from Empty Bag" ~:
                "Removing Number from empty Bag should return empty Bag" ~:
                (remove 1 []) ~?= []

test_remove_2 = "Test remove String from Empty Bag" ~:
                "Removing String from empty Bag should return empty Bag" ~:
                (remove "aba" []) ~?= []

test_remove_3 = "Test remove List from Empty Bag" ~:
                "Removing List from empty Bag should return empty Bag" ~:
                (remove [1] []) ~?= []

test_remove_4 = "Test remove Number not in the Bag" ~:
                "Removing Number not in the Bag should return the same Bag" ~:
                List.sort (remove 1 [(2, 2), (3, 1)]) ~?= List.sort [(2, 2), (3, 1)]

test_remove_5 = "Test remove String not in the Bag" ~:
                "Removing String not in the Bag should return the same Bag" ~:
                List.sort (remove "gu" [("leo", 1), ("vini", 4)]) ~?= List.sort [("leo", 1), ("vini", 4)]

test_remove_6 = "Test remove List not in the Bag" ~:
                "Removing List not in the Bag should return the same Bag" ~:
                List.sort (remove [1, 2] [([2, 1], 2), ([1, 1], 1), ([2, 2], 1)]) ~?= List.sort [([2, 1], 2), ([1, 1], 1), ([2, 2], 1)]

test_remove_7 = "Test remove Number in the Bag" ~:
                "Removing Number in the Bag should return the Bag without it" ~:
                (remove 1 [(2, 2), (1, 1)]) ~?= [(2, 2)]

test_remove_8 = "Test remove String in the Bag" ~:
                "Removing String in the Bag should return the Bag without it" ~:
                (remove "gu" [("gu", 1), ("vini", 4)]) ~?= [("vini", 4)]

test_remove_9 = "Test remove List in the Bag" ~:
                "Removing List in the Bag should return the Bag without it" ~:
                (remove [1, 2] [([2, 1], 2), ([1, 2], 1)]) ~?= [([2, 1], 2)]

test_remove_10 = "Test remove Number in the Bag" ~:
                "Removing Number in the Bag should return the Bag without it" ~:
                List.sort (remove 1 [(2, 2), (1, 2)]) ~?= List.sort [(2, 2), (1, 1)]

test_remove_11 = "Test remove String in the Bag" ~:
                "Removing String in the Bag should return the Bag without it" ~:
                List.sort (remove "gu" [("gu", 2), ("vini", 4)]) ~?= List.sort [("gu", 1), ("vini", 4)]

test_remove_12 = "Test remove List in the Bag" ~:
                "Removing List in the Bag should return the Bag without it" ~:
                List.sort (remove [1, 2] [([2, 1], 2), ([1, 2], 2)]) ~?= List.sort [([2, 1], 2), ([1, 2], 1)]

tests_remove :: [Test]
tests_remove = [
    test_remove_1, test_remove_2, test_remove_3,
    test_remove_4, test_remove_5, test_remove_6,
    test_remove_7, test_remove_8, test_remove_9,
    test_remove_10, test_remove_11, test_remove_12]
