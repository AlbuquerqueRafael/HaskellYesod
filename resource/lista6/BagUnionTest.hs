module BagUnionTest(tests_union) where

import Test.HUnit
import Student
import qualified Data.List as List

-- TODO1: POR ALGUM MOTIVO NÃO CONSIGO CRIAR UMA BAG VAZIA.
-- TODO2: BAG É UMA COLEÇÃO ORDENADA?????

--test_union_1 = TestCase (assertEqual "The union between two empty bags should be an empty bag" ( union [] [] ) ([]) )

test_union_2 = TestCase (assertEqual "[Type: Char] The union between an bag A and an empty bag B should be equal to the bag A"
    (List.sort $ union [('A', 1), ('B', 1)] [] ) (List.sort [('A', 1), ('B', 1)]) )

test_union_3 = TestCase (assertEqual "[Type: Number] The union between an bag A and an empty bag B should be equal to the bag A"
    (List.sort $ union [(1, 1), (2, 1)] [] ) (List.sort [(1, 1), (2, 1)]) )

test_union_4 = TestCase (assertEqual "[Type: String] The union between an bag A and an empty bag B should be equal to the bag A"
    (List.sort $ union [("ab", 1), ("cd", 1)] [] ) (List.sort [("ab", 1), ("cd", 1)]) )

test_union_5 = TestCase (assertEqual "[Type: Char] The union between an empty bag A and an bag B should be equal to the bag B"
    (List.sort $ union [] [('A', 1), ('B', 1)]  ) (List.sort [('A', 1), ('B', 1)]) )

test_union_6 = TestCase (assertEqual "[Type: Number] The union between an empty bag A and an bag B should be equal to the bag B"
    (List.sort $ union [] [(1, 1), (2, 1)]  ) (List.sort [(1, 1), (2, 1)]) )

test_union_7 = TestCase (assertEqual "[Type: String] The union between an empty bag A and an bag B should be equal to the bag B"
    (List.sort $ union [] [("ab", 1), ("cd", 1)]  ) (List.sort [("ab", 1), ("cd", 1)]) )

test_union_8 = TestCase (assertEqual "[Type: Char] The union between bags with diferent elements should have the elements of both"
    (List.sort $ union [('A', 1)] [('B', 1)]  ) (List.sort [('A', 1), ('B', 1)]) )

test_union_9 = TestCase (assertEqual "[Type: Number] The union between bags with diferent elements should have the elements of both"
    (List.sort $ union [(1, 1)] [(2, 1)] ) (List.sort [(1, 1), (2, 1)]) )

test_union_10 = TestCase (assertEqual "[Type: String] The union between bags with diferent elements should have the elements of both"
    (List.sort $ union [("string1", 1)] [("string2", 1)]  ) (List.sort [("string1", 1), ("string2", 1)]) )

test_union_11 = TestCase (assertEqual "[Type: Char] The union between bags with the same elements should have the max number of elements"
    (List.sort $ union [('A', 1), ('B', 2)] [('A', 2), ('B', 1)]  ) (List.sort [('A', 2), ('B', 2)]) )

test_union_12 = TestCase (assertEqual "[Type: Number] The union between bags with the same elements should have the max number of elements"
    (List.sort $ union [(1, 1), (2, 2)] [(1, 2), (2, 1)]  ) (List.sort [(1, 2), (2, 2)]) )

test_union_13 = TestCase (assertEqual "[Type: String] The union between bags with the same elements should have the max number of elements"
    (List.sort $ union [("ab", 1), ("cd", 2)] [("ab", 2), ("cd", 1)]  ) (List.sort [("ab", 2), ("cd", 2)]) )

test_union_14 = TestCase (assertEqual "[Type: Char] Shouls combine elements of both bags"
    (List.sort $ union [('A', 1), ('B', 2), ('C', 3)] [('A', 2), ('B', 1), ('D', 1)]  )
    (List.sort [('A', 2), ('B', 2), ('C', 3), ('D', 1)]) )

test_union_15 = TestCase (assertEqual "[Type: Number] Shouls combine elements of both bags"
    (List.sort $ union [(1, 1), (2, 2), (3, 3)] [(1, 2), (2, 1), (4, 1)]  )
    (List.sort [(1, 2), (2, 2), (3, 3), (4, 1)]) )

test_union_16 = TestCase (assertEqual "[Type: String] Shouls combine elements of both bags"
    (List.sort $ union [("ab", 1), ("cd", 2), ("ef", 3)] [("ab", 2), ("cd", 1), ("gh", 1)]  )
    (List.sort [("ab", 2), ("cd", 2), ("ef", 3), ("gh", 1)]) )

tests_union :: [Test]
tests_union =  [
    TestLabel "Test union between a Char Bag and an empty Bag" test_union_2,
    TestLabel "Test union between a Number Bag and an empty Bag" test_union_3,
    TestLabel "Test union between a String Bag and an empty Bag" test_union_4,

    TestLabel "Test union between an empty Bag and a Char Bag" test_union_5,
    TestLabel "Test union between an empty Bag and a Number Bag" test_union_6,
    TestLabel "Test union between an empty Bag and a String Bag" test_union_7,

    TestLabel "Test union between two bags with diferent Char elements" test_union_8,
    TestLabel "Test union between two bags with diferent Number elements" test_union_9,
    TestLabel "Test union between two bags with diferent String elements" test_union_10,

    TestLabel "Test union between two bags with same Char elements" test_union_11,
    TestLabel "Test union between two bags with same Number elements" test_union_12,
    TestLabel "Test union between two bags with same String elements" test_union_13,

    TestLabel "Test union between two bags with some equal and some diferent Char elements" test_union_14,
    TestLabel "Test union between two bags with some equal and some diferent Number elements" test_union_15,
    TestLabel "Test union between two bags with some equal and some diferent String elements" test_union_16]
