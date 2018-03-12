module BagInclusionTest(tests_inclusion) where

import Test.HUnit
import Student

test_inclusion_1 =  "Test if Bag A is included on an empty Bag B" ~:
                    "Bag A is included on an empty Bag B should return False"  ~:
                    (inclusion [(2, 1)] []) ~?= False

test_inclusion_2 =  "Test if an empty Bag A is included on Bag B" ~:
                    "Test if an empty Bag A is included on Bag B should return True"  ~:
                    (inclusion [] [(2, 1)]) ~?= True

test_inclusion_3 =  "Test if a Bag A is included in itself" ~:
                    "Bag A is included in itself should return True"  ~:
                    (inclusion [(2, 1), (3, 1)] [(2, 1), (3, 1)]) ~?= True

test_inclusion_4 =  "Test if a Bag A is included in itself" ~:
                    "Bag A is included in itself should return True"  ~:
                    (inclusion [(3, 1), (2, 1)] [(2, 1), (3, 1)]) ~?= True

test_inclusion_5 =  "Test if Bag A is included on Bag B when A and B are disjointed" ~:
                    "Test if Bag A is included on Bag B when A and B are disjointed should return False"  ~:
                    (inclusion [(2, 1)] [(1, 2)]) ~?= False

test_inclusion_6 =  "Test if Bag A is included on Bag B when A is larger then B" ~:
                    "Bag A is included on Bag B when A is larger then B should return False"  ~:
                    (inclusion [(2, 1), (3, 1)] [(2, 1)]) ~?= False

test_inclusion_7 =  "Test if Bag A is included on Bag B when A has more of the same element than B" ~:
                    "Bag A is included on Bag B when A has more of the same element than B should return False"  ~:
                    (inclusion [('a', 2)] [('a', 1)]) ~?= False

test_inclusion_8 =  "Test if Bag A is included on Bag B when A has less of the same element than B" ~:
                    "Bag A is included on Bag B when A has less of the same element than B should return True"  ~:
                    (inclusion [('a', 1)] [('a', 2)]) ~?= True

tests_inclusion :: [Test]
tests_inclusion = [
                test_inclusion_1, test_inclusion_2, test_inclusion_3,
                test_inclusion_4, test_inclusion_5, test_inclusion_6,
                test_inclusion_7, test_inclusion_8]
