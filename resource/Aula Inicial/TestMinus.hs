module TestMinus (tests) where
import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

bag1 = Map.fromList [('A',3),('B',1)]
bag2 = Map.fromList [('B',2),('A',1)]
-- Test
testMinus1 = TestCase (assertEqual "Test minus 1" (Map.fromList [('A',2)]) (minus bag1 bag2))

-- Test method with a bag with elements and another without elements
testMinus2 = TestCase (assertEqual "Test minus 2" (Map.fromList [('A',3),('B',1)]) (minus bag1 Map.empty))

-- Test method with two equal bags
testMinus3 = TestCase (assertEqual "Test minus 3" Map.empty (minus bag1 bag1))

-- Test method using bag with element count == 0
bag4 = Map.fromList [('A',0), ('B',0)]
testMinus4 = TestCase (assertEqual "Test minus 4" (Map.fromList [('A',3),('B',1)]) (minus bag1 bag4))

-- Test method with any equal element
bag5 = Map.fromList [('C',1),('D',3),('E',4)]
testMinus5 = TestCase (assertEqual "Test minus 5" bag5 (minus bag5 bag2))

-- Test method with negative counts
bag6 = (Map.fromList [('A',-3),('B',-1)])
testMinus6 = TestCase (assertEqual "Test minus 6" (Map.fromList [('A',6),('B',2)]) (minus bag1 bag6))

-- Test with first bag empty
testMinus7 = TestCase (assertEqual "Test minus 7" Map.empty (minus Map.empty bag1))

-- Common tests
testMinus8 = TestCase (assertEqual "Test minus 8" (Map.fromList [('A',3),('B',1)]) (minus bag1 bag5))

testMinus9 = TestCase (assertEqual "Test minus 9" (Map.fromList [('B',1)]) (minus bag2 bag1))

testMinus10 = TestCase (assertEqual "Test minus 10" (Map.fromList [('A',1),('B',2)]) (minus bag2 Map.empty))

testMinus11 = TestCase (assertEqual "Test minus 11" Map.empty (minus bag4 bag2))

testMinus12 = TestCase (assertEqual "Test minus 12" bag2 (minus bag2 bag5))

testMinus13 = TestCase (assertEqual "Test minus 13" (Map.fromList [('A',4),('B',3)]) (minus bag2 bag6))

tests = [testMinus1,testMinus2, testMinus3, testMinus4, testMinus5, testMinus6, testMinus7, testMinus8, testMinus9,
    testMinus10,testMinus11,testMinus12,testMinus13]
