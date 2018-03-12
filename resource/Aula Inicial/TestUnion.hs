module TestUnion (tests) where
import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

correctInsert :: (Ord k) => k -> Map k Integer -> Map k Integer
correctInsert element = Map.insertWith (+) element 1

--Test Union 1: Testa a união de duas bags vazias.
testUnion1 :: Test
testUnion1 = TestCase (assertEqual "Test Union 1" (Map.empty :: Map Char Integer) (Map.empty `union` Map.empty))

--Test Union 2: Testa a união de uma bag vazia com uma bag contendo alguma coisa
k2 = correctInsert 'B' Map.empty
testUnion2 :: Test
testUnion2 = TestCase (assertEqual "Test Union 2" (Map.fromList [('B',1)]) (Map.empty `union` k2))

--Test Union 3: Testa a união de duas bags sem elementos em comum
k3 = correctInsert 'B' Map.empty
l3 = correctInsert 'A' Map.empty
testUnion3 :: Test
testUnion3 = TestCase (assertEqual "Test Union 3" (Map.fromList [('A',1),('B',1)]) (k3 `union` l3))

--Test Union 4: Testa a união de duas bags que possuem elementos em comum
k4 = correctInsert 'A' Map.empty
l4 = correctInsert 'A' Map.empty
testUnion4 :: Test
testUnion4 = TestCase (assertEqual "Test Union 4" (Map.fromList [('A',1)]) (k4 `union` l4))

--Test Union 5: Testa a união de duas bags que possuem elementos em comum
k5 = correctInsert 'A' Map.empty
k5_1 = correctInsert 'A' k5
l5 = correctInsert 'A' Map.empty
testUnion5 :: Test
testUnion5 = TestCase (assertEqual "Test Union 5" (Map.fromList [('A',2)]) (k5_1 `union` l5))

--Test Union 6: Testa a união de duas bags com elementos em comum e sem elementos em comum
k6 = correctInsert 'A' Map.empty
k6_1 = correctInsert 'A' k6
k6_2 = correctInsert 'B' k6_1

l6 = correctInsert 'C' Map.empty
l6_1 = correctInsert 'A' l6
testUnion6 :: Test
testUnion6 = TestCase (assertEqual "Test Union 6" (Map.fromList [('A',2), ('B',1), ('C',1)]) (l6_1 `union` k6_2))

--Test Union 7: Testa a união de duas bags com elementos em comum e sem elementos em comum
k7 = correctInsert 'B' Map.empty
k7_1 = correctInsert 'B' k7
k7_2 = correctInsert 'B' k7_1
k7_3 = correctInsert 'C' k7_2

l7 = correctInsert 'C' Map.empty
l7_1 = correctInsert 'A' l7
l7_2 = correctInsert 'A' l7_1
l7_3 = correctInsert '1' l7_2

testUnion7 :: Test
testUnion7 = TestCase (assertEqual "Test Union 7" (Map.fromList [('1',1),('A',2), ('B',3), ('C',1)]) (k7_3 `union` l7_3))

--Test Union 8: Testa a união de duas bags com elementos em comum e sem elementos em comum
l8 = correctInsert 'C' Map.empty
l8_1 = correctInsert 'A' l8
l8_2 = correctInsert 'A' l8_1
l8_3 = correctInsert '1' l8_2

k8 = correctInsert 'B' Map.empty
k8_1 = correctInsert 'B' k8
k8_2 = correctInsert 'B' k8_1
k8_3 = correctInsert 'C' k8_2
testUnion8 :: Test
testUnion8 = TestCase (assertEqual "Test Union 8" (Map.fromList [('1',1),('A',2), ('B',3), ('C',1)]) (k8_3 `union` l8_3))

--Test Union 9: Testa a união de duas bags com elementos em comum e sem elementos em comum
l9 = correctInsert "could be array char array" Map.empty
l9_1 = correctInsert "could be array char array" l9
l9_2 = correctInsert "could be array char array" l9_1

k9 = correctInsert "test" Map.empty
k9_1 = correctInsert "could be array char array" k9

testUnion9 :: Test
testUnion9 = TestCase (assertEqual "Test Union 9" (Map.fromList [("could be array char array", 3), ("test",1)]) (k9_1 `union` l9_2))

--Test Union 10 Testa a união de duas bags com elementos em comum e sem elementos em comum
l10 = correctInsert 'A' Map.empty
l10_1 = correctInsert 'A' l10
l10_2 = correctInsert 'B' l10_1
l10_3 = correctInsert 'B' l10_2
l10_4 = correctInsert 'A' l10_3
l10_5 = correctInsert 'C' l10_4
l10_6 = correctInsert 'C' l10_5
l10_7 = correctInsert 'D' l10_6

k10 = correctInsert 'C' Map.empty
k10_1 = correctInsert 'D' k10
k10_2 = correctInsert 'U' k10_1
k10_3 = correctInsert 'A' k10_2
k10_4 = correctInsert 'A' k10_3
k10_5 = correctInsert 'A' k10_4
k10_6 = correctInsert 'C' k10_5
k10_7 = correctInsert 'b' k10_6

testUnion10 :: Test
testUnion10 = TestCase (assertEqual "Test Union 10" (Map.fromList [('b',1), ('A',3), ('B',2), ('C',2),
                                                                        ('D',1), ('U', 1)]) (l10_7 `union` k10_7))

tests :: [Test]
tests = [testUnion1, testUnion2, testUnion3, testUnion4, testUnion5, testUnion6, testUnion7, testUnion8, testUnion9, testUnion10]
