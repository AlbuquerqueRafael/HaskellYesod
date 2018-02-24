module TestIntersection (tests) where
import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

correctInsert :: (Ord k) => k -> Map k Integer -> Map k Integer
correctInsert element = Map.insertWith (+) element 1

--Test Intersection 1: Testa a união de duas bags vazias.
testIntersection1 :: Test
testIntersection1 = TestCase (assertEqual "Test Insersecao 1" Map.empty (intersection Map.empty Map.empty :: Map Char Integer))

--Test Intersection 2: Testa a interseção de uma bag vazia com uma bag contendo alguma coisa
k2 = correctInsert 'B' Map.empty
testIntersection2 :: Test
testIntersection2 = TestCase (assertEqual "Test Insersecao 2" Map.empty (intersection Map.empty k2))

--Test Intersection 3: Testa a interseção de duas bags sem elementos em comum
k3 = correctInsert 'B' Map.empty
l3 = correctInsert 'A' Map.empty
testIntersection3 :: Test
testIntersection3 = TestCase (assertEqual "Test Insersecao 3" Map.empty (intersection k3 l3))

--Test Intersection 4: Testa a interseção de duas bags que possuem elementos em comum
k4 = correctInsert 'A' Map.empty
l4 = correctInsert 'A' Map.empty
testIntersection4 :: Test
testIntersection4 = TestCase (assertEqual "Test Insersecao 4" (Map.fromList [('A',1)]) (intersection k4 l4))

--Test Intersection 5: Testa a interseção de duas bags que possuem elementos em comum
k5 = correctInsert 'A' Map.empty
k5_1 = correctInsert 'A' k5
l5 = correctInsert 'A' Map.empty
testIntersection5 :: Test
testIntersection5 = TestCase (assertEqual "Test Insersecao 5" (Map.fromList [('A',1)]) (intersection k5_1 l5))

--Test Intersection 6: Testa a interseção de duas bags com elementos em comum e sem elementos em comum
k6 = correctInsert 'A' Map.empty
k6_1 = correctInsert 'A' k6
k6_2 = correctInsert 'B' k6_1

l6 = correctInsert 'C' Map.empty
l6_1 = correctInsert 'A' l6
testIntersection6 :: Test
testIntersection6 = TestCase (assertEqual "Test Insersecao 6" (Map.fromList [('A',1)]) (intersection l6_1 k6_2))

--Test Intersection 7: Testa a interseção de duas bags com elementos em comum e sem elementos em comum
k7 = correctInsert 'B' Map.empty
k7_1 = correctInsert 'B' k7
k7_2 = correctInsert 'B' k7_1
k7_3 = correctInsert 'C' k7_2

l7 = correctInsert 'C' Map.empty
l7_1 = correctInsert 'A' l7
l7_2 = correctInsert 'A' l7_1
l7_3 = correctInsert '1' l7_2
testIntersection7 :: Test
testIntersection7 = TestCase (assertEqual "Test Insersecao 7" (Map.fromList [('C',1)]) (intersection k7_3 l7_3))

--Test Intersection 8: Testa a interseção de duas bags com elementos em comum e sem elementos em comum
l8 = correctInsert 'C' Map.empty
l8_1 = correctInsert 'A' l8
l8_2 = correctInsert 'A' l8_1
l8_3 = correctInsert '1' l8_2
l8_4 = correctInsert 'B' l8_3

k8 = correctInsert 'B' Map.empty
k8_1 = correctInsert 'B' k8
k8_2 = correctInsert 'B' k8_1
k8_3 = correctInsert 'C' k8_2
testIntersection8 :: Test
testIntersection8 = TestCase (assertEqual "Test Insersecao 8" (Map.fromList [('B',1), ('C',1)]) (intersection k8_3 l8_4))

--Test Intersection 9: Testa a interseção de duas bags com elementos em comum e sem elementos em comum
l9 = correctInsert "could be array char array" Map.empty
l9_1 = correctInsert "could be array char array" l9
l9_2 = correctInsert "could be array char array" l9_1

k9 = correctInsert "test" Map.empty
k9_1 = correctInsert "could be array char array" k9
testIntersection9 :: Test
testIntersection9 = TestCase (assertEqual "Test Insersecao 9" (Map.fromList [("could be array char array", 1)]) (intersection k9_1 l9_2))

--Test Intersection 10 Testa a interseção de duas bags com elementos em comum e sem elementos em comum
l10 = correctInsert 'A' Map.empty
l10_1 = correctInsert 'A' l10
l10_2 = correctInsert 'B' l10_1
l10_3 = correctInsert 'B' l10_2
l10_4 = correctInsert 'A' l10_3
l10_5 = correctInsert 'C' l10_4
l10_6 = correctInsert 'C' l10_5
l10_7 = correctInsert 'D' l10_6
l10_8 = correctInsert 'b' l10_7
l10_9 = correctInsert 'b' l10_8

k10 = correctInsert 'C' Map.empty
k10_1 = correctInsert 'D' k10
k10_2 = correctInsert 'U' k10_1
k10_3 = correctInsert 'A' k10_2
k10_4 = correctInsert 'A' k10_3
k10_5 = correctInsert 'A' k10_4
k10_6 = correctInsert 'C' k10_5
k10_7 = correctInsert 'b' k10_6

tests =  [testIntersection1, testIntersection2, testIntersection3, testIntersection4, testIntersection5, testIntersection6, testIntersection7,
                  testIntersection8, testIntersection9, testIntersection9]
