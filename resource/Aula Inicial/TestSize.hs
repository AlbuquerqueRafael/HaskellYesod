module TestSize (tests) where
import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

-- Test Size 1: Verificação de uma Bag vazia
testSize1 = TestCase (assertEqual "Test Size 1" 0 (size (Map.empty :: Map Char Integer)))

-- Test Size 2: Verificação de uma Bag com um elemento
t2 = insert 'A' Map.empty
testSize2 = TestCase (assertEqual "Test Size 2" 1 (size t2))

-- Test Size 3: Verificação de uma Bag com um elemento (dois do mesmo tipo)
t3 = insert 'A' Map.empty
t3_1 = insert 'A' t3
testSize3 = TestCase (assertEqual "Test Size 3" 1 (size t3_1))

-- Test Size 4: Verificação de uma Bag com dois elemento
t4 = insert 'A' Map.empty
t4_1 = insert 'A' t4
t4_2 = insert 'B' t4_1
testSize4 = TestCase (assertEqual "Test Size 4" 2 (size t4_2))

-- Test Size 5, 6, 7 e 8: Verificação de uma Bag com com inserts e removes
t5 = insert 'A' Map.empty
t5_1 = insert 'A' t5
t5_2 = insert 'B' t5_1
testSize5 = TestCase (assertEqual "Test Size 5" 2 (size t5_2))
t6 = remove 'B' t5_2
testSize6 = TestCase (assertEqual "Test Size 6" 1 (size t6))
t7 = remove 'A' t6
testSize7 = TestCase (assertEqual "Test Size 7" 1 (size t7))
t8 = remove 'A' t7
testSize8 = TestCase (assertEqual "Test Size 8" 0 (size t8))

tests = [testSize1, testSize2, testSize3, testSize4, testSize5, testSize6, testSize7, testSize8]
