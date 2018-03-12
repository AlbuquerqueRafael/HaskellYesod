module TestInsert (tests) where
import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

emptyBag = Map.empty

-- Test Insert 1: Inclusão de um primeiro elemento na Bag vazia
testInsert1 = TestCase (assertEqual "Test Insert 1" (Map.fromList [('A',1)]) (insert 'A' emptyBag))

-- Test Insert 2: Inclusão de um elemento já existente na Bag [insert A e insert A]
t2 = insert 'A' emptyBag
testInsert2 = TestCase (assertEqual "Test Insert 2" (Map.fromList [('A',2)]) (insert 'A' t2))

-- Test Insert 3: Inclusão de um elemento distinto na Bag [insert A e insert B]
t3 = insert 'A' emptyBag
testInsert3 = TestCase (assertEqual "Test Insert 3" (Map.fromList [('A',1), ('B',1)]) (insert 'B' t3))

-- Test Insert 4 e 5: Inclusão de um elemento distinto na Bag, remove-lo e adicionando outro [insert A, insert B, remove A e insert C]
t4 = insert 'A' emptyBag
t4_1 = insert 'B' t4
testInsert4 = TestCase (assertEqual "Test Insert 4" (Map.fromList [('A',1), ('B',1)]) t4_1)
t5 = remove 'A' t4_1
testInsert5 = TestCase (assertEqual "Test Insert 5" (Map.fromList [('B',1), ('C',1)]) (insert 'C' t5))

tests = [testInsert1, testInsert2, testInsert3, testInsert4, testInsert5]
