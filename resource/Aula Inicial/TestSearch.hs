module TestSearch (tests) where
import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

singletonBag = Map.fromList [('a', 1)]
singletonWithMoreElemBag = Map.fromList [('a', 2)]
normalBag = Map.fromList [('a', 2), ('b', 1), ('c', 9), ('d', 3)]

-- Test search 1: Procura por um elemento em uma bag vazia e retorna 0
testSearch1 = TestCase (assertEqual "Test search 1" 0 (search 'a' Map.empty))

-- Test search 2: Procura por um elemento que não existe em uma bag com apenas um elemento
testSearch2 = TestCase (assertEqual "Test search 2" 0 (search 'k' singletonBag))

-- Test search 3: Procura por um elemento em uma bag com vários elementos, mas não o
-- elemento que procuro
testSearch3 = TestCase (assertEqual "Test search 3" 0 (search 'k' normalBag))

-- Test search 4: Procura por um elemento em uma lista com apenas esse elemento
testSearch4 = TestCase (assertEqual "Test search 4" 1 (search 'a' singletonBag))

-- Test search 5: Procura por um elemento em uma lista com vários elementos iguais
-- do elemento que procuro
testSearch5 = TestCase (assertEqual "Test search 5" 2 (search 'a' singletonWithMoreElemBag))

-- Test search 6: Procura por um elemento em uma lista com vários elementos
testSearch6 = TestCase (assertEqual "Test search 6" 9 (search 'c' normalBag))

-- Test search 7: Procupra por um elemento que há apenas uma cópia dele na bag,
-- mas a bag possui vários outros elementos
testSearch7 = TestCase (assertEqual "Test search 7" 1 (search 'b' normalBag))

tests = [
    testSearch1, testSearch2, testSearch3,
    testSearch4, testSearch5, testSearch6,
    testSearch7]
