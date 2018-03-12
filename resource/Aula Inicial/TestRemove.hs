module TestRemove (tests) where
import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

singletonBag = Map.fromList [('a', 1)]
singletonWithMoreElemBag = Map.fromList [('a', 2)]
normalBag = Map.fromList [('a', 2), ('b', 1), ('c', 9), ('d', 3)]

-- Test remove 1: Remoção de um elemento em uma bag com um único elemento retorna uma bag vazia
testRemove1 =
    TestCase (assertEqual "Test remove 1" Map.empty
        (remove 'a' singletonBag))

-- Test remove 2: Remoção de um elemento em uma bag com vários elementos iguais
testRemove2 =
    TestCase (assertEqual "Test remove 2" singletonBag (remove 'a' singletonWithMoreElemBag))

-- Test remove 3: Remoção de um elemento em uma bag vazia retorna lista vazia
testRemove3 =
    TestCase (assertEqual "Test remove 3" Map.empty (remove 'a' Map.empty))

-- Test remove 4: Remoção de elementos que não estão em um Bag com elementos não altera nada
testRemove4 =
    TestCase (assertEqual "Test remove 4" normalBag (remove 'k' normalBag))

-- Test remove 5: Remoção de um elemento que não possui repetições em uma bag com vários
-- elementos diferentes retorna a bag sem o elemento
testRemove5 =
    TestCase (assertEqual "Test remove 5" (Map.fromList [('a', 2), ('c', 9), ('d', 3)])
        (remove 'b' normalBag))

-- Test remove 6: Remoção de um elemento que possui várias cópias dele na bag e vários outros
-- elementos diferentes
testRemove6 =
    TestCase (assertEqual "Test remove 6" (Map.fromList [('a', 2), ('b', 1), ('c', 8), ('d', 3)])
        (remove 'c' normalBag))

-- Test remove 7: Remoção de um elemento que não está em uma bag com vários elementos não altera a bag
testRemove7 =
    TestCase (assertEqual "Test remove 7" normalBag (remove 'k' normalBag))

tests = [
    testRemove1, testRemove2, testRemove3,
    testRemove4, testRemove5, testRemove6,
    testRemove7]
