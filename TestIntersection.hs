import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

x = Bag (Map.empty)
y = Bag (Map.empty)

correct_insert elem (Bag m) = Bag (Map.insertWith (+) elem 1 m)

--Test Intersection 1: Testa a união de duas bags vazias.
testIntersection1 = TestCase (assertEqual "Test Insersecao 1" x (intersection x y :: Bag()))

--Test Intersection 2: Testa a interseção de uma bag vazia com uma bag contendo alguma coisa
k2 = correct_insert 'B' y
testIntersection2 = TestCase (assertEqual "Test Insersecao 2" x (intersection x k2 :: Bag Char))

--Test Intersection 3: Testa a interseção de duas bags sem elementos em comum
k3 = correct_insert 'B' x
l3 = correct_insert 'A' y
testIntersection3 = TestCase (assertEqual "Test Insersecao 3" x (intersection k3 l3 :: Bag Char))

--Test Intersection 4: Testa a interseção de duas bags que possuem elementos em comum
k4 = correct_insert 'A' x
l4 = correct_insert 'A' y
testIntersection4 = TestCase (assertEqual "Test Insersecao 4" (Bag (Map.fromList [('A',1)])) (intersection k4 l4))

--Test Intersection 5: Testa a interseção de duas bags que possuem elementos em comum
k5 = correct_insert 'A' x
k5_1 = correct_insert 'A' k5
l5 = correct_insert 'A' y
testIntersection5 = TestCase (assertEqual "Test Insersecao 5" (Bag (Map.fromList [('A',1)])) (intersection k5_1 l5))

--Test Intersection 6: Testa a interseção de duas bags com elementos em comum e sem elementos em comum
k6 = correct_insert 'A' x
k6_1 = correct_insert 'A' k6
k6_2 = correct_insert 'B' k6_1

l6 = correct_insert 'C' y
l6_1 = correct_insert 'A' l6
testIntersection6 = TestCase (assertEqual "Test Insersecao 6" (Bag (Map.fromList [('A',1)])) (intersection l6_1 k6_2))

--Test Intersection 7: Testa a interseção de duas bags com elementos em comum e sem elementos em comum
k7 = correct_insert 'B' x
k7_1 = correct_insert 'B' k7
k7_2 = correct_insert 'B' k7_1
k7_3 = correct_insert 'C' k7_2

l7 = correct_insert 'C' y
l7_1 = correct_insert 'A' l7
l7_2 = correct_insert 'A' l7_1
l7_3 = correct_insert '1' l7_2

testIntersection7 = TestCase (assertEqual "Test Insersecao 7" (Bag (Map.fromList [('C',1)])) (intersection k7_3 l7_3))

--Test Intersection 8: Testa a interseção de duas bags com elementos em comum e sem elementos em comum
l8 = correct_insert 'C' y
l8_1 = correct_insert 'A' l8
l8_2 = correct_insert 'A' l8_1
l8_3 = correct_insert '1' l8_2
l8_4 = correct_insert 'B' l8_3

k8 = correct_insert 'B' x
k8_1 = correct_insert 'B' k8
k8_2 = correct_insert 'B' k8_1
k8_3 = correct_insert 'C' k8_2
testIntersection8 = TestCase (assertEqual "Test Insersecao 8" (Bag (Map.fromList [('B',1), ('C',1)])) (intersection k8_3 l8_4))

--Test Intersection 9: Testa a interseção de duas bags com elementos em comum e sem elementos em comum
l9 = correct_insert "could be array char array" y
l9_1 = correct_insert "could be array char array" l9
l9_2 = correct_insert "could be array char array" l9_1

k9 = correct_insert "test" x
k9_1 = correct_insert "could be array char array" k9

testIntersection9 = TestCase (assertEqual "Test Insersecao 9" (Bag (Map.fromList [("could be array char array", 1)])) (intersection k9_1 l9_2))

--Test Intersection 10 Testa a interseção de duas bags com elementos em comum e sem elementos em comum
l10 = correct_insert 'A' y
l10_1 = correct_insert 'A' l10
l10_2 = correct_insert 'B' l10_1
l10_3 = correct_insert 'B' l10_2
l10_4 = correct_insert 'A' l10_3
l10_5 = correct_insert 'C' l10_4
l10_6 = correct_insert 'C' l10_5
l10_7 = correct_insert 'D' l10_6
l10_8 = correct_insert 'b' l10_7
l10_9 = correct_insert 'b' l10_8

k10 = correct_insert 'C' y
k10_1 = correct_insert 'D' k10
k10_2 = correct_insert 'U' k10_1
k10_3 = correct_insert 'A' k10_2
k10_4 = correct_insert 'A' k10_3
k10_5 = correct_insert 'A' k10_4
k10_6 = correct_insert 'C' k10_5
k10_7 = correct_insert 'b' k10_6

testUnion10 = TestCase (assertEqual "Test Union 10" (Bag (Map.fromList [('b',1), ('A',2), ('C',2),
                                                                        ('D',1)])) (intersection l10_9 k10_7))

tests = TestList [testIntersection1, testIntersection2, testIntersection3, testIntersection4, testIntersection5, testIntersection6, testIntersection7,
                  testIntersection8, testIntersection9, testIntersection9]

run = runTestTT tests
