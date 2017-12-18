module TestSum (tests) where
import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

emptyBag = Bag (Map.empty)

-- Test Sum 1: União de duas bags vazias
-- testSum1 = TestCase (assertEqual "Test Sum 1" (Bag (Map.fromList [])) (sumBag emptyBag emptyBag))

-- Test Sum 2: União de uma bag com elemento e uma vazia
t2 = insert 'A' emptyBag
testSum2 = TestCase (assertEqual "Test Sum 2" (Bag (Map.fromList [('A',1)])) (sumBag t2 emptyBag))

-- Test Sum 3: União de duas bags sem elementos em comum
t3 = insert 'A' emptyBag
t3_1 = insert 'B' emptyBag
testSum3 = TestCase (assertEqual "Test Sum 3" (Bag (Map.fromList [('A',1), ('B',1)])) (sumBag t3 t3_1))

-- Test Sum 4: União de duas bags com elementos em comum
t4 = insert 'A' emptyBag
t4_1 = insert 'A' emptyBag
testSum4 = TestCase (assertEqual "Test Sum 4" (Bag (Map.fromList [('A',2)])) (sumBag t4 t4_1))

-- Test Sum 5: União de duas bags com elementos em comum e sem elementos em comum
t5 = insert 'A' emptyBag
t5_1 = insert 'A' t5
t6 = insert 'B' emptyBag
t6_1 = insert 'B' t6
t6_2 = insert 'A' t6_1
testSum5 = TestCase (assertEqual "Test Sum 5" (Bag (Map.fromList [('A',3),('B',2)])) (sumBag t5_1 t6_2))

tests = [testSum2, testSum3, testSum4, testSum5]
