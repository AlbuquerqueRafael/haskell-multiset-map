import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

bag1 = (Bag (Map.fromList [('A',3),('B',1)]))
bag2 = (Bag (Map.fromList [('B',2),('A',1)]))

-- Test if some bag is a subset of this
testInclusion1 = TestCase (assertEqual "Test inclusion 1" True (inclusion bag1 bag1))

bag3 = Bag (Map.empty)
-- Test if empty bag is a subset of another
testInclusion2 = TestCase (assertEqual "Test inclusion 2" True (inclusion bag3 bag1))

-- Test if some big bag can be included on another small bag
bag4 = Bag (Map.fromList [('A',5), ('B', 6), ('C',7) , ('D',8)])
testInclusion3 = TestCase (assertEqual "Test inclusion 3" False (inclusion bag4 bag1))

-- Test using small bag ( with all another bag elements )
bag5 = Bag (Map.fromList [('A',3),('B',2), ('D',2)])
testInclusion4 = TestCase (assertEqual "Test inclusion 4" True (inclusion bag5 bag4))

testInclusion5 = TestCase (assertEqual "Test inclusion 5" False (inclusion bag4 bag5))

-- Test using bag with some attributes , but greater counts
bag6 = Bag (Map.fromList [('A',10),('B',20), ('D',30)])

testInclusion6 = TestCase (assertEqual "Test inclusion 6" True (inclusion bag4 bag6))

testInclusion7 = TestCase (assertEqual "Test inclusion 7" False (inclusion bag6 bag4))


testInclusion8 = TestCase (assertEqual "Test inclusion 8" False (inclusion bag4 bag3))

-- Test with negavite numbers
bag7 = Bag (Map.fromList [('A',-3),('B',-2),('C',-2)])
testInclusion9 = TestCase (assertEqual "Test inclusion 9" False (inclusion bag7 bag5))

testInclusion10 = TestCase (assertEqual "Test inclusion 8" True (inclusion bag2 bag6))

tests = TestList[testInclusion1, testInclusion2, testInclusion3, testInclusion4, testInclusion5, testInclusion6,
		 testInclusion7, testInclusion8, testInclusion9, testInclusion10]

run = runTestTT tests