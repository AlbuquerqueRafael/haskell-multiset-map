import MultisetMap
import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map

emptyBag = Bag (Map.empty)

-- Test remove 1
testRemove1 =
    TestCase (assertEqual "Test remove 1" emptyBag
        (remove 'a' (Bag (Map.fromList [('a', 1)]))))

-- Test remove 2
testRemove2 =
    TestCase (assertEqual "Test remove 2" emptyBag remove 'a' (Bag Map.empty))

tests = TestList[testRemove1, testRemove2]

run = runTestTT tests
