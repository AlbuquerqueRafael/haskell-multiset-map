
module MultisetMap (
    Bag(Bag),
    insert,
    remove
    ) where
    import Data.Map (Map)
    import qualified Data.Map as Map
    
    data Bag k = Bag (Map k Integer) deriving(Eq, Show)

    -- insert :: Ord k => k -> Bag k -> Bag k
    insert elem (Bag b) = Bag (Map.insertWith (+) elem 1 b)

    -- remove :: Ord k => k -> Bag m -> Maybe Int
    remove elem (Bag m)
        | (Map.findWithDefault 1 elem m == 1) = (Bag (Map.delete elem m))
        | otherwise = Bag (Map.insertWith (+) elem (-1) m)            
