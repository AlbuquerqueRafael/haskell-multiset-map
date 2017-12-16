
module MultisetMap (
    Bag(Bag),
    insert,
    remove,
    search,
    union
    ) where
    import Data.Map (Map)
    import qualified Data.Map as Map
    
    data Bag k = Bag (Map k Integer) deriving(Eq, Show)

    insert elem (Bag m) = Bag (Map.insertWith (+) elem 1 m)

    remove elem (Bag m)
        | (Map.findWithDefault 1 elem m == 1) = (Bag (Map.delete elem m))
        | otherwise = Bag (Map.insertWith (+) elem (-1) m)            

    search elem (Bag m)
        = case Map.lookup elem m of
            Nothing -> 0
            Just n -> n

    union (Bag m1) (Bag m2) = Bag (Map.unionWith f m1 m2)
            where
                f a b = if a > b then a else b