module MultisetMap (
    insert,
    remove,
    search,
    union,
    intersection,
    minus,
    inclusion,
    sumBag,
    size
    ) where
    import Data.Map (Map)
    import qualified Data.Map as Map

    insert :: (Num a, Ord k) => k -> Map k a -> Map k a
    insert element = Map.insertWith (+) element 1

    remove :: (Eq a, Num a, Ord k) => k -> Map k a -> Map k a
    remove element m
        | Map.findWithDefault 1 element m == 1 = Map.delete element m
        | otherwise = Map.insertWith (+) element (-1) m

    search :: Ord k => k -> Map k Integer -> Integer
    search element m
        = case Map.lookup element m of
            Nothing -> 0
            Just n -> n

    union :: Ord k => Map k Integer -> Map k Integer -> Map k Integer
    union = Map.unionWith f
            where
                f a b = if a > b then a else b

    intersection :: (Ord c, Ord k) => Map k c -> Map k c -> Map k c
    intersection = Map.intersectionWith f
                where
                    f a b = if a < b then a else b

    minus :: (Ord a, Ord k, Num a) => Map k a -> Map k a -> Map k a
    minus = Map.differenceWith f
                    where
                        f a b = if a <= b then Nothing else Just (a-b)

    inclusion :: (Ord a, Ord k) => Map k a -> Map k a -> Bool
    inclusion = Map.isSubmapOfBy (<=)

    sumBag :: (Num a, Ord k) => Map k a -> Map k a -> Map k a
    sumBag = Map.unionWith (+)

    size :: Map k a -> Int
    size = Map.size
