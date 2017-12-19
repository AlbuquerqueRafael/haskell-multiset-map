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

    insert elem m = Map.insertWith (+) elem 1 m

    remove elem m
        | (Map.findWithDefault 1 elem m == 1) = (Map.delete elem m)
        | otherwise = Map.insertWith (+) elem (-1) m

    search elem m
        = case Map.lookup elem m of
            Nothing -> 0
            Just n -> n

    union m1 m2 = Map.unionWith f m1 m2
            where
                f a b = if a > b then a else b

    intersection :: (Ord c, Ord k) => Map k c -> Map k c -> Map k c
    intersection m1 m2 = Map.intersectionWith f m1 m2
                where
                    f a b = if a < b then a else b

    minus m1 m2 = Map.differenceWith f m1 m2
                    where
                        f a b = if(a <= b) then Nothing else Just (a-b)

    inclusion m1 m2 = Map.isSubmapOfBy (<=) m1 m2

    sumBag  m1  m2 = Map.unionWith (+) m1 m2

    size  m = Map.size m
