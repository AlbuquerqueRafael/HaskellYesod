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

    insert :: (Ord k) => k -> Map k Integer -> Map k Integer
    insert element = Map.insertWith (+) element 1

    remove :: (Ord k) => k -> Map k Integer -> Map k Integer
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

    intersection :: (Ord k) => Map k Integer -> Map k Integer -> Map k Integer
    intersection = Map.intersectionWith f
                where
                    f a b = if a < b then a else b

    minus :: (Ord k) => Map k Integer -> Map k Integer -> Map k Integer
    minus = Map.differenceWith f
                    where
                        f a b = if a <= b then Nothing else Just (a-b)

    inclusion :: (Ord k) => Map k Integer -> Map k Integer -> Bool
    inclusion = Map.isSubmapOfBy (<=)

    sumBag :: (Ord k) => Map k Integer -> Map k Integer -> Map k Integer
    sumBag = Map.unionWith (+)

    size :: (Ord k) => Map k Integer -> Int
    size = Map.size
