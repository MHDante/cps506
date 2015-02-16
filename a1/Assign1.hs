module Assign1(ass) where
    trifecta :: (a -> b) -> (b -> c) -> (c -> d) -> a -> d
    mapCF :: a -> (a -> b -> c) -> [b] -> [c]
    allPairFunc :: [a -> b -> Bool] -> [(a,b)] -> Bool
