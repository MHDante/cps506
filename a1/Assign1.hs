module Assign1 where
    trifecta :: (a -> b) -> (b -> c) -> (c -> d) -> (a -> d)
    trifecta a b c d = c$b$a$d
    
    mapCF :: a -> (a -> b -> c) -> [b] -> [c]
    mapCF a b c = map f c where f = b a
    
    allPairFunc :: [a -> b -> Bool] -> [(a,b)] -> Bool
    allPairFunc (h1:t1) (h2:t2) = ((h1 (fst h2)) (snd h2)) && allPairFunc t1 t2