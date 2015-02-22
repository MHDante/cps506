module Assign1 where
    import Data.Char

    trifecta :: (a -> b) -> (b -> c) -> (c -> d) -> (a -> d)
    trifecta a b c d = c$b$a$d
    
    mapCF :: a -> (a -> b -> c) -> [b] -> [c]
    mapCF a b c = map f c where f = b a
    
    allPairFunc :: [a -> b -> Bool] -> [(a,b)] -> Bool
    allPairFunc (h1:t1) (h2:t2) = ((h1 (fst h2)) (snd h2)) && allPairFunc t1 t2
    allPairFunc [] [] = True
    allPairFunc (h:t) [] = False
    allPairFunc [] (h:t) = False
    
    divBy x y = (mod y x) == 0
    greaterThan x y = x>y
    equal x y = x==y
    
    fns1 = [divBy, greaterThan, equal]
    pairs1 = [(2,4),(5,3), (2,2)]
    tst1 = allPairFunc fns1 pairs1

    fns2 = [divBy, greaterThan, equal]
    pairs2 = [(2,4),(3,3), (2,2)]
    tst2 = allPairFunc fns2 pairs2

    fns3 = [divBy, greaterThan, equal]
    pairs3 = [(2,4),(3,3), (2,2), (1337,2)]
    tst3 = allPairFunc fns2 pairs2
    allPairFuncCorrect = (tst1 == True && tst2 == False && tst3 == False)
    
    tst4 = mapCF 2 divBy [3,4,5,6,7]
    tst5 = mapCF 5 greaterThan [3,4,5,6,7]
    mapCFCorrect = (tst4 == [False,True,False,True,False]) && (tst5 == [True,True,False,False,False])
    
    myToLower x = trifecta ord (+32) chr x
    tst6 = myToLower 'S'
    trifectaCorrect = tst6 == 's'
    
    main = do        
        if mapCFCorrect && trifectaCorrect && allPairFuncCorrect
            then print("Assignment Complete!")
            else print("Get back to work!")