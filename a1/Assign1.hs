module Assign1 where
	
	--Name:   Dante Camarena
	--Course: CPS506, Winter 2015, Assignment #1
	--Due:    2015.02.16 23:59
	--Credit: This is entirely my own work.
    
	import Data.Char

    trifecta :: (a -> b) -> (b -> c) -> (c -> d) -> a -> d
    trifecta a b c d = c$b$a$d
    
    mapCF :: a -> (a -> b -> c) -> [b] -> [c]
    mapCF a b  = map f where f = b a
    
    allPairFunc :: [a -> b -> Bool] -> [(a,b)] -> Bool
    allPairFunc (h1:t1) (h2:t2) = ((h1 (fst h2)) (snd h2)) && allPairFunc t1 t2
    allPairFunc [] [] = True
    allPairFunc (_:_) [] = False
    allPairFunc [] (_:_) = False

    divBy :: Int -> Int -> Bool
    divBy x y = (mod y x) == 0
    
    greaterThan :: Int -> Int -> Bool
    greaterThan x y = x>y
    
    equal :: Int -> Int -> Bool
    equal x y = x==y
    
    fns1 :: [Int -> Int -> Bool]
    fns1 = [divBy, greaterThan, equal]
    
    pairs1 :: [(Int, Int)]
    pairs1 = [(2,4),(5,3), (2,2)]
    
    tst1 :: Bool
    tst1 = allPairFunc fns1 pairs1

    
    fns2 :: [Int -> Int -> Bool]
    fns2 = [divBy, greaterThan, equal]
    
    pairs2 :: [(Int, Int)]
    pairs2 = [(2,4),(3,3), (2,2)]
    
    tst2 :: Bool
    tst2 = allPairFunc fns2 pairs2

    fns3 :: [Int -> Int -> Bool]
    fns3 = [divBy, greaterThan, equal]
    
    pairs3 :: [(Integer, Integer)]
    pairs3 = [(2,4),(3,3), (2,2), (1337,2)]
    
    tst3 :: Bool
    tst3 = allPairFunc fns2 pairs2
    
    allPairFuncCorrect :: Bool
    allPairFuncCorrect = (tst1 == True && tst2 == False && tst3 == False)
    
    tst4 :: [Bool]
    tst4 = mapCF 2 divBy [3,4,5,6,7]
    tst5 :: [Bool]
    tst5 = mapCF 5 greaterThan [3,4,5,6,7]
    mapCFCorrect :: Bool
    mapCFCorrect = (tst4 == [False,True,False,True,False]) && (tst5 == [True,True,False,False,False])
    
    myToLower :: Char -> Char
    myToLower x = trifecta ord (+32) chr x
    tst6 :: Char
    tst6 = myToLower 'S'
    trifectaCorrect :: Bool
    trifectaCorrect = tst6 == 's'
    
    main :: IO ()
    main = do        
        if mapCFCorrect && trifectaCorrect && allPairFuncCorrect
            then print("Assignment Complete!")
            else print("Get back to work!")