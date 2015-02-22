module Assign2 where

    data Sink = Yes|
                No|
                AnInteger Int|
                ADouble Double|
                List [Sink] |
                AString [Char]
            
    instance Show Sink where
        show Yes = "True"
        show No = "False"
        show (AnInteger i) = show i
        show (ADouble d) = show d
        show (List l) = show l
        show (AString s) = show s

    class Extract t where
        asBool:: t->Bool
        asInteger:: t-> Int
        asDouble::t-> Double

    instance Extract Bool where
        asBool = id
        asInteger True = 1
        asInteger False = 0
        asDouble True = 1.0
        asDouble False = 0.0

    instance Extract Int where
        asBool 0 = False
        asBool _ = True
        asInteger = id
        asDouble i = fromIntegral i

    instance Extract Double where
        asBool 0.0 = False
        asBool _ = True
        asInteger d = truncate d
        asDouble = id

    instance Extract Sink where
        asBool Yes = True
        asBool No = False
        asBool (AnInteger i) = asBool i
        asBool (ADouble d) = asBool d
        asBool (List l) = (length l) > 0 
        asBool (AString s) | (elem s ["True","true","yes","Yes"]) = True
                           | otherwise = False
        asInteger Yes = 1
        asInteger No = 0
        asInteger (AnInteger i) = asInteger i
        asInteger (ADouble d) = asInteger d
        asInteger (List l) = length l
        asInteger (AString s) = read s :: Int

        asDouble Yes = 1.0
        asDouble No = 0.0
        asDouble (AnInteger i) = asDouble i
        asDouble (ADouble d) = asDouble d
        asDouble (List l) = asDouble $ length l
        asDouble (AString s) = read s :: Double

    
    main :: IO ()
    main = print (asBool $ AString "no")