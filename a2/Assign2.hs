module Assign2 where
    --import Test.HUnit
    --import Test.QuickCheck
    
    data Sink t = Yes|
                No|
                AnInteger Int|
                ADouble Double|
                List [Sink t] |
                AString [Char] |
                Other t
            
    instance Show (Sink t) where
        show Yes = "True"
        show No = "False"
        show (AnInteger i) = show i
        show (ADouble d) = show d
        show (List l) = show l
        show (AString s) = show s
        show (Other _) = "This sink is Generic"

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

    instance Extract (Sink t) where
        asBool Yes = True
        asBool No = False
        asBool (AnInteger i) = asBool i
        asBool (ADouble d) = asBool d
        asBool (List l) = (length l) > 0 
        asBool (AString s) | (elem s ["True","true","yes","Yes"]) = True
                           | otherwise = False
        asBool (Other _) = error "Cannot Convert Generic Sink to Bool"
        
        asInteger Yes = 1
        asInteger No = 0
        asInteger (AnInteger i) = asInteger i
        asInteger (ADouble d) = asInteger d
        asInteger (List l) = length l
        asInteger (AString s) = read s :: Int
        asInteger (Other _) = error "Cannot Convert Generic Sink to Integer"

        asDouble Yes = 1.0
        asDouble No = 0.0
        asDouble (AnInteger i) = asDouble i
        asDouble (ADouble d) = asDouble d
        asDouble (List l) = asDouble $ length l
        asDouble (AString s) = read s :: Double
        asDouble (Other _) = error "Cannot Convert Generic Sink to Double"

    instance Num (Sink t) where
        Yes + Yes = Yes
        No + No = No
        No + Yes = Yes
        Yes + No = Yes
        (AnInteger a) + x = AnInteger (a + (asInteger x))
        x + (AnInteger a) = AnInteger (a + (asInteger x))
        (ADouble a) + x = ADouble (a + (asDouble x))
        x + (ADouble a) = ADouble (a + (asDouble x))
        (List a) + (List b) = List (a++b)
        (AString a) + (AString b) = AString(a++b)
        _ + _ = error "Operation Not Supported"

        Yes * Yes = Yes
        No * No = No
        No * Yes = No
        Yes * No = No
        (AnInteger a) * x = AnInteger (a * (asInteger x))
        x * (AnInteger a) = AnInteger (a * (asInteger x))
        (ADouble a) * x = ADouble (a * (asDouble x))
        x * (ADouble a) = ADouble (a * (asDouble x))
        _ * _ = error "Operation Not Supported"

        negate Yes = No
        negate No = Yes
        negate (AnInteger a) = AnInteger $ negate a
        negate (ADouble a) = ADouble $ negate a
        negate (List a) = List $ reverse a
        negate (AString a) = AString $ reverse a
        negate _ = error "Operation Not Supported"

        abs Yes = Yes
        abs No = No
        abs (AnInteger a) = AnInteger $ abs a
        abs (ADouble a) = ADouble $ abs a
        abs _ = error "Operation Not Supported"

        fromInteger x = AnInteger $ fromIntegral x

        signum Yes = No
        signum No = Yes
        signum (AnInteger a) = AnInteger $ signum a
        signum (ADouble a) = ADouble $ signum a
        signum (List a) = AnInteger $ signum $ length a
        signum (AString a) = AnInteger $ signum $ length a
        signum _ = error "Operation Not Supported"
        
    main :: IO ()
    main = print (asBool $ AString "no")