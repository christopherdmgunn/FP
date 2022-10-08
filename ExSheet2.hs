import Data.Char

--Functions 
--Ex1
isAscending4 :: Int -> Int -> Int -> Int -> Bool
isAscending4 i j k l  = i<j && j<k && k<l

--Ex2
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

--Ex3
rotate :: (a, b, c) -> (c, a, b)
rotate (a, b, c) = (c, a, b)

--Ex4
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f1 a b = f1 (a, b) 

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f1 (a, b)  = f1 a b

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f1 b a = f1 a b

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f1 f2 (a, b) = (f1 a, f2 b)

mapPair2 :: ((a, b) -> (c, d)) -> (a, b) -> (c, d)
mapPair2 f1 (a,b) = f1 (a,b)

f :: (a, b) -> (a -> c) -> (b -> d) -> ((c, d) -> e) -> e
f (a,b) f1 f2 f3 = f3 (f1 a, f2 b)

--list is used when same type and dont know how many
--tuple used when different type and we know how many
--ie list function cant use tuple, generate the first n integers and store them
--tuple not list, match pairs of integers with string values (1, "name") etc

--------------------------------------------------

--IO
echoCaps :: IO ()
echoCaps = do
    x <- getLine
    let upper = map toUpper x
    putStrLn upper


echoFile :: FilePath -> IO ()
echoFile path = do
    x <- readFile path
    let allLines = lines x
    print (allLines)
    forM_ allLines putStrLn
            
