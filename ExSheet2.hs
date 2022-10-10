import Data.Char {- Used for toUpper -}
import Control.Monad {- Used for forever -}

{- Booleans, Comparisons, Tuples, and More Lists -}

-- Q1
isAscending4 :: Int -> Int -> Int -> Int -> Bool
isAscending4 x1 x2 x3 x4 = x1 < x2 && x2 < x3 && x3 < x4

-- Q2
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Q3
rotate :: (a, b, c) -> (c, a, b)
rotate (x, y, z) = (z, x, y)

-- Q4
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

-- Q5
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y

-- Q6
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f y x = f x y

-- Q7
mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f1 f2 (x, y) = (f1 x, f2 y)

-- Q8
mapPair2 :: ((a, b) -> (c, d)) -> (a, b) -> (c, d)
mapPair2 f pair = f pair
-- Equivalently, mapPair2 f (x, y) = f (x, y)

-- Q9
f :: (a, b) -> (a -> c) -> (b -> d) -> ((c, d) -> e) -> e
f (x, y) f1 f2 f3 = f3 (cVal, dVal)
    where
        cVal = f1 x
        dVal = f2 y

{- IO -}

-- Q1
echoCaps :: IO ()
echoCaps = do
    str <- getLine
    putStrLn [ toUpper x | x <- str ]

{- We can equivalently do the list comprehension with a map:
 - putStrLn (map toUpper str) -}

-- Q2
echoFile :: FilePath -> IO ()
echoFile path = do
    str <- readFile path
    let strLines = lines str
    forM_ strLines (putStrLn)

-- Q3
getOpFun :: String -> (Int -> Int -> Int)
getOpFun "+" = (+)
getOpFun "-" = (-)
getOpFun "*" = (*)
getOpFun _ = (+)

-- Note: As discussed in the class, we don't actually need the
-- :: Int annotations on 'read' here because we have the type
-- information from getOpFun. That said, it's generally good
-- practice to put them in.
calculator :: IO ()
calculator = do
    putStrLn $ "Which operation?"
    op <- getLine
    let opFun = getOpFun op
    putStrLn $ "Argument 1?"
    arg1 <- getLine
    putStrLn $ "Argument 2?"
    arg2 <- getLine
    let result = opFun ((read arg1) :: Int) ((read arg2) :: Int)
    putStrLn "Result: "
    putStrLn $ show result

-- Q4
infiniteAppend :: IO ()
infiniteAppend = do
    path <- getLine
    forever (do
        input <- getLine
        appendFile path (input ++ "\n"))

{- Further Exercises -}
isbnCheck :: [Int] -> String
isbnCheck digits = if digit == 10 then "X" else (show digit)
    where
        coefficients = reverse [2..10]
        pairs = zipWith (*) coefficients digits
        summed = sum pairs
        digit = 11 - (summed `mod` 11)