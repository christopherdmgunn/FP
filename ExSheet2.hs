import Data.Char
import Control.Monad

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
    forM_ allLines putStrLn


calculator :: IO()
calculator = do
    
    putStrLn "Give me the operator you would like to use."
    operator <- getLine

    putStrLn "Now give me the first number."
    num1 <- getLine
    let firstNum = read num1 :: Int
    
    putStrLn "Now give me the second number."
    num2 <- getLine
    let secondNum = read num2 :: Int

    case operator of
        "-" -> print (firstNum - secondNum)
        "*" -> print (firstNum * secondNum)
        _ -> print (firstNum + secondNum)


infiniteAppend :: IO ()
infiniteAppend = do
    putStr "Please enter filepath "
    filePath <- getLine

    forever (helpAppend filePath)


isbnCheck :: [Int] -> String
isbnCheck digits = 
    if digit == 10 
        then "X" 
        else (show digit)
    where
        coefficients = reverse [2..10]
        pairs = zipWith (*) coefficients digits
        summed = sum pairs
        digit = 11 - (summed `mod` 11)



--Helper Functions
helpAppend :: FilePath -> IO ()
helpAppend filePath = do
    putStrLn "Please enter text to append to file"
    text <- getLine
    appendFile filePath text