import Data.Bool (bool)
-- Ex 1 
max2 :: Int -> Int -> Int
max2 x y  = 
    if y < x  
        then x
    else 
        y

--Ex 2
max3 :: Int -> Int -> Int -> Int 
max3 x y z  = max2 z (max2 x y)  

--Ex3 
--Wrong
f :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
f intToSting stringToBool z = stringToBool(intToSting z) 

--correct answer
f' :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
f' f1 f2 = \i -> f2 (f1 i) 
--or f' f1 f2 = f2 . f1

--Ex 4
--Wrong
g :: (Int -> Bool) -> (Bool -> String) -> Int -> String
g intToBool boolToString z = boolToString (intToBool z)

--correct answer
g' :: (Int -> Bool) -> (Bool -> String) -> Int -> String
g' f1 f2 i = f2 (f1 i)

--Ex 5
twice :: (Int -> Int) -> Int -> Int
twice funct x = funct (funct x)

--Ex 6
gravPull :: Float -> Float -> Float -> Float
gravPull m1 m2 d = 
    let 
        g = 6.67*(10**(-11))
        nom = m1 * m2
        denom = d^2
    in 
        (g * nom)/denom

--or from Lecturer
force :: Float -> Float -> Float -> Float
force m1 m2 d = (g*m1*m2)/(d^2)
    where g = 6.67*(10**(-11))

-- List comprehensions

--Ex 1
divBy3 :: [Int]
divBy3 = [x | x<-[1..30], x `mod` 3 == 0 ]

--Ex2
triangles :: Int -> [Int]
triangles n = [x | x <-[0..n], x<-[x*(x+1) `div` 2]]

--Ex 3 
primes :: Int -> [Int]
primes n = [x | x <-[2..n], null([y | y <-[2..x-1],  rem x y == 0])]

--Ex 4
flatten :: [[a]] -> [a]
flatten list = [y | x <- list, y <- x]

--Ex bonus 
pythagTrip :: Int -> [[Int]]
pythagTrip n = [[x,y,z] | x <-[1..n], y <-[1..n], z<-[1..n], (x^2)+(y^2)==(z^2)]


main :: IO ()
main = do
    
    putStr "Functions \n~~~~~~~~~~~~~~~~~~~~~~~ \n"

    --Ex 1
    print ("Ex1 max of two numbers 1 and 5 : " ++ show (max2 1 5))

    -- Ex 2
    print("Ex2 max of three numbers 1 5 8 : " ++ show (max3 1 5 8))

    --Ex 3
    print("Ex3 int 5 to sting to bool : " ++ show (f show stringToBool 5))
    
    --Ex 4 
    print ("Ex4 int 5 to bool to int to string : " ++ show(g intToBool show 5))

    --Ex 5
    print("Ex5 call a function (doubleX) twice on 5 : " ++ show(twice doubleX 5))

    --Ex 6
    print("Ex6 gravitational pull between objects of mass 50 1 meter apart : " ++ show(gravPull 50.0 50.0 1.0))

    -- List comprehensions
    putStr "\n List Comprehensions \n~~~~~~~~~~~~~~~~~~~~~~~ \n"

    --Ex 1
    print("Ex1 0 to 30 divisable by 3 : " ++ show divBy3)

    --Ex 2
    print("Ex2 Displays the first 20 triangle numbers : " ++ show (triangles 20))

    --Ex 3
    print ("Ex3 display all prime numbers below 10 : " ++ show (primes 10))
    
    --Ex 4
    print("Ex4 flatten [[1,2,3], [4,5,6], [7,8,9]] into a single list : " ++ show (flatten  [[1,2,3], [4,5,6], [7,8,9]]))

    print(pythagTrip 50)
--Helpers 

--string to bool
stringToBool :: String -> Bool
stringToBool s = not (null s)

--int to bool 
intToBool :: Int -> Bool
intToBool i = i /= 0

--double 
doubleX :: Int -> Int
doubleX x = x * 2