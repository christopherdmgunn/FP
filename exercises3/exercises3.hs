import Data.List

{- Recursive functions on lists -}
isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [x] = True
isAscending (x:y:xs) =  x < y && isAscending (y:xs)

myTake :: Int -> [a] -> [a]
myTake 0 x = []
myTake index (x:xs) = 
    if index > length xs + 1 
    then x : myTake (length xs) xs 
    else x : myTake (index-1) xs

dropOdds :: [a] -> [a]
dropOdds [x] = []
dropOdds (x:xs) = evens xs

evens :: [a] -> [a]
evens [x] = [x]
evens (x:xs) = x : dropOdds xs

myIntersperse :: [a] -> a -> [a]
myIntersperse [x] num = x : [num]
myIntersperse (x:xs) num = x : num : myIntersperse xs num

myReverseRec :: [a] -> [a]
myReverseRec [] = [] 
myReverseRec [x] = [x]
myReverseRec (x:xs) = myReverseRec xs ++ [x]

myReverseFold :: [a] -> [a]
myReverseFold list = foldl (\acc elt -> elt:acc) [] list

{- Higher-order functions -}
myMap :: (a -> b) -> [a] -> [b]
myMap f1 [] = []
myMap f1 (x:xs) = f1 x : myMap f1 xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ _ _ = undefined

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ _ _ = undefined

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined

{- Data structures -}
data ArithExpr =
      Add ArithExpr ArithExpr
    | Sub ArithExpr ArithExpr
    | Mul ArithExpr ArithExpr
    | Div ArithExpr ArithExpr
    | Value Int

evalExpr :: ArithExpr -> Int
evalExpr (Value a) = a
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Sub a b) = evalExpr a - evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
evalExpr (Div a b) = evalExpr a `div` evalExpr b

showExpr :: ArithExpr -> String
showExpr _ = undefined

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead(x:_) = Just x

safeDiv :: Int -> Int -> Maybe Int
safeDiv i 0 = Nothing 
safeDiv i j =  Just (i `div ` j)

addSafeDiv :: (Int, Int) -> (Int, Int) -> Maybe Int
addSafeDiv (i,j) (k,l) = fmap sum $ sequence [safeDiv i j, safeDiv k l]

data Tree a = Leaf | Node a [Tree a]

sumTree :: Tree Int -> Int
sumTree Leaf = 0
sumTree (Node x children) = x + (sum (map sumTree children))

reverseTree :: Tree a -> Tree a
reverseTree _ = undefined

data BinaryTree a = BLeaf | BNode a (BinaryTree a) (BinaryTree a)

toTree :: BinaryTree a -> Tree a
toTree _ = undefined

fromTree :: Tree a -> Maybe (BinaryTree a)
fromTree Leaf = Just BLeaf
fromTree (Node x []) = Just(BNode x BLeaf BLeaf)
fromTree (Node x [c1]) = 
    case (fromTree c1) of
        Just bt1 -> Just (BNode x bt1 BLeaf)
        Nothing -> Nothing

fromTree (Node x [c1,c2]) = 
    case (fromTree c1, fromTree c2) of
        (Just bt1, Just bt2) -> Just (BNode x bt1 bt2)
        (_,_) -> Nothing

fromTree (Node x _) = Nothing


{- Sorting -}
mergeSort :: (Ord a) => [a] -> [a]
mergeSort _ = undefined

{- Connected Words -}
printStats :: IO ()
printStats = undefined

connectedWords :: IO [String]
connectedWords = undefined

connectionMap :: [(Char, [Char])]
connectionMap =
    [
        ('a', "aqzsw"),
        ('b', "bvghn"),
        ('c', "cxdfv"),
        ('d', "dxserfc"),
        ('e', "ewsdr"),
        ('f', "fdrtgvc"),
        ('g', "gftyhbv"),
        ('h', "hgyujnb"),
        ('i', "iujklo"),
        ('j', "jhuikmn"),
        ('k', "kjiolm"),
        ('l', "lkop"),
        ('m', "mnjk"),
        ('n', "nbhjm"),
        ('o', "oiklp"),
        ('p', "pol"),
        ('q', "qwa"),
        ('r', "redft"),
        ('s', "sawedxz"),
        ('t', "trfgy"),
        ('u', "uyhji"),
        ('v', "vcfgb"),
        ('w', "wqase"),
        ('x', "xzsdc"),
        ('y', "ytghu"),
        ('z', "zasx")
    ]

isConnected :: String -> Bool
isConnected _ = undefined
