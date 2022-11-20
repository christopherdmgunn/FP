{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import GHC.CmmToAsm.AArch64.Instr (x0)
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use id" #-}
this list  = [y| x<-list, y<-x]

this' x = 'x'
this'' x = "x"


-- iden :: Foldable t => b -> t (b -> b) -> b
-- iden x = ( foldr (\x -> x) x)

-- iden' :: Foldable t => b -> t (b -> b) -> b
-- iden' x = ( foldl (\x -> x ) x )



speller :: [String] -> String
speller [] = ""
speller [x] = "and " ++ spellerHelper x 
speller (x:xs) = spellerHelper x ++ ", " ++ speller xs

spellerHelper :: String -> String
spellerHelper x =
     [head x] ++ " is for " ++  x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead(x:_) = Just x


tally :: (Eq int, Num int, Ord int) => int -> Maybe String
tally -x = Nothing
tally 0 = Just ""  
tally x = Just(tallyHelper' x)

tallyHelper' :: (Eq int, Ord int, Num int) => int -> String
tallyHelper' x = if (x > 5) 
    then (tallyHelper 5) ++ tallyHelper' (x Prelude.- 5)
    else tallyHelper x 

tallyHelper :: (Eq int, Num int) => int -> String
tallyHelper 0 = ""
tallyHelper 1 = "| "
tallyHelper 2 = "|| "
tallyHelper 3 = "||| "
tallyHelper 4 = "|||| "
tallyHelper 5 = "||||/ "
