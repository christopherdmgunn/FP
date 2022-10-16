speller :: [String] -> String
speller [] = ""
speller [x] = "and " ++ spellerHelper x 
speller (x:xs) = spellerHelper x ++ ", " ++ speller xs

spellerHelper :: String -> String
spellerHelper x =
     [head x] ++ " is for " ++  x                                        