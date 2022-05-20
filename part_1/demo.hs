module Main where
import Alex
import Happy

-- Si scriva una funzione toStringTree che dato un albero si ottenga una stringa che lo rappresenti
toStringTree :: (Eq a, Show a) => Tree a -> String
toStringTree (Node (v,h) []) = "Nodo: v: " ++ show v ++ " h: " ++ show h
toStringTree (Node (v,h) xs) = "Nodo: v: " ++ show v ++ " h: " ++ show h ++ " con Figli -> (" ++ (\(x:xs) -> foldl (\acc x -> acc ++ "; " ++ x) x xs) (map toStringTree xs) ++ ")"

-- Si scriva una funzione toStringTree che dato un albero si ottenga una stringa che lo rappresenti
toStringTreeBasic :: (Eq a, Show a) => Tree a -> String
toStringTreeBasic (Node (v,h) []) = "v: " ++ show v ++ ", h: " ++ show h ++ ""
toStringTreeBasic (Node (v,h) xs) = "v: " ++ show v ++ ", h: " ++ show h ++ " {" ++ (\(x:xs) -> foldl (\acc x -> acc ++ "; " ++ x) x xs) (map toStringTreeBasic xs) ++ "}"

-- si scriva un predicato isAlmostBalanced che, preso un albero, determina se ha la seguente proprietà: per ogni nodo le altezze di tutti i figli differiscono al massimo di 1.
isAlmostBalanced :: (Show a) => Tree a -> Bool
isAlmostBalanced (Node (v,h) xs) = ((h - minHeight) < 3) && and (map isAlmostBalanced xs) where
    minHeight = minAux $ map (\(Node (v,h) _) -> h) xs where
        minAux [] = 0
        minAux xs = minimum xs 

trees_int = [ "1"                                        -- Bilanciato
            , "5{ 1 {2 3{ 4}} 6 {7 }8}"                  -- Non bilanciato
            , "5{ 1 {2 3{ 4}} 6 {7 }8{1}}"               -- Bilanciato
            , "21{41{65{  4 1 3 {61{ 7 5 6} 53} 7 }} 8}" -- Non bilanciato
            , "1{ 2  {4 5 } 3 {6  7}}" ]                 -- Bilanciato

trees_double =  [ "1.0"                                                                             -- Bilanciato
                , "5.0{ 1.0 {2.0 3.0{ 4.0}} 6.0 {7.0 }8.0}"                                         -- Non bilanciato
                , "5.0{ 1.0 {2.0 3.0{ 4.0}} 6.0 {7.0 }8.0{1.0}}"                                    -- Bilanciato
                , "21.43{41.89{60.63{  4.62 1.12 3.41 {62.71{ 7.11 5.44 6.14} 53.16} 7.74 }} 5.91}" -- Non bilanciato
                , "1.876 { 2.456  {4.134 5.968 } 3.91 {6.013  7.987}}" ]                            -- Bilanciato

trees_int_parsed    = map ( \ t -> parseTreeInt    $ alexScanTokens t) trees_int 
trees_double_parsed = map ( \ t -> parseTreeDouble $ alexScanTokens t) trees_double 

trees_int_isAVL     = map isAlmostBalanced trees_int_parsed
trees_double_isAVL  = map isAlmostBalanced trees_double_parsed

trees_int_zipped    = zip trees_int_parsed    trees_int_isAVL
trees_double_zipped = zip trees_double_parsed trees_double_isAVL

--do nothing
print_list_couple f []     = do
    return ()

--print a list of couples
print_list_couple f (x:xs) = do
    print $ f (fst x)
    putStrLn $ "   is AVL? -> " ++ (show $ snd x) ++ "\n"
    print_list_couple f xs
    
--do nothing
print_list f []     = do
    return ()

--print a list
print_list f (x:xs) = do
    print $ f x
    print_list f xs

--main
main = do
    putStrLn ""
    --print_list_couple toStringTree trees_int_zipped
    --print_list_couple toStringTree trees_double_zipped
    --print_list_couple toStringTreeBasic trees_int_zipped
    --print_list_couple toStringTreeBasic trees_double_zipped
    print_list_couple id trees_int_zipped
    print_list_couple id trees_double_zipped
