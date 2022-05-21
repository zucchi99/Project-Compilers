module Main where
import Alex
import Happy

-- NB per testare in ghci includere al path la sudirectory __build
-- ghci -i./__build demo.hs

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

parse_trees_int    trees_int    = map ( \ t -> parseTreeInt    $ alexScanTokens t) trees_int
parse_trees_double trees_double = map ( \ t -> parseTreeDouble $ alexScanTokens t) trees_double

are_trees_AVL trees = map isAlmostBalanced trees

--do nothing
print_list_couple []     = do
    return ()

--print a list of couples
print_list_couple ((x,(y,z)):xs) = do
    putStrLn $ "original input:"
    putStrLn $ " -> " ++ (show x)
    putStrLn $ "parsed tree:"
    putStrLn $ " -> " ++ (show y)
    putStrLn $ "is AVL?" 
    putStrLn $ " -> " ++ (show z) ++ "\n"
    print_list_couple xs

--test own inputs
test parse_fun trees = 
    let parsed_trees = (parse_fun trees)
    in  print_list_couple $ zip trees $ zip parsed_trees (are_trees_AVL parsed_trees)

test_int trees    = test parse_trees_int trees

test_double trees = test parse_trees_double trees

--main
main = do
    putStrLn ""
    test_int    trees_int
    test_double trees_double
