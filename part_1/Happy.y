{
module Happy where
import Alex
}

%name                       parseTreeInt         RadiceInt
%name                       parseTreeDouble      RadiceDouble
%tokentype                  { Token }
%error                      { parseError }

%token 
    '{'                     { TokenStartChildren }
    '}'                     { TokenEndChildren }
    int                     { TokenInt $$ }
    double                  { TokenDouble $$ }

%%

-- INIZIO ALBERI INTERI
RadiceInt       : int                               { Node ($1,0) [] }
                | int '{' FigliInt '}'              { Node ($1, nodeCalculateHeight $3 ) $3 }

FigliInt        : RadiceInt                         { [$1] }
                | RadiceInt FigliInt                { $1:$2 }
-- FINE ALBERI INTERI

-- INIZIO ALBERI DOUBLE
RadiceDouble    : double                            { Node ($1,0) [] }
                | double '{' FigliDouble '}'        { Node ($1, nodeCalculateHeight $3 ) $3 }

FigliDouble     : RadiceDouble                      { [$1] }
                | RadiceDouble FigliDouble          { $1:$2 }
-- FINE ALBERI DOUBLE

{
parseError :: [Token] -> a
parseError _ = error "Errore durante il parsing"

nodeCalculateHeight xs = (+) 1 $ maximum $ map nodeHeight xs where
    nodeHeight (Node (v,h) _) = h

data Tree a = Node (a, Int) [Tree a] deriving Show
}