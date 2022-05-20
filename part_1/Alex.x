{
module Alex where
}

%wrapper "basic"

$digit = 0-9
@int = (\-)?$digit+
@double = @int(\.$digit+)?

rules :-
    $white+ ;
    "{"         { \_ -> TokenStartChildren}
    "}"         { \_ -> TokenEndChildren}
    @int        { \s -> TokenInt (read s) }
    @double     { \s -> TokenDouble (read s) }

{
data Token =
    TokenStartChildren  |
    TokenEndChildren    |
    TokenInt Int        |
    TokenDouble Double
    deriving (Eq, Show)
}