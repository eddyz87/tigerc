{
module Parser where
import Tokens
}

%name parse
%tokentype { TokenKind }

%token int { TInt $$ }
%token '+' { Plus }

%%

exp : int                        { }
    | int '+' int                { }
    ;

{

happyError :: [TokenKind] -> a
happyError _ = error "Parse error"

}
