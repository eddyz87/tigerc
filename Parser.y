{
module Parser where
import Tokens
}

%name parse
%tokentype { TokenKind }

%token int { TInt $$ }
-- %token '+' { Plus }
%token 'type' { Type }
%token id { Id $$ }
%token '{' { Lbrace }
%token '}' { Rbrace }
%token '(' { Lparen }
%token ')' { Rparen }
%token ',' { Comma }
%token ':' { Colon }
%token 'array' { Array }
%token 'of' { Of }
%token '=' { Eq }
%token 'var' { Var }
%token ':=' { Assign }
%token 'function' { Function }

%%

decs: decs dec                 { }
    | {- empty -}              { }
    ;

dec: tydec                     {}
   | vardec                    {}
   | fundec                    {}

tydec: 'type' id '=' ty        {}

ty: id                         {}
  | '{' tyfields '}'           {}
  | 'array' 'of' id            {}

tyfields: tyfields ',' tyfield   {}
        |                        {}

tyfield: id ':' id               {}

vardec: 'var' id ':=' exp         {}
      | 'var' id ':' id ':=' exp  {}            

fundec: 'function' id '(' tyfields ')' '=' exp        {}
      | 'function' id '(' tyfields ')' ':' id '=' exp {}
      
exp : int                        { }
--     | int '+' int                { }
--     ;

{

happyError :: [TokenKind] -> a
happyError _ = error "Parse error"

}
