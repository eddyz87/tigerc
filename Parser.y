{
module Parser where
import Tokens
}

%name parse
%tokentype { TokenKind }

%token int         { TInt $$ }
%token string      { TString $$ }
%token id          { Id $$ }

%token 'type'      { Type }
%token 'var'       { Var }  
%token 'function'  { Function }
%token 'break'     { Break }
%token 'of'        { Of }
%token 'end'       { End }
%token 'in'        { In }
%token 'nil'       { Nil }
%token 'let'       { Let }
%token 'do'        { Do }
%token 'to'        { To }
%token 'for'       { For }
%token 'while'     { While }
%token 'else'      { Else }
%token 'then'      { Then }
%token 'if'        { If }
%token 'array'     { Array }
%token ':='        { Assign }
%token '|'         { Or }
%token '&'         { And }
%token '>='        { Ge }
%token '>'         { Gt }
%token '<='        { Le }
%token '<'         { Lt }
%token '<>'        { Neq }
%token '='         { Eq }
%token '/'         { Divide }
%token '*'         { Times }
%token '-'         { Minus }
%token '+'         { Plus }
%token '.'         { Dot }
%token '}'         { Rbrace }
%token '{'         { Lbrace }
%token ']'         { Rbrack }
%token '['         { Lbrack }
%token ')'         { Rparen }
%token '('         { Lparen }
%token ';'         { Semicolon }
%token ':'         { Colon }
%token ','         { Comma }

%%

decs : decs dec                 {}
     |                          {}
     ;

dec : tydec                     {}
    | vardec                    {}
    | fundec                    {}

tydec : 'type' id '=' ty        {}

ty : id                         {}
   | '{' tyfields '}'           {}
   | 'array' 'of' id            {}

tyfields: tyfields ',' tyfield  {}
        |                       {}

tyfield: id ':' id              {}

vardec: 'var' id ':=' exp         {}
      | 'var' id ':' id ':=' exp  {}            

fundec: 'function' id '(' tyfields ')' '=' exp        {}
      | 'function' id '(' tyfields ')' ':' id '=' exp {}
      
exp : lvalue               {}
    | 'nil'                {}
    | '(' exps ')'         {}
    | int                  {}
    | string               {}
    | id '(' args ')'      {}
    | '-' exp              {}
    | exp '+' exp          {}
    | exp '-' exp          {}
    | exp '*' exp          {}
    | exp '/' exp          {}
    | exp '=' exp          {}
    | exp '>' exp          {}
    | exp '<' exp          {}
    | exp '<>' exp         {}
    | exp '>=' exp         {}
    | exp '<=' exp         {}
    | exp '|' exp          {}
    | exp '&' exp          {}
    | id '{' inits '}'                     {}
    | id '[' exp ']' 'of' exp              {}
    | lvalue ':=' exp                      {}
    | 'if' exp 'then' exp 'else' exp       {}
    | 'if' exp 'then' exp                  {}
    | 'while' exp 'do' exp                 {}
    | 'for' id ':=' exp 'to' exp 'do' exp  {}
    | 'break'                              {}
    | 'let' decs 'in' exps 'end'           {}

exps : exps ';' exp        {}
     |                     {}
    
args : args ',' exp        {}
     |                     {}

inits : inits ',' init     {}
      |                    {}

init : id '=' exp          {}
      
lvalue: id                                {}
      | lvalue '.' id                     {}
      | lvalue '[' exp ']'                {}
    
    
{

happyError :: [TokenKind] -> a
happyError _ = error "Parse error"

}
