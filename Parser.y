{
module Parser where
import qualified Tokens as T
import Ast
}

%name parse
%tokentype { T.TokenKind }

%token int         { T.TInt $$ }
%token string      { T.TString $$ }
%token id          { T.Id $$ }

%token 'type'      { T.Type }
%token 'var'       { T.Var }  
%token 'function'  { T.Function }
%token 'break'     { T.Break }
%token 'of'        { T.Of }
%token 'end'       { T.End }
%token 'in'        { T.In }
%token 'nil'       { T.Nil }
%token 'let'       { T.Let }
%token 'do'        { T.Do }
%token 'to'        { T.To }
%token 'for'       { T.For }
%token 'while'     { T.While }
%token 'else'      { T.Else }
%token 'then'      { T.Then }
%token 'if'        { T.If }
%token 'array'     { T.Array }
%token ':='        { T.Assign }
%token '|'         { T.Or }
%token '&'         { T.And }
%token '>='        { T.Ge }
%token '>'         { T.Gt }
%token '<='        { T.Le }
%token '<'         { T.Lt }
%token '<>'        { T.Neq }
%token '='         { T.Eq }
%token '/'         { T.Divide }
%token '*'         { T.Times }
%token '-'         { T.Minus }
%token '+'         { T.Plus }
%token '.'         { T.Dot }
%token '}'         { T.Rbrace }
%token '{'         { T.Lbrace }
%token ']'         { T.Rbrack }
%token '['         { T.Lbrack }
%token ')'         { T.Rparen }
%token '('         { T.Lparen }
%token ';'         { T.Semicolon }
%token ':'         { T.Colon }
%token ','         { T.Comma }

%nonassoc ':='
%nonassoc 'do'
%right 'then' 'else'
%nonassoc 'of'
%left '|' '&'
%nonassoc '=' '>' '<' '>=' '<=' '<>'
%left '+' '-'
%left '*' '/'

%%

program :: { [Dec] }
program : decs                  { $1 }

decs :: { [Dec] }
decs : decs dec                 { $2 : $1 }
     |                          { [] }

dec :: { Dec }
dec : tydec                     { $1 }
    | vardec                    { $1 }
    | fundec                    { $1 }

tydec :: { Dec }
tydec : 'type' id '=' ty        { TypeDec [($2, $4)] }

ty :: { Ty }
ty : id                         { NameTy $1 }
   | '{' tyfields '}'           { RecordTy $2 }
   | 'array' 'of' id            { ArrayTy $3 }

tyfields :: { [Field] }   
tyfields : tyfields ',' tyfield    { $3 : $1 }
         |                         { [] }

tyfield :: { Field }         
tyfield : id ':' id                { Field $1 $3 }

vardec :: { Dec }
vardec : 'var' id ':=' exp         { VarDec $2 Nothing $4 }
       | 'var' id ':' id ':=' exp  { VarDec $2 (Just $4) $6 }

fundec :: { Dec }
fundec : 'function' id '(' tyfields ')' '=' exp
         { FunctionDec [(FunDec $2 $4 Nothing $7)] }
       | 'function' id '(' tyfields ')' ':' id '=' exp
         { FunctionDec [(FunDec $2 $4 (Just $7) $9)] }

exp :: { Exp }
exp : lvalue_not_id        { VarExp $1 }
    | id                   { VarExp (SimpleVar $1) }
    | 'nil'                { NilExp }
    | '(' exps ')'         { SeqExp $2 }
    | int                  { IntExp $1 }
    | string               { StringExp $1 }
    | id '(' args ')'      { CallExp $1 $3 }
    | '-' exp              { OpExp Minus (IntExp 0) $2 }
    | exp '+' exp          { OpExp Plus $1 $3 }
    | exp '-' exp          { OpExp Minus $1 $3 }
    | exp '*' exp          { OpExp Mult $1 $3 }
    | exp '/' exp          { OpExp Div $1 $3 }
    | exp '=' exp          { OpExp Eq $1 $3 }
    | exp '>' exp          { OpExp Gt $1 $3 }
    | exp '<' exp          { OpExp Lt $1 $3 }
    | exp '<>' exp         { OpExp Neq $1 $3 }
    | exp '>=' exp         { OpExp Ge $1 $3 }
    | exp '<=' exp         { OpExp Le $1 $3 }
    | exp '|' exp          { OpExp Or $1 $3 }
    | exp '&' exp          { OpExp And $1 $3 }
    | id '{' inits '}'                     { RecordExp $3 $1 }
    | id '[' exp ']' 'of' exp              { ArrayExp $1 $3 $6 }
    | lvalue ':=' exp                      { AssignExp $1 $3 }
    | 'if' exp 'then' exp 'else' exp       { IfExp $2 $4 (Just $6) }
    | 'if' exp 'then' exp                  { IfExp $2 $4 Nothing }
    | 'while' exp 'do' exp                 { WhileExp $2 $4 }
    | 'for' id ':=' exp 'to' exp 'do' exp  { ForExp $2 $4 $6 $8 }
    | 'break'                              { BreakExp }
    | 'let' decs 'in' exps 'end'           { LetExp $2 (SeqExp $4) }

exps :: { [Exp] }    
exps : exps ';' exp        { $3 : $1 }
     |                     { [] }

args :: { [Exp] }
args : args ',' exp        { $3 : $1 }
     |                     { [] }

inits :: { [(Id, Exp)] }
inits : inits ',' init     { $3 : $1 }
      |                    { [] }

init :: { (Id, Exp) }
init : id '=' exp          { ($1, $3) }

lvalue :: { Var }
lvalue : id                { SimpleVar $1 }
       | id '[' exp ']'    { SubscriptVar (SimpleVar $1) $3 }
       | lvalue_not_id     { $1 }

lvalue_not_id :: { Var }       
lvalue_not_id : lvalue '.' id             { FieldVar $1 $3 }
              | lvalue_not_id '[' exp ']' { SubscriptVar $1 $3 }
    
{

happyError :: [T.TokenKind] -> a
happyError _ = error "Parse error"

}
