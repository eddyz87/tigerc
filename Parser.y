{
module Parser where
import qualified Tokens as T
import Ast
}

%name parse

%monad { Either String }

%tokentype { T.Token }

%token int         { T.Token _ (T.TInt $$) }
%token string      { T.Token _ (T.TString $$) }
%token id          { T.Token _ (T.Id $$) }

%token 'type'      { T.Token _ T.Type }
%token 'var'       { T.Token _ T.Var }  
%token 'function'  { T.Token _ T.Function }
%token 'break'     { T.Token _ T.Break }
%token 'of'        { T.Token _ T.Of }
%token 'end'       { T.Token _ T.End }
%token 'in'        { T.Token _ T.In }
%token 'nil'       { T.Token _ T.Nil }
%token 'let'       { T.Token _ T.Let }
%token 'do'        { T.Token _ T.Do }
%token 'to'        { T.Token _ T.To }
%token 'for'       { T.Token _ T.For }
%token 'while'     { T.Token _ T.While }
%token 'else'      { T.Token _ T.Else }
%token 'then'      { T.Token _ T.Then }
%token 'if'        { T.Token _ T.If }
%token 'array'     { T.Token _ T.Array }
%token ':='        { T.Token _ T.Assign }
%token '|'         { T.Token _ T.Or }
%token '&'         { T.Token _ T.And }
%token '>='        { T.Token _ T.Ge }
%token '>'         { T.Token _ T.Gt }
%token '<='        { T.Token _ T.Le }
%token '<'         { T.Token _ T.Lt }
%token '<>'        { T.Token _ T.Neq }
%token '='         { T.Token _ T.Eq }
%token '/'         { T.Token _ T.Divide }
%token '*'         { T.Token _ T.Times }
%token '-'         { T.Token _ T.Minus }
%token '+'         { T.Token _ T.Plus }
%token '.'         { T.Token _ T.Dot }
%token '}'         { T.Token _ T.Rbrace }
%token '{'         { T.Token _ T.Lbrace }
%token ']'         { T.Token _ T.Rbrack }
%token '['         { T.Token _ T.Lbrack }
%token ')'         { T.Token _ T.Rparen }
%token '('         { T.Token _ T.Lparen }
%token ';'         { T.Token _ T.Semicolon }
%token ':'         { T.Token _ T.Colon }
%token ','         { T.Token _ T.Comma }

%nonassoc ':='
%nonassoc 'do'
%right 'then' 'else'
%nonassoc 'of'
%left '|' '&'
%nonassoc '=' '>' '<' '>=' '<=' '<>'
%left '+' '-'
%left '*' '/'

%%

program :: { Exp }
program : exp                   { $1 }

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
tyfields : list(tyfield, ',')  { $1 }

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
    | id '[' exp ']'       { VarExp $ SubscriptVar (SimpleVar $1) $3 }
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
exps : list(exp, ';')  { $1 }

args :: { [Exp] }
args : list(exp, ',')  { $1 }

inits :: { [(Id, Exp)] }
inits : list(init, ',')  { $1 }

init :: { (Id, Exp) }
init : id '=' exp          { ($1, $3) }

lvalue :: { Var }
lvalue : id                { SimpleVar $1 }
       | id '[' exp ']'    { SubscriptVar (SimpleVar $1) $3 }
       | lvalue_not_id     { $1 }

lvalue_not_id :: { Var }       
lvalue_not_id : lvalue '.' id             { FieldVar $1 $3 }
              | lvalue_not_id '[' exp ']' { SubscriptVar $1 $3 }

list(kind, sep) : rlist(kind, sep)  { reverse $1 }
   
rlist(kind, sep) : rlist(kind, sep) sep kind    { $3 : $1 }
                 | kind                         { [$1] }
                 |                              { [] }
              
{

happyError :: [T.Token] -> Either String a
happyError lookahead = Left $ "Parser error at token " ++
                              (show $ take 1 lookahead)

}
