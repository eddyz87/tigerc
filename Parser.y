{
module Parser where
import qualified Tokens as T
import Tokens (tokenPos)
import Tokens (tokenId)
import Tokens (Pos(NoPos))
import qualified Data.List as L
import Ast
}

%name parse

%monad { Either String }

%tokentype { T.Token }

%token int         { T.Token _ (T.TInt $$) }
%token string      { T.Token _ (T.TString $$) }
%token id          { T.Token _ (T.Id _) }

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

decs : nonEmptyList(dec)        { groupDecs $1 }

dec :: { Dec }
dec : tydec                     { TypeDec [$1] }
    | vardec                    { VariableDec $1 }
    | fundec                    { FunctionDec [$1] }

tydec :: { (Id, Ty) }
tydec : 'type' id '=' ty        { (tokenId $2, $4) }

ty :: { Ty }
ty : id                         { NameTy $ tokenId $1 }
   | '{' tyfields '}'           { RecordTy $2 }
   | 'array' 'of' id            { ArrayTy $ tokenId $3 }
   
tyfields :: { [Field] }
tyfields : list(tyfield, ',')  { $1 }

tyfield :: { Field }         
tyfield : id ':' id                { Field (tokenId $1) (tokenId $3) }

vardec :: { VarDec }
vardec : 'var' id ':=' exp         { VarDec (tokenId $2) Nothing $4 }
       | 'var' id ':' id ':=' exp  { VarDec (tokenId $2) (Just (tokenId $4)) $6 }

fundec :: { FunDec }
fundec : 'function' id '(' tyfields ')' '=' exp
         { FunDec (tokenId $2) $4 Nothing $7 }
       | 'function' id '(' tyfields ')' ':' id '=' exp
         { FunDec (tokenId $2) $4 (Just $ tokenId $7) $9 }

exp :: { Exp }
exp : lvalue               { VarExp $1 }
    | 'nil'                { NilExp }
    | '(' exps ')'         { SeqExp $2 }
    | int                  { IntExp $1 }
    | string               { StringExp $1 }
    | id '(' args ')'      { CallExp (tokenId $1) $3 }
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
    | id '{' inits '}'                     { RecordExp $3 (tokenId $1) }
    | id '[' exp ']' 'of' exp              { ArrayExp (tokenId $1) $3 $6 }
    | lvalue ':=' exp                      { AssignExp $1 $3 }
    | 'if' exp 'then' exp 'else' exp       { IfExp $2 $4 (Just $6) }
    | 'if' exp 'then' exp                  { IfExp $2 $4 Nothing }
    | 'while' exp 'do' exp                 { WhileExp $2 $4 }
    | 'for' id ':=' exp 'to' exp 'do' exp  { ForExp (tokenId $2) $4 $6 $8 }
    | 'break'                              { BreakExp }
    | 'let' decs 'in' exps 'end'           { LetExp $2 (SeqExp $4) }

exps :: { [Exp] }
exps : list(exp, ';')  { $1 }

args :: { [Exp] }
args : list(exp, ',')  { $1 }

inits :: { [(Id, Exp)] }
inits : list(init, ',')  { $1 }

init :: { (Id, Exp) }
init : id '=' exp          { (tokenId $1, $3) }

lvalue :: { Var }
lvalue : id                         { SimpleVar (tokenId $1) (tokenPos $1) }
       | id '[' exp ']'             { SubscriptVar (SimpleVar (tokenId $1) (tokenPos $1)) $3 (tokenPos $2) }
       | lvalue '.' id              { FieldVar $1 (tokenId $3) (tokenPos $2) }
       | lvalue '.' id '[' exp ']'
         { SubscriptVar (FieldVar $1 (tokenId $3) (tokenPos $2)) $5 (tokenPos $4) }

list(kind, sep) : rlist(kind, sep)  { reverse $1 }
   
rlist(kind, sep) : rlist(kind, sep) sep kind    { $3 : $1 }
                 | kind                         { [$1] }
                 |                              { [] }
              
list1(kind) : rlist1(kind)  { reverse $1 }
   
rlist1(kind) : rlist1(kind) kind    { $2 : $1 }
             | kind                 { [$1] }
             |                      { [] }

nonEmptyList(kind)    : revNonEmptyList(kind)         { reverse $1 }
revNonEmptyList(kind) : revNonEmptyList(kind) kind    { $2 : $1 }
                      | kind                          { [$1] }
              
{

happyError :: [T.Token] -> Either String a
happyError lookahead = Left $ "Parser error at token " ++
                              (show $ take 1 lookahead)

groupDecs :: [Dec] -> [Dec]
groupDecs decs = glue $ L.groupBy eq decs
  where
    eq (FunctionDec _) (FunctionDec _) = True
    eq (TypeDec _) (TypeDec _) = True
    eq _ _ = False
    glue :: [[Dec]] -> [Dec]
    glue [] = []
    glue ([]:ys) = glue ys
    glue ((x:xs):ys) = case x of
      VariableDec _ -> x : (glue $ xs:ys)
      FunctionDec _ -> (FunctionDec funs):(glue ys)
        where funs = L.concat $ map functionDecs (x:xs)
      TypeDec _ -> (TypeDec types):(glue ys)
        where types = L.concat $ map typeDecs (x:xs)

}
