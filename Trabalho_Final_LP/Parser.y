{ 
module Parser where 

import Lexer
}

%name parser 
%tokentype { Token }
%error { parseError }

%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenAdd }
    "&&"            { TokenAnd }
    if              { TokenIf }
    then            { TokenThen }
    else            { TokenElse }
    var             { TokenVar $$ }
    '\\'            { TokenLam }
    ':'             { TokenColon }
    "->"            { TokenArrow }
    Number          { TokenTNum }
    Boolean         { TokenTBool }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    '-'            { TokenSubtract }
    '*'            { TokenMul }
    '/'             { TokenDiv }
    Not             { TokenNot }
    '='            { TokenEqual }
    '<'            { TokenLessThan }
    '['            { TokenBracketL }
    ']'            { TokenBracketR }
    ','            { TokenComma }
    Let             { TokenLet }
    In              { TokenIn }
    '='             { TokenAssign }
    While_State     { TokenWhileState }
    With            { TokenWith }
    Do             { TokenDo }
    cons            { TokenCons }
    nil             { TokenNil }
    head            { TokenHead }
    tail            { TokenTail }
    isEmpty         { TokenIsEmpty }


%nonassoc if then else 
%nonassoc '\\' 
%right '='
%left '=' '<'
%left '+'  '-'
%left '*' 
%left '/'
%right 'not'
%left "&&"
%left App

%start Exp
%% 

Exp     : num                           { Num $1 }
        | true                          { BTrue }
        | false                         { BFalse }
        | Exp '+' Exp                   { Add $1 $3 }
        | Exp '-' Exp                   { Subtract $1 $3 }
        | Exp '*' Exp                   { Mul $1 $3 }
        | Exp : Exp '/' Exp             { Div $1 $3 }
        | Not Exp                       { Not $2 }
        | Exp '=' Exp                   { Equal $1 $3 }
        | Exp '<' Exp                   { LessThan $1 $3 }
        | Exp "&&" Exp                  { And $1 $3 }
        | if Exp then Exp else Exp      { If $2 $4 $6 }
        | var                           { Var $1 }
        | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
        | Exp Exp                       { App $1 $2 }
        | '(' Exp ')'                   { Paren $2 }
        | '[' ListElements ']'          { List $2 }
        | '[' ']'                       { List [] }
        | Let var '=' Exp In Exp        { Let $2 $4 $6 }
        | While_State Expr With Expr DO Expr { WhileState $2 $4 $6 }
        | Exp '[' Exp ']'               { Index $1 $3 }   
        | Exp '[' Exp']' Assign Exp     { Assign $1 $3 $5 }
        | cons Exp Exp                   { Cons $2 $3 }
        | nil                            { Nil }
        | head Exp                       { Head $2 }
        | tail Exp                       { Tail $2 }
        | isEmpty Exp                    { IsEmpty $2 }


ListElements : Exp                      { [$1] }
              | ListElements ',' Exp      { $1 ++ [$3] }

Type    : Boolean                       { TBool }
        | Number                        { TNum }
        | '(' Type "->" Type ')'        { TFun $2 $4 }
        | '[' Type ']'                  { TList $2 }

{ 
parseError :: [Token] -> a 
parseError _ = error "Erro sintático!"
}