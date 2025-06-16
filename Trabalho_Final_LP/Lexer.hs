module Lexer where 

import Data.Char

data Expr = BTrue 
          | BFalse 
          | Num Int 
          | Add Expr Expr 
          | And Expr Expr 
          | If Expr Expr Expr 
          | Var String 
          | Lam String Ty Expr 
          | App Expr Expr 
          | Paren Expr 
          | Subtract Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Not Expr
          | Equal Expr Expr
          | LessThan Expr Expr
          | List [Expr]
          | Cons Expr Expr
          | Nil
          | Head Expr
          | Tail Expr
          | IsEmpty Expr

          | Let String Expr Expr
          | Index Expr Expr
          | Assign Expr Expr Expr
          | WhileState Expr Expr Expr
          deriving Show 

data Ty = TBool 
        | TNum 
        | TFun Ty Ty 
        | TList Ty
        | TVoid
        deriving (Show, Eq)

data Token = TokenTrue 
           | TokenFalse 
           | TokenNum Int 
           | TokenAdd 
           | TokenAnd 
           | TokenIf 
           | TokenThen
           | TokenElse 
           | TokenVar String 
           | TokenLam 
           | TokenColon
           | TokenArrow 
           | TokenTNum 
           | TokenTBool
           | TokenLParen 
           | TokenRParen 
           | TokenSubtract
           | TokenMul
           | TokenNot
           | TokenEqual
           | TokenLessThan
           | TokenBracketL
           | TokenBracketR
           | TokenComma 
           | TokenLet 
           | TokenIn
           | TokenAssign
           | TokenWhileState
           | TokenWith
           | TokenDo
           | TokenDiv
           | TokenNil
           | TokenCons
           | TokenHead
           | TokenTail
           | TokenIsEmpty

           deriving Show 

lexer :: String -> [Token]
lexer [] = [] 
lexer ('+':cs) = TokenAdd : lexer cs 
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('-':cs) = TokenSubtract : lexer cs
lexer ('*':cs) = TokenMul : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs 
lexer (':':cs) = TokenColon : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs 
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('=':'=':cs) = TokenEqual : lexer cs
lexer ('=':cs) = TokenAssign : lexer cs
lexer ('<':cs) = TokenLessThan : lexer cs
lexer ('[':cs) = TokenBracketL : lexer cs
lexer (']':cs) = TokenBracketR : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs) 
             | isAlpha c = lexKW (c:cs)

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of 
             ("true", rest) -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest 
             ("if", rest) -> TokenIf : lexer rest 
             ("then", rest) -> TokenThen : lexer rest 
             ("else", rest) -> TokenElse : lexer rest
             ("not", rest) -> TokenNot : lexer rest 
             ("Number", rest) -> TokenTNum : lexer rest 
             ("Boolean", rest) -> TokenTBool : lexer rest
             ("let", rest) -> TokenLet : lexer rest 
             ("in", rest) -> TokenIn : lexer rest
             ("whilestate", rest) -> TokenWhileState : lexer rest
             ("with", rest) -> TokenWith : lexer rest
             ("do", rest) -> TokenDo : lexer rest
             ("nil", rest)       -> TokenNil : lexer rest
             ("cons", rest)      -> TokenCons : lexer rest
             ("head", rest)      -> TokenHead : lexer rest
             ("tail", rest)      -> TokenTail : lexer rest
             ("isEmpty", rest)   -> TokenIsEmpty : lexer rest
             (var, rest) -> TokenVar var : lexer rest 