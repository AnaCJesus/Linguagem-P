module Interpreter where 

import Lexer 

isValue :: Expr -> Bool 
isValue BTrue       = True 
isValue BFalse      = True  
isValue (Num _)     = True 
isValue (Lam _ _ _) = True
isValue (List es)   = all isValue es
isValue Nil         = True
isValue (Cons h t)  = isValue h && isValue t
isValue _           = False




replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0    = newVal : xs
    | otherwise = x : replaceNth (n - 1) newVal xs

subst :: String -> Expr -> Expr -> Expr
subst v e BTrue = BTrue 
subst v e BFalse = BFalse 
subst v e (Num x) = Num x 
subst v e (Add e1 e2) = Add (subst v e e1) (subst v e e2)
subst v e (And e1 e2) = And (subst v e e1) (subst v e e2)
subst v e (If e1 e2 e3) = If (subst v e e1) (subst v e e2) (subst v e e3)
subst v e (Var x) = if v == x then 
                      e 
                    else 
                      Var x 
subst v e (Lam x t b) = if v == x then Lam x t b else Lam x t (subst v e b)
subst v e (App e1 e2) = App (subst v e e1) (subst v e e2)
subst v e (Paren e1) = Paren (subst v e e1)
subst v e (Subtract e1 e2) = Subtract (subst v e e1) (subst v e e2)
subst v e (Mul e1 e2) = Mul (subst v e e1) (subst v e e2)
subst v e (Not e1) = Not (subst v e e1)
subst v e (Equal e1 e2) = Equal (subst v e e1) (subst v e e2)
subst v e (LessThan e1 e2) = LessThan (subst v e e1) (subst v e e2)
subst v e (List es) = List (map (subst v e) es)
subst v e (Let x val body) = if v == x then Let x (subst v e val) body else Let x (subst v e val) (subst v e body)
subst v e (Index list_expr index_expr) = Index (subst v e list_expr) (subst v e index_expr)
subst v e (Assign list_expr index_expr value_expr) = Assign (subst v e list_expr) (subst v e index_expr) (subst v e value_expr)
subst v e Nil = Nil
subst v e (Cons h t) = Cons (subst v e h) (subst v e t)
subst v e (Head ex) = Head (subst v e ex)
subst v e (Tail ex) = Tail (subst v e ex)
subst v e (IsEmpty ex) = IsEmpty (subst v e ex)



step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2) 
step (Add (List es1) (List es2))
    | length es1 == length es2 = List (zipWith Add es1 es2 )
    | otherwise = error "Erro em Add: listas com tamanhos diferentes!" 
step (Add (Num n) (List es)) = List (map (Add (Num n)) es)
step (Add (List es) (Num n)) = List (map (Add (Num n)) es)    
step (Add (Num n1) e2) = let e2' = step e2       
                           in Add (Num n1) e2' 
step (Add e1 e2) = Add (step e1) e2              
step (Subtract (Num n1) (Num n2)) = Num (n1 - n2) 
step (Subtract (List es1) (List es2))
    | length es1 == length es2 = List (zipWith Subtract es1 es2)
    | otherwise = error "Erro em Subtract: listas com tamanhos diferentes!"
step (Subtract (Num n) (List es)) = List (map (Subtract (Num n)) es)
step (Subtract (List es) (Num n)) = List (map (Subtract (Num n)) es)
step (Subtract (Num n1) e2) = let e2' = step e2   
                               in Subtract (Num n1) e2'
step (Subtract e1 e2) = Subtract (step e1) e2    
step (Mul (Num n1) (Num n2)) = Num (n1 * n2)  
step (Mul (List es1) (List es2))
    | length es1 == length es2 = List (zipWith Mul es1 es2)
    | otherwise = error "Erro em Mul: listas com tamanhos diferentes!"
step (Mul (Num n) (List es)) = List (map (Mul (Num n)) es) 
step (Mul (List es) (Num n)) = List (map (Mul (Num n)) es)  
step (Mul (Num n1) e2) = let e2' = step e2       
                         in Mul (Num n1) e2'
step (Mul e1 e2) = Mul (step e1) e2 
step (Div (Num n1) (Num n2))
    | n2 == 0 = error "Erro: Divisão por zero!"
    | otherwise = Num (n1 div n2)
step (Div e1 e2)
    | not (isValue e1) = Div (step e1) e2
    | not (isValue e2) = Div e1 (step e2)
    | otherwise = error ("Erro em Div: argumentos não numéricos: " ++ show e1 ++ " / " ++ show e2)

            
step (And BTrue e2) = e2 
step (And BFalse e2) = BFalse 
step (And e1 e2) = And (step e1) e2 
step (Not BTrue) = BFalse
step (Not BFalse) = BTrue
step (Not e) = Not (step e)
step (Equal (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse
step (Equal (Num n1) e2) = let e2' = step e2
                             in Equal (Num n1) e2'
step (Equal e1 e2) = Equal (step e1) e2
step (LessThan (Num n1) (Num n2)) = if n1 < n2 then BTrue else BFalse
step (LessThan (Num n1) e2) = let e2' = step e2
                                 in LessThan (Num n1) e2'
step (LessThan e1 e2) = LessThan (step e1) e2
step (If BTrue e1 e2) = e1 
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2 
step (App e1@(Lam x t b) e2) | isValue e2 = subst x e2 b
                             | otherwise  = App e1 (step e2)
step (App e1 e2) = App (step e1) e2 
step (Paren e) = e 
step (List es) | not (all isValue es) =
                    let (prefix,nonValue:suffix) = break (not . isValue) es
                        evaluatedNonValue = step nonValue
                   in List (prefix ++ (evaluatedNonValue : suffix))
              | otherwise = error ("Não há regra de passo para esta expressão. (List: " ++ show (List es) ++ ")") 
step (Let x val body) | isValue val = subst x val body
                      | otherwise = Let x (step val) body
step (Index (List es) (Num n))
    | n >= 0 && n < length es = es !! n
    | otherwise = error "Erro em Index: índice fora dos limites da lista!"
step (Index list_expr index_expr)
    | not (isValue list_expr) = Index (step list_expr) index_expr
    | not (isValue index_expr) = Index list_expr (step index_expr)
    | otherwise = error ("Erro em Index: " ++ show list_expr ++ "[" ++ show index_expr ++ "] não pode ser avaliado.")
step (Assign (List es) (Num n) new_val)
    | isValue new_val && n >= 0 && n < length es = List (replaceNth n new_val es) 
    | not (isValue new_val) = Assign (List es) (Num n) (step new_val)
    | otherwise = error "Erro em Assign: índice fora dos limites da lista ou novo valor não avaliado!"
step (Assign list_expr index_expr value_expr)
    | not (isValue list_expr) = Assign (step list_expr) index_expr value_expr
    | not (isValue index_expr) = Assign list_expr (step index_expr) value_expr
    | otherwise = error ("Erro em Assign: " ++ show list_expr ++ "[" ++ show index_expr ++ "] = " ++ show value_expr ++ " não pode ser avaliado.")

step (WhileState current_state cond_func body_func)
    | not (isValue current_state) = WhileState (step current_state) cond_func body_func
    | isValue current_state =
        let condition_result = eval (App cond_func current_state)
        in case condition_result of
            BTrue  ->
                let next_state_expr = App body_func current_state
                    next_state = eval next_state_expr 
                in WhileState next_state cond_func body_func 
            BFalse -> current_state 
            _      -> error "Erro em WhileState: condição não é booleana após avaliação!"
step (Cons h t)
    | not (isValue h) = Cons (step h) t
    | not (isValue t) = Cons h (step t)
    | otherwise       = Cons h t

step (Head Nil) = error "Erro em Head: lista vazia"
step (Head (Cons h _)) = h
step (Head e)
    | not (isValue e) = Head (step e)
    | otherwise = error ("Erro em Head: expressão inválida: " ++ show e)
step (Tail Nil) = error "Erro em Tail: lista vazia"
step (Tail (Cons _ t)) = t
step (Tail e)
    | not (isVaslue e) = Tail (step e)
    | otherwise = error ("Erro em Tail: expressão inválida: " ++ show e)

step (IsEmpty Nil) = BTrue
step (IsEmpty (Cons _ _)) = BFalse
step (IsEmpty e)
    | not (isValue e) = IsEmpty (step e)
    | otherwise = error ("Erro em IsEmpty: expressão inválida: " ++ show e)

step e = error ("Não há regra de passo para esta expressão: " ++ show e)
eval :: Expr -> Expr 
eval e | isValue e = e 
       | otherwise = eval (step e)