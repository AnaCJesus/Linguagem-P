module TypeChecker where 

import Lexer 

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty 
typeof ctx (Num _) = Just TNum 
typeof ctx BFalse = Just TBool 
typeof ctx BTrue = Just TBool 
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TNum, Just TNum) -> Just TNum 
                       (Just (TList TNum), Just (TList TNum)) -> Just (TList TNum)
                       (Just TNum, Just (TList TNum))  -> Just (TList TNum)
                       (Just (TList TNum), Just TNum)  -> Just (TList TNum) 
                       _                                -> Nothing
typeof ctx (Subtract e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TNum, Just TNum) -> Just TNum
                       (Just (TList TNum), Just (TList TNum)) -> Just (TList TNum)
                       (Just TNum, Just (TList TNum))  -> Just (TList TNum)
                       (Just (TList TNum), Just TNum)  -> Just (TList TNum)
                       _                               -> Nothing
typeof ctx (Mul e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TNum, Just TNum) -> Just TNum
                       (Just (TList TNum), Just (TList TNum)) -> Just (TList TNum)
                       (Just TNum, Just (TList TNum))  -> Just (TList TNum) 
                       (Just (TList TNum), Just TNum)  -> Just (TList TNum)
                       _                               -> Nothing
typeof ctx (Div e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                           (Just TNum, Just TNum) -> Just TNum
                           _                      -> Nothing
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                       (Just TBool, Just TBool) -> Just TBool 
                       _                        -> Nothing 
typeof ctx (Not e) = case typeof ctx e of  
                       Just TBool -> Just TBool
                       _          -> Nothing
typeof ctx (Equal e1 e2) = 
    case (typeof ctx e1, typeof ctx e2) of 
      (Just t1, Just t2) | t1 == t2 -> Just TBool 
                         | otherwise -> Nothing 
      _ -> Nothing
typeof ctx (LessThan e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                            (Just TNum, Just TNum) -> Just TBool
                            _                      -> Nothing
typeof ctx (If e1 e2 e3) = 
    case (typeof ctx e1) of 
      Just TBool -> case (typeof ctx e2, typeof ctx e3) of 
                      (Just t1, Just t2) | t1 == t2  -> Just t1  
                                         | otherwise -> Nothing 
                      _ -> Nothing 
      _ -> Nothing
typeof ctx (Var v) = lookup v ctx 
typeof ctx (Lam x t1 b) = let ctx' = (x, t1) : ctx in 
                            case typeof ctx' b of 
                              Just t2 -> Just (TFun t1 t2)
                              _       -> Nothing 
typeof ctx (App e1 e2) = 
  case (typeof ctx e1) of 
    Just (TFun t11 t12) -> case typeof ctx e2 of 
                             Just t2 -> if t11 == t2 then 
                                          Just t12 
                                        else 
                                          Nothing 
                             _ -> Nothing 
    _ -> Nothing 
typeof ctx (Paren e) = typeof ctx e 
typeof ctx (List []) = Nothing
typeof ctx (List (e:es)) = case typeof ctx e of
                              Just t -> if all (\x -> typeof ctx x == Just t) es then
                                           Just (TList t)
                                         else Nothing
                              _      -> Nothing
typeof ctx (Let x val body) =
    case typeof ctx val of
        Just t_val -> let ctx' = (x, t_val) : ctx
                      in typeof ctx' body
        _ -> Nothing
typeof ctx (Index list_expr index_expr) =
    case (typeof ctx list_expr, typeof ctx index_expr) of
        (Just (TList t), Just TNum) -> Just t
        _                           -> Nothing
typeof ctx (Assign list_expr index_expr value_expr) =
    case (typeof ctx list_expr, typeof ctx index_expr, typeof ctx value_expr) of
        (Just (TList t_list), Just TNum, Just t_val) | t_list == t_val -> Just (TList t_list)
        _                                                              -> Nothing
typeof ctx (WhileState initial_state cond_func body_func) =
    case (typeof ctx initial_state, typeof ctx cond_func, typeof ctx body_func) of
        (Just t_initial, Just (TFun t_cond_in TBool), Just (TFun t_body_in t_body_out))
            | t_initial == t_cond_in && t_initial == t_body_in && t_initial == t_body_out
                -> Just t_initial
        _ -> Nothing                
                             

typecheck :: Expr -> Expr  
typecheck e = case typeof [] e of 
                Just _ -> e 
                _      -> error "Erro de tipo!"

