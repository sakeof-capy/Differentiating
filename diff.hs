{-# OPTIONS_GHC -Wall #-}
module Differentiating where

import Data.Char

type Parser a = String -> Maybe (a, String)

data Expr = Dif Expr Char 
          | Fun String Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Neg Expr
          | Var String
          | Num Double
          deriving Show

diff :: Expr -> String -> Expr
diff (Dif _ _)     _ = error "Diff not processed"
diff (Add ex1 ex2) v = Add (diff ex1 v) (diff ex2 v)
diff (Sub ex1 ex2) v = Sub (diff ex1 v) (diff ex2 v)
diff (Mul ex1 ex2) v = Add (Mul (diff ex1 v) ex2) (Mul ex1 (diff ex2 v))
diff (Div ex1 ex2) v = Div (Sub (Mul (diff ex1 v) ex2) (Mul ex1 (diff ex2 v))) (Pow ex2 (Num 2))
-- diff (Pow ex1 ex2) v = undefined
diff (Neg ex)      v = Neg $ diff ex v
diff (Var c)       v | v == c    = Num 1
                     | otherwise = Num 0
diff (Num _)       _ = Num 0

simpl :: Expr -> Expr
-- simpl (Add (Num n1) (Num n2)) = Num (n1 + n2)
-- simpl (Sub (Num n1) (Num n2)) = Num (n1 - n2)
-- simpl (Mul (Num n1) (Num n2)) = Num (n1 * n2)
-- simpl (Div (Num n1) (Num n2)) = Num (n1 `div` n2) -- Integer division!
-- simpl (Pow (Num n1) (Num n2)) = Num (n1 ^ n2)
-- simpl (Neg (Num n)) = Num (-n)
simpl (Dif e v) = Dif (simpl e) v
simpl (Add e1 e2) = case (simpl e1, simpl e2) of
                      (Num 0, e) -> e
                      (e, Num 0) -> e
                      (Num n1, Num n2) -> Num (n1 + n2)
                      (e1', e2') -> Add e1' e2'
simpl (Sub e1 e2) = case (simpl e1, simpl e2) of
                      (Num 0, e) -> Neg e
                      (e, Num 0) -> e
                      (Num n1, Num n2) -> Num (n1 - n2)
                      (e1', e2') -> Sub e1' e2'
simpl (Mul e1 e2) = case (simpl e1, simpl e2) of
                      (Num 0, _) -> Num 0
                      (_, Num 0) -> Num 0
                      (Num 1, e) -> e
                      (e, Num 1) -> e
                      (Num n1, Num n2) -> Num (n1 * n2)
                      (e1', e2') -> Mul e1' e2'
simpl (Div e1 e2) = case (simpl e1, simpl e2) of
                      (_, Num 0) -> error "Division by zero!"
                      (Num 0, _) -> Num 0
                      (Num n1, Num n2) -> Num (n1 / n2)
                      (e1', e2') -> Div e1' e2'
simpl (Pow e1 e2) = case (simpl e1, simpl e2) of
                      (_, Num 0) -> Num 1
                      (Num 0, _) -> Num 0
                      (Num 1, _) -> Num 1
                      (_, Num 1) -> e1
                      (Num n1, Num n2) -> Num (n1 ** n2)
                      (e1', e2') -> Pow e1' e2'
simpl (Neg e1)    = case simpl e1 of
                      Num n -> Num $ -n
                      e1'   -> Neg e1'
simpl v@(Var _)   = v  
simpl n@(Num _)   = n
--simpl _           = error "***Unsupported expression."

toString :: Expr -> String
toString (Dif ex1 v)   = 'd' : '(' : toString (ex1) ++ ", " ++ [v] ++ ")" 
toString (Fun nam ex2) = nam ++ "(" ++ toString ex2 ++ ")"
toString (Add ex1 ex2) = '(' : toString (ex1) ++ " + " ++ toString(ex2) ++ ")"
toString (Sub ex1 ex2) = '(' : toString (ex1) ++ " - " ++ toString(ex2) ++ ")"
toString (Mul ex1 ex2) = toString (ex1) ++ "*" ++ toString(ex2)
toString (Div ex1 ex2) = toString (ex1) ++ "/" ++ toString(ex2)
toString (Pow ex1 ex2) = toString (ex1) ++ "^" ++ toString(ex2)
toString (Neg ex1)     = '-' : '(' : toString (ex1) ++ ")"
toString (Var v)       = v
toString (Num n)       = show n




--------------
parse :: String -> Maybe Expr
parse s = case expr s of
            Just (e, "") -> Just e
            _ -> Nothing

expr :: Parser Expr
expr s = case term s of
           Just (e1, s') -> rest e1 s'
           Nothing -> Nothing
  where rest e1 ('+':s') = case term s' of
                             Just (e2, s'') -> rest (Add e1 e2) s''
                             Nothing -> Nothing
        rest e1 ('-':s') = case term s' of
                             Just (e2, s'') -> rest (Sub e1 e2) s''
                             Nothing -> Nothing
        rest e s' = Just (e, s')

term :: Parser Expr
term s = case pow s of
           Just (e1, s') -> rest e1 s'
           Nothing -> Nothing
  where rest e1 ('*':s') = case pow s' of
                             Just (e2, s'') -> rest (Mul e1 e2) s''
                             Nothing -> Nothing
        rest e1 ('/':s') = case pow s' of
                             Just (e2, s'') -> rest (Div e1 e2) s''
                             Nothing -> Nothing
        rest e s' = Just (e, s')

pow :: Parser Expr
pow s = case factor s of
          Just (e1, s') -> rest e1 s'
          Nothing -> Nothing
  where rest e1 ('^':s') = case factor s' of
                             Just (e2, s'') -> rest (Pow e1 e2) s''
                             Nothing -> Nothing
        rest e s' = Just (e, s')

factor :: Parser Expr
factor [] = Nothing
factor ('-':s) = case factor s of
                    Just (e, s') -> Just (Neg e, s')
                    Nothing -> Nothing
factor ('(':s) = case expr s of
                    Just (e, ')':s') -> Just (e, s')
                    _ -> Nothing
factor s@(c:_) | isAlpha c = case function s of
                                Just (e, s') -> Just (e, s')
                                Nothing -> identifier s
                | isDigit c = number s
                | otherwise = Nothing

function :: Parser Expr
function s = case identifier s of
               Just (Var name, '(':rest1) -> case expr rest1 of
                                               Just (arg, ')':rest2) -> Just (Fun name arg, rest2)
                                               _ -> Nothing
               _ -> Nothing

identifier :: Parser Expr
identifier s = Just (Var (takeWhile isAlphaNum s), dropWhile isAlphaNum s)

number :: Parser Expr
number s = Just (Num (read (takeWhile isDigit s)), dropWhile isDigit s)


test :: String -> String
test str = case parse str of
             Just ex -> toString ex
             Nothing -> error "***Parse error: invalid expression."


-- parse :: String -> Maybe Expr
-- parse s = case expr $ filter (not . isSpace) s of
--   Just (e, "") -> Just e
--   _ -> Nothing

-- expr :: Parser Expr
-- expr s = case term s of
--   Just (e1, s') -> rest e1 s'
--   _ -> Nothing
--   where
--     rest e1 ('+' : s') = case term s' of
--       Just (e2, s'') -> rest (Add e1 e2) s''
--       _ -> Nothing
--     rest e1 ('-' : s') = case term s' of
--       Just (e2, s'') -> rest (Sub e1 e2) s''
--       _ -> Nothing
--     rest e s' = Just (e, s')

-- term :: Parser Expr
-- term s = case pow s of
--   Just (e1, s') -> rest e1 s'
--   _ -> Nothing
--   where
--     rest e1 ('*' : s') = case pow s' of
--       Just (e2, s'') -> rest (Mul e1 e2) s''
--       _ -> Nothing
--     rest e1 ('/' : s') = case pow s' of
--       Just (e2, s'') -> rest (Div e1 e2) s''
--       _ -> Nothing
--     rest e s' = Just (e, s')

-- pow :: Parser Expr
-- pow s = case factor s of
--   Just (e1, s') -> rest e1 s'
--   _ -> Nothing
--   where
--     rest e1 ('^' : s') = case factor s' of
--       Just (e2, s'') -> rest (Pow e1 e2) s''
--       _ -> Nothing
--     rest e s' = Just (e, s')

-- factor :: Parser Expr
-- factor [] = Nothing
-- factor ('-' : s) = case factor s of
--   Just (e, s') -> Just (Neg e, s')
--   _ -> Nothing
-- factor ('(' : s) = case expr s of
--   Just (e, ')' : s') -> Just (e, s')
--   _ -> Nothing
-- factor (c : s)
--   | isAlpha c = identifier (c : s)
--   | isDigit c = number (c : s)
--   | otherwise = Nothing

-- identifier :: Parser Expr
-- identifier s = Just (Var (takeWhile isAlphaNum s), dropWhile isAlphaNum s)

-- number :: Parser Expr
-- number s = Just (Num (read (takeWhile isDigit s)), dropWhile isDigit s)

-- differentiate :: String -> String -> String
-- differentiate inp var = case parse inp of
--                           Just ex -> toString $ simpl $ diff ex var
--                           Nothing -> error "***Illegal input: parse error."