{-# OPTIONS_GHC -Wall #-}
module Differentiating where
import Data.Char
import Data.List

data Expr = Fun String Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Neg Expr
          | Var String
          | Num Double
          deriving (Show, Eq)

-- instance Eq Expr where
--   (Fun s1 e1) == (Fun s2 e2) = s1 == s2 && e1 == e2
--   (Add e1 e2) == (Add e3 e4) = (e1 == e3 && e2 == e4) || (e1 == e4 && e2 == e3)
--   (Sub e1 e2) == (Sub e3 e4) = e1 == e3 && e2 == e4
--   (Mul e1 e2) == (Mul e3 e4) = (e1 == e3 && e2 == e4) || (e1 == e4 && e2 == e3)
--   (Div e1 e2) == (Div e3 e4) = e1 == e3 && e2 == e4
--   (Pow e1 e2) == (Pow e3 e4) = e1 == e3 && e2 == e4
--   (Neg e1) == (Neg e2) = e1 == e2
--   (Var s1) == (Var s2) = s1 == s2
--   (Num d1) == (Num d2) = d1 == d2
--   _ == _ = False

type WholedExpr = Expr -> Expr
type DiffTable = [(String, WholedExpr)]
type Parser a = String -> Maybe (a, String)

diffTable :: DiffTable
diffTable = 
        [
           ("sin",  \x -> Fun "cos" x),
           ("cos",  \x -> Neg$ Fun "sin" x),
           ("tan",  \x -> Div (Num 1.0) (Pow (Fun "cos" x) (Num 2.0))),
           ("cot",  \x -> Neg$ Div (Num 1.0) (Pow (Fun "sin" x) (Num 2.0))),
           ("sqrt", \x -> Div (Num 1.0) (Mul (Num 2.0) (Fun "sqrt" x))),
           ("ln",   \x -> Div (Num 1.0) x)
        ]

isValidVarName :: String -> Bool
isValidVarName name = case find (\(name', _) -> name == name') diffTable of
                        Nothing -> True
                        _       -> False

diff :: Expr -> String -> Expr
diff ex v | isValidVarName v = diff' ex v
          | otherwise        = error$ "Semantic error: Invalid variable name \"" ++ v ++ "\""                  

diff' :: Expr -> String -> Expr
diff' (Add ex1 ex2) v = Add (diff' ex1 v) (diff' ex2 v)
diff' (Sub ex1 ex2) v = Sub (diff' ex1 v) (diff' ex2 v)
diff' (Mul ex1 ex2) v = Add (Mul (diff' ex1 v) ex2) (Mul ex1 (diff' ex2 v))
diff' (Div ex1 ex2) v = Div (Sub (Mul (diff' ex1 v) ex2) (Mul ex1 (diff' ex2 v))) (Pow ex2 (Num 2))
diff' (Pow ex1 (Num n)) v = Mul (Num n) (Mul (diff' ex1 v) (Pow ex1 (Num (n - 1))))
diff' (Pow ex1 ex2) v = Mul (Pow (ex1) (ex2)) (Add (Mul (diff' ex1 v) (Div ex2 ex1)) (Mul (diff' ex2 v) (Fun "ln" ex1)))
diff' (Neg ex)      v = Neg $ diff' ex v
diff' (Var c)       v | v == c    = Num 1
                      | otherwise = Num 0
diff' (Num _)       _ = Num 0
diff' (Fun name ex) v = case find (\(name', _) -> name == name') diffTable of
                       Just (_, der) ->  Mul (diff' ex v) (der ex)
                       Nothing       -> error $ "Semantic error: Not differentiable function: \"" ++ name ++ "\""

simpl :: Expr -> Expr
simpl (Add e1 e2) = case (simpl e1, simpl e2) of
                      (Num 0, e) -> e
                      (e, Num 0) -> e
                      (Num n1, Num n2) -> Num (n1 + n2)
                      (Mul (Num n1) e1', Mul (Num n2) e2') -> 
                        if e1' == e2'
                        then Mul (Num (n1 + n2)) e1'
                        else Add (Mul (Num n1) e1') (Mul (Num n2) e2')
                      (e1', (Add e2' e3')) -> Add (Add e1' e2') e3'
                      (e1', e2') -> 
                        if e1' == e2'
                        then Mul (Num 2.0) (e1')
                        else Add e1' e2'
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
                      (Var v, Num n) -> Mul (Num n) (Var v)
                      (Pow (Var v) (Num p), Num n) -> Mul (Num n) (Pow (Var v) (Num p))
                      --(e1', Add e2' e3') -> Add (simpl$ Mul e1' e2') (simpl$ Mul e1' e3')
                      --(Add e2' e3', e1') -> Add (simpl$ Mul e1' e2') (simpl$ Mul e1' e3')
                      (e1', e2') -> 
                        if e1' == e2'
                        then Pow e1' (Num 2.0)
                        else Mul e1' e2'
simpl (Div e1 e2) = case (simpl e1, simpl e2) of
                      (_, Num 0) -> error "Division by zero!"
                      (Num 0, _) -> Num 0
                      (Num n1, Num n2) -> Num (n1 / n2)
                      (e1', e2') -> if e1' == e2'
                                    then Num 1
                                    else Div e1' e2'
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
simpl (Fun f arg) = Fun f $ simpl arg


toString :: Expr -> String
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

parseIgnoreSpaces :: String -> Maybe Expr
parseIgnoreSpaces = parse . (filter (not . isSpace))

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
                                Nothing -> case identifier s of
                                    Just((Var v, s'')) -> if (isValidVarName v) 
                                                          then Just((Var v, s'')) 
                                                          else error$ "Parse error: Invalid variable name \"" ++ v ++ "\""
                                    _                  -> Nothing
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

test :: String -> String -> String
test str v = case parseIgnoreSpaces str of
             Just ex -> toString$  simpl ex 
             Nothing -> error "Parse error: Invalid expression."

diffString :: String -> String -> String
diffString str v = case parseIgnoreSpaces str of
                     Just ex -> toString$ simpl$ diff ex v
                     Nothing -> error "Parse error: Invalid expression."

----------------------------------------------------------------------
