import Data.Char

data Expr = Func String Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Neg Expr
          | Var String
          | Num Double
          deriving Show

type Parser a = String -> Maybe (a, String)

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
                                               Just (arg, ')':rest2) -> Just (Func name arg, rest2)
                                               _ -> Nothing
               _ -> Nothing

identifier :: Parser Expr
identifier s = Just (Var (takeWhile isAlphaNum s), dropWhile isAlphaNum s)

number :: Parser Expr
number s = Just (Num (read (takeWhile isDigit s)), dropWhile isDigit s)
