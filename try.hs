import Data.Char

-- Define a data type to represent expressions
data Expr = Exp Expr Expr 
          | Num Double 
          | Var String 
          | Add Expr Expr 
          | Sub Expr Expr 
          | Mul Expr Expr 
          | Div Expr Expr 
          deriving Show

parseExpr :: String -> Maybe Expr
parseExpr s = case parseAddSub (filter (not . isSpace) s) of
                Just (expr, "") -> Just expr
                _ -> Nothing

parseAddSub :: String -> Maybe (Expr, String)
parseAddSub s = do
    (expr1, s1) <- parseMulDiv s
    case s1 of
        '+':s2 -> do
            (expr2, s3) <- parseAddSub s2
            return (Add expr1 expr2, s3)
        '-':s2 -> do
            (expr2, s3) <- parseAddSub s2
            return (Sub expr1 expr2, s3)
        _ -> return (expr1, s1)

parseMulDiv :: String -> Maybe (Expr, String)
parseMulDiv s = do
    (expr1, s1) <- parseTerm s
    case s1 of
        '*':s2 -> do
            (expr2, s3) <- parseMulDiv s2
            return (Mul expr1 expr2, s3)
        '/':s2 -> do
            (expr2, s3) <- parseMulDiv s2
            return (Div expr1 expr2, s3)
        _ -> return (expr1, s1)

parseTerm :: String -> Maybe (Expr, String)
parseTerm s@('-':rest) = do
    (expr, s') <- parseTerm rest
    return (Sub (Num 0) expr, s')
parseTerm s@(c:_)
    | isAlpha c = parseVar s
    | c == '(' = do
        (expr, s') <- parseAddSub (tail s)
        case s' of
            ')':s'' -> return (expr, s'')
            _ -> Nothing
    | otherwise = parseExpFactor s

parseNum :: String -> Maybe (Expr, String)
parseNum s =
    let (digits, rest) = span isDigit s in
    case rest of
        '.' : rest' -> do
            (digits', rest'') <- parseDigits rest'
            let numStr = digits ++ "." ++ digits'
            return (Num (read numStr), rest'')
        _ -> return (Num (read digits), rest)

parseVar :: String -> Maybe (Expr, String)
parseVar s =
    let (name, rest) = span isAlphaNum s in
    Just (Var name, rest)

parseDigits :: String -> Maybe (String, String)
parseDigits s =
    let (digits, rest) = span isDigit s in
    if null digits
        then Nothing
        else Just (digits, rest)


-- Define a function to parse a factor (number, variable, or parenthesized expression)
parseFactor :: String -> Maybe (Expr, String)
parseFactor s@('-':rest) = do
    (expr, s') <- parseFactor rest
    return (Sub (Num 0) expr, s')
parseFactor s@(c:_)
    | isDigit c = parseNum s
    | isAlpha c = parseVar s
    | c == '(' = do
        (expr, s') <- parseAddSub (tail s)
        case s' of
            ')':s'' -> return (expr, s'')
            _ -> Nothing
    | otherwise = Nothing

-- Define a function to parse an exponentiation factor (base factor with optional exponent)
parseExpFactor :: String -> Maybe (Expr, String)
parseExpFactor s = do
    (base, s') <- parseFactor s
    case s' of
        '^':s'' -> do
            (exp, s''') <- parseExpFactor s''
            return (Exp base exp, s''')
        _ -> return (base, s')

-- parseFactor :: String -> Maybe (Expr, String)
-- parseFactor s@('-':rest) = do
--     (expr, s') <- parseFactor rest
--     return (Sub (Num 0) expr, s')
-- parseFactor s@(c:_)
--     | isDigit c = parseNum s
--     | isAlpha c = parseVar s
--     | c == '(' = do
--         (expr, s') <- parseAddSub (tail s)
--         case s' of
--             ')':s'' -> return (expr, s'')
--             _ -> Nothing
--     | otherwise = Nothing

-- parseExpFactor :: String -> Maybe (Expr, String)
-- parseExpFactor s = do
--     (base, s') <- parseFactor s
--     case s' of
--         '^':s'' -> do
--             (exp, s''') <- parseExpFactor s''
--             return (Exp base exp, s''')
--         _ -> return (base, s')
