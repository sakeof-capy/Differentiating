import Data.Char

-- Define a data type to represent expressions
data Expr = Num Double | Var String | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr deriving Show

-- Define a parser function that takes an input string and returns an expression
-- parseExpr :: String -> Maybe Expr
-- parseExpr s = case parseAddSub s of
--                 Just (expr, "") -> Just expr
--                 _ -> Nothing

parseExpr :: String -> Maybe Expr
parseExpr s = case parseAddSub (filter (not . isSpace) s) of
                Just (expr, "") -> Just expr
                _ -> Nothing

-- Define a function to parse addition and subtraction expressions
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

-- Define a function to parse multiplication and division expressions
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

-- Define a function to parse simple terms (numbers or variables)
-- parseTerm :: String -> Maybe (Expr, String)
-- parseTerm s@('-':rest) = do
--     (expr, s') <- parseTerm rest
--     return (Sub (Num 0) expr, s')
-- parseTerm s@(c:_)
--     | isDigit c = parseNum s
--     | isAlpha c = parseVar s
-- parseTerm _ = Nothing

-- Define a function to parse simple terms (numbers, variables, or parenthesized expressions)
parseTerm :: String -> Maybe (Expr, String)
parseTerm s@('-':rest) = do
    (expr, s') <- parseTerm rest
    return (Sub (Num 0) expr, s')
parseTerm s@(c:_)
    | isDigit c = parseNum s
    | isAlpha c = parseVar s
    | c == '(' = do
        (expr, s') <- parseAddSub (tail s)
        case s' of
            ')':s'' -> return (expr, s'')
            _ -> Nothing
parseTerm _ = Nothing


-- Define a function to parse numbers
parseNum :: String -> Maybe (Expr, String)
parseNum s =
    let (digits, rest) = span isDigit s in
    case rest of
        '.' : rest' -> do
            (digits', rest'') <- parseDigits rest'
            let numStr = digits ++ "." ++ digits'
            return (Num (read numStr), rest'')
        _ -> return (Num (read digits), rest)

-- Define a function to parse variable names
parseVar :: String -> Maybe (Expr, String)
parseVar s =
    let (name, rest) = span isAlphaNum s in
    Just (Var name, rest)

-- Define a function to parse digits after the decimal point
parseDigits :: String -> Maybe (String, String)
parseDigits s =
    let (digits, rest) = span isDigit s in
    if null digits
        then Nothing
        else Just (digits, rest)
