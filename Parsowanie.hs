
{- lab 8 -}

import Data.Char

-- definicja parsera tak jak na końcu wykładu 7
newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) input = p input

instance Monad Parser where
    return x = Parser (\input -> [(x, input)])
    p >>= f =
        Parser $ \input ->
            case parse p input of
                [] -> []
                [(v, out)] -> parse (f v) out

-- reszta funkcji tak jak w programming-in-haskell.pdf
failure :: Parser a
failure = Parser (\_ -> [])

item :: Parser Char
item = Parser $ \input -> case input of
        [] -> []
        (x:xs) -> [(x, xs)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $
    \inp -> case parse p inp of
        [] -> parse q inp
        [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v : vs)
