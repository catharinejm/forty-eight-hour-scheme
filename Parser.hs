import Text.ParserCombinators.Parsec hiding (spaces)
import System
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                 Left err -> "No match: " ++ show err
                 Right val -> "Found value: " ++ show val

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many escapedChar
                 char '"'
                 return $ String x

escapedChar = try (string "\\\"" >> return '"')
              <|> try (string "\\\\" >> return '\\')
              <|> try (string "\\n" >> return '\n')
              <|> try (string "\\t" >> return '\t')
              <|> try (string "\\r" >> return '\r')
              <|> noneOf "\""

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber