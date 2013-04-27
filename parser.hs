module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding(spaces)
import Control.Monad
import Numeric

data LispVal = 
		Atom String
	|	List [LispVal]
	|	DottedList [LispVal] LispVal
	|	Number Integer
	|	String String
	|	Char Char
	|	Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = 
	case parse parseExpr "lisp" input of 
		Left err -> "No match: " ++ show err
		Right value -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseEscapeString :: Parser Char
parseEscapeString =
		((char '\\') >>
			(	((char '"') >> return '"')
			<|>	((char '\\') >> return '\\')
			<|>	((char 'n') >> return '\n')
			<|>	((char 't') >> return '\t')
			<|>	((char 'r') >> return '\r')
			)
		)
	<|>	(noneOf "\"") 

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many parseEscapeString
	char '"'
	return $ String x

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = [first] ++ rest
	return $ case atom of 
			"#t" -> Bool True
			"#f" -> Bool False
			otherwise -> Atom atom


parseNumber :: Parser LispVal
parseNumber = 
	(do
		char '#'
		(char 'x' >> (liftM (Number . fst . head . readHex) $ many1 (oneOf ("0123456789abcdefABCDEF")))) <|>(char 'o' >> (liftM (Number . fst . head . readOct) $ many1 (oneOf ("01234567")))) <|> (char 'd' >> (liftM (Number . read) $ many1 digit))
	)
	<|> (liftM (Number. read) $ many1 digit)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

main :: IO ()
main = do 
	args <- getArgs
	putStrLn (readExpr (args !! 0))	
