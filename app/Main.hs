{-# LANGUAGE ExistentialQuantification #-}

module Main where

--TODO make modules

import Lib

--TODO use Except?
import Control.Monad.Error.Class
import Control.Monad(liftM)
import Data.Char(digitToInt)
import Data.Complex(Complex((:+)), realPart, imagPart)
import Data.Digits(unDigits)
import Data.List(findIndices)
import Data.Maybe(fromMaybe)
import Data.Ratio((%), numerator, denominator)
import Numeric
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled

evalL = eval . extractValue . readExpr

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
 
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNumber
             | String String
             | Character Char
             | Bool Bool

data LispNumber = LComplex (Complex Float)
                | LReal     Float
                | LRational Rational
                | LInteger  Integer
    deriving (Eq)

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispNumber where show = showNum
showNum :: LispNumber -> String
showNum (LComplex c) = show (realPart c) ++ "+" ++ show (imagPart c) ++ "i"
showNum (LReal f) = show f
showNum (LRational rat) = show (numerator rat) ++ "/" ++ show (denominator rat)
showNum (LInteger i) = show i

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

letterOrNumber :: Parser Char
letterOrNumber = oneOf $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

spaces :: Parser ()
spaces = skipMany1 space

parens = between (char '(') (char ')')

quotes = between quote quote
    where quote = char '\"'

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

parseString :: Parser LispVal
parseString =
    String <$> quotes stringContent
            where
                stringContent = many (nonEscaped <|> escaped)
                nonEscaped = noneOf "\\\""
                escaped = do
                    char '\\' 
                    c <- oneOf "nrt\\\""
                    return $ case c of
                        'n'  -> '\n'
                        'r'  -> '\r'
                        't'  -> '\t'
                        '\\' -> '\\'
                        '\"' -> '\"'

parseCharacter :: Parser LispVal
parseCharacter = do
    string "#\\"
    c <- (try characterName) <|> simpleCharacter 
    return $ Character c
        where
            simpleCharacter = symbol <|> letterOrNumber <|> return ' '
            characterName = 
                    (string "backspace" >> return '\b')
                <|> (string "newline"   >> return '\n')
                <|> (string "page"      >> return '\f')
                <|> (string "return"    >> return '\r')
                <|> (string "space"     >> return ' ' )
                <|> (string "tab"       >> return '\t')

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- TODO these types might not be 100% suitable
parseNumber :: Parser LispVal
parseNumber = 
        (try parseComplex  >>= (return . Number . LComplex ) )
    <|> (try parseReal     >>= (return . Number . LReal    ) )
    <|> (try parseRational >>= (return . Number . LRational) )
    <|> (    parseInteger  >>= (return . Number . LInteger ) )

parseComplex :: Parser (Complex Float)
parseComplex = do
    real <- parseReal
    char '+'
    imag <- parseReal
    char 'i'
    return $ real :+ imag

parseReal :: Parser Float
parseReal = do
    l <- many1 digit
    char '.'
    r <- many digit
    let f = l ++ "." ++ r
    return $ (fst . head . readFloat) f

parseRational :: Parser Rational
parseRational = do
    numerator <- parseInteger
    char '/'
    denominator <- parseInteger
    return (numerator % denominator)

parseInteger :: Parser Integer
parseInteger = do
    base <- parseBasePrefix
    n <- many1 $ oneOf (take (fromIntegral base) validDigits)
    return $ unDigits base $ map (toInteger . digitToInt) n
        where validDigits = ['0'..'9'] ++ ['A'..'F']

parseBasePrefix :: Parser Integer
parseBasePrefix =
    option 10 $ do
        char '#'
        b <- oneOf "bodx"
        return $ fromMaybe 10 $ lookup b [('b', 2), ('o',8), ('d',10), ('x',16)]

parseExpr :: Parser LispVal
parseExpr =  try parseCharacter 
         <|> parseString
         <|> try parseNumber
         <|> parseAtom
         <|> parseQuoted
         <|> parens (try parseList <|> parseDottedList)

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- TODO Excercises at end of Section Parsing
--   1. Add support for backquote
--   2. Add support for vectors

-- Evaluator
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func 
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient" , numericBinop quot),
              ("remainder", numericBinop rem),
              ("=" , numBoolBinop (==)),
              ("<" , numBoolBinop (<)),
              (">" , numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?" , strBoolBinop (==)),
              ("string<?" , strBoolBinop (<)),
              ("string>?" , strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car"   , car ),
              ("cdr"   , cdr ),
              ("cons"  , cons),
              ("eq?"   , eqv ),
              ("eqv?"  , eqv ),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . LInteger . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool (String "#t") = return True
unpackBool (String "#f") = return False
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- Note: this converts even Complex/Real/Rational types to Integer
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number (LComplex c)) = return $ toInteger $ round $ realPart c
unpackNum (Number (LReal f)) = return $ toInteger $ round f
unpackNum (Number (LRational r)) = return $ toInteger $ round r
unpackNum (Number (LInteger n)) = return $ n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- Error

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val