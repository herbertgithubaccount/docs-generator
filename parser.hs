-- parser code inspired by this stackoverflow post: https://codereview.stackexchange.com/questions/253497/simple-haskell-parser
-- and this articled linked in said post: https://www.cs.nott.ac.uk/%7Epszgmh/monparsing.pdf

import Control.Monad

import Data.Functor ((<&>))
import Control.Applicative
    ( Alternative(many), (<|>), empty, some, liftA3 )
import Distribution.Compat.CharParsing (letter, CharParsing (string))
import Data.Char (isLetter, isDigit)
import System.Directory.Internal.Prelude (getArgs)

type ErrorMsg = String

newtype Parser a = Parser { runParser :: String -> Either ErrorMsg (a, String) }
  deriving (Functor)

-- item (x:xs) = Right (x, xs)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    -- given a Parser `p`, and a function `f: A -> Parser B`, 
    -- apply `p` to `str`
    -- if this results in an error, return it and abort
    -- otherwise, if `p` parses `str` successfully, then 
        -- `runParser p str` will return a pair `(a, str')`, where `a \in A` and `str'` is what's left of `str`.
        -- so, we call `f a`, which returns a parser `p_B`, and then we call that on `str'`. 
    p >>= f = Parser $ \str -> case runParser p str of
            Left errMsg -> Left errMsg
            Right (a, str') -> runParser (f a) str'

instance Applicative Parser where
  pure :: a -> Parser a
  -- pure function sends a string `str` to `Parser ( Right (a, str))`
  pure a = Parser $ Right . (a, )

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) f p = f >>= (p <&>)

  -- the following are equivalent:
--   (<*>) f p = f >>= (p <&>)
--   (<*>) f p = f >>= (\f' -> p >>= \a -> pure (f' a))
--   (<*>) = ap
--   (<*>) f p = do { f' <- f; p' <- p; return (f' p') }

instance MonadFail Parser where
    fail = Parser . const . Left

instance Alternative Parser where
  empty = fail "<empty>"
  -- try parser `p`, then try parser `q` if `p` fails
  p <|> q = Parser $ \str -> case runParser p str of
    Left _ -> runParser q str;
    res -> res


instance MonadPlus Parser


result :: a -> Parser a
result = pure

zero :: Parser a
zero = empty

-- will always eat up a character, if such a character exists
eatChar :: Parser Char
eatChar = Parser f where
    f "" = Left "Unexpected EOF"
    f (x:xs) = Right (x, xs)
-- parseChar = Parser(\case{
--     [] -> runParser empty ""; 
--     (x:xs) -> Right (x, xs)
-- })



satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy pred parser = do {a <- parser; guard (pred a); return a}

-- consumes a single character if `pred` succeeds, otherwise fails
satisfyChar :: (Char -> Bool) -> Parser Char
satisfyChar pred = satisfy pred eatChar

charParser :: Char -> Parser Char
charParser charToMatch = satisfyChar (==charToMatch)
charParser' :: Char -> Parser Char
charParser' charToMatch = satisfyChar (==charToMatch) <|> fail ("Failed to match " ++ [charToMatch])

-- `mapM` runs `charParser` for each input character and puts the result
-- together into a list of characters, namely the desired `String`
stringParser :: String -> Parser String
stringParser = mapM charParser

stringParser' :: String -> Parser String
stringParser' str = mapM charParser str <|> fail ("Failed to match " ++ str)

letterParser :: Parser Char
letterParser = satisfyChar isLetter
digitParser :: Parser Char
digitParser = satisfyChar isDigit


addErrMsg :: Parser a -> String -> Parser a
addErrMsg p str = p <|> fail str

intsParser :: Parser String
intsParser = some digitParser

parensParser :: Parser a -> Parser a
parensParser p = charParser '(' *> p <* charParser ')'
parensParser' :: Parser a -> Parser a
parensParser' p = addErrMsg (charParser '(') "Expected '('" *> p <* addErrMsg (charParser ')') "Expected ')'"

bracketsParser :: Parser a -> Parser a
bracketsParser p = charParser '[' *> p <* charParser ']'
bracketsParser' :: Parser a -> Parser a
bracketsParser' p = stringParser' "[" *> p <* stringParser' "]"

-- TODO: make parser ignore repeated ']'
descParser :: Parser String
descParser = (spaceParser *> stringParser' "[[" *> many (satisfyChar (/= ']')) <* stringParser' "]]")
    <|> stringParser ""

matchInput :: String -> String -> String
matchInput = parseToString . stringParser


parseToString :: Show a => Parser a -> String -> String
parseToString p str = case runParser p str of
    Left errMsg -> errMsg
    Right x -> show x


varNameParser :: Parser String
varNameParser = some $ satisfyChar isLetter

spaceParser :: Parser String
spaceParser = many $ charParser ' '

surroundBySpaces :: Parser a -> Parser a
surroundBySpaces p = spaceParser *> p <* spaceParser

class LuaExpr a where
    parser :: Parser a
    parseString :: String -> Either ErrorMsg a
    parseString str = case runParser parser str of
        Left err -> Left err
        Right (a, _) -> Right a
    parseString' :: String -> Either ErrorMsg (a, String)
    parseString' = runParser parser

data IntsExpr = ManyInts [String] | OneInt String
    deriving (Show, Eq)

instance LuaExpr IntsExpr where
    parser = ManyInts <$> bracketsParser (many $ (intsParser <* stringParser' ",") <|> intsParser)
            <|> OneInt <$> intsParser


    -- join  (some intsParser)

-- bind :: Parser a -> (a -> Parser b) -> Parser b
-- p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]


data FuncDefExpr = FuncDefExpr{
    fname :: String,
    fargNames :: [String],
    fretNames :: [String],
    fargDefns :: [FuncArgDefn]
} deriving (Show, Eq)


instance LuaExpr FuncDefExpr where
    parser = do
        fname <- addErrMsg varNameParser  "Failed to parse function name"
        fargNames <- parensParser' namesParser
        surroundBySpaces (stringParser' "->")
        fretNames <- namesParser
        surroundBySpaces (stringParser' "where")
        fargTypes <- many (parser :: Parser FuncArgDefn)
        pure $ FuncDefExpr fname fargNames fretNames fargTypes

        where
            nameParser = spaceParser *> varNameParser <* ((spaceParser <* charParser ',') <|> stringParser "")
            namesParser = many nameParser <* spaceParser
            -- namesParser = many nameParser <* spaceParser
            -- nameParser = 
            --     (many $ spaceParser *> ((varNameParser <* spaceCommaParser) <|> varNameParser))
            --     <* spaceParser


data FuncArgDefn = FuncArgDefn {
    aName :: String,
    aType :: TypeExpr,
    aDesc :: String
} deriving (Show, Eq)

instance LuaExpr FuncArgDefn where
    parser = liftA3 FuncArgDefn funcArgNameParser (parser :: Parser TypeExpr) descParser
        where 
            funcArgNameParser = varNameParser <* charParser' ':' <* spaceParser
    -- parser = do
    --     argName <- varNameParser <* stringParser' ":" <* spaceParser
    --     argType <- typeExprParser
    --     FuncArgDefn argName argType <$> descParser
-- should look like <retname>: <retType> [[<desc>]]



data TypeExpr = SingleBasicType PrimitiveType | MultiBasicTypes [PrimitiveType]
    deriving (Show, Eq)

instance LuaExpr TypeExpr where
    parser = SingleBasicType <$> primitiveTypeParser <|> MultiBasicTypes <$> multiTypeParser
        where
            primitiveTypeParser = parser :: Parser PrimitiveType
            multiTypeParser = parensParser' $ many (primitiveTypeParser <* stringParser' "|" <|> primitiveTypeParser)

newtype PrimitiveType = PrimitiveType String deriving (Show, Eq)

instance LuaExpr PrimitiveType where
    parser = PrimitiveType <$> surroundBySpaces typesParser
        where typesParser = stringParser "string" <|> stringParser "integer" <|> stringParser "boolean" 
                        <|> stringParser "nil" <|> stringParser "number"




main = do
    -- (putStr "Digits: " >> getLine) >>= putStrLn . parseToString intsParser
    -- (putStr "Digits: " >> getLine) >>= putStrLn . parseToString intsExprParser
    -- (putStr "Type: " >> getLine) >>= putStrLn . parseToString typeExprParser
    -- args <- getArgs
    -- putStrLn . parseToString typeExprParser $ head args
    -- getArgs >>= putStrLn . parseToString funcRetExprParser . head
    getArgs >>= putStrLn . parseToString (parser:: Parser FuncDefExpr) . head
    -- getArgs >>= putStrLn . parseToString typeExprParser . head

    -- matchInput <$> (putStr "Pattern: " >> getLine) <*> (putStr "Input: " >> getLine)
    --    >>= putStrLn