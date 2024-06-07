
import Control.Monad

import Data.Functor ((<&>))
import Control.Applicative
    ( Alternative(many), (<|>), empty, some )
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
item = eatChar

-- consumes a single character if `pred` succeeds, otherwise fails
satisfyChar :: (Char -> Bool) -> Parser Char
-- sat pred = do
    -- char <- parseChar
    -- if pred char then pure char else zero
-- sat pred = 
--     parseChar >>= \char -> 
--     if pred char then pure char else zero
satisfyChar pred = satisfy pred eatChar



satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy pred parser = do {a <- parser; guard (pred a); return a}


charParser :: Char -> Parser Char
charParser charToMatch = satisfyChar (==charToMatch)

-- `mapM` runs `charParser` for each input character and puts the result
-- together into a list of characters, namely the desired `String`
stringParser :: String -> Parser String
stringParser = mapM charParser

letterParser :: Parser Char
letterParser = satisfyChar isLetter
digitParser :: Parser Char
digitParser = satisfyChar isDigit





intsParser :: Parser String
intsParser = some digitParser


parensParser :: Parser a -> Parser a
parensParser p = charParser '(' *> p <* charParser ')'

bracketsParser :: Parser a -> Parser a
bracketsParser p = charParser '[' *> p <* charParser ']'
bracketsParser' :: Parser a -> Parser a
bracketsParser' p = stringParser "[" *> p <* stringParser "]"

-- TODO: make parser ignore repeated ']'
descParser :: Parser String
descParser = (spaceParser *> stringParser "[[" *> many (satisfyChar (/= ']')) <* stringParser "]]")
    <|> stringParser ""
-- multipleIntsParser :: Parser String
-- multipleIntsParser = do
--     many bracketsParser $ intsParser <|> stringParser ","


data IntsExpr = ManyInts [String] | OneInt String
    deriving (Show, Eq)

intsExprParser :: Parser IntsExpr
intsExprParser = ManyInts <$> bracketsParser (many $ (intsParser <* stringParser ",") <|> intsParser)
            <|> OneInt <$> intsParser

    -- join  (some intsParser)

-- bind :: Parser a -> (a -> Parser b) -> Parser b
-- p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]

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

data FuncDefExpr = FuncDefExpr{
    fname :: String,
    fargNames :: [String],
    fretNames :: [String],
    fargDefns :: [FuncArgDefn]
} deriving (Show, Eq)


basicdefparser :: Parser (String, [String], [String])
basicdefparser = do
    fname <- varNameParser
    -- spaceParser
    charParser '('
    -- spaceParser
    fargNames <- nameParser
    charParser ')'
    -- stringParser " -> "
    -- fretNames <- nameParser
    return (fname, fargNames, ["rets"])
    where
        nameParser = some ((stringParser ", " *> varNameParser)  <|> varNameParser)


funcDefExprParser :: Parser FuncDefExpr
funcDefExprParser = do
    fname <- varNameParser
    -- spaceParser
    charParser '('
    -- spaceParser
    fargNames <- nameParser
    charParser ')' 
    stringParser " -> "
    fretNames <- nameParser
    spaceParser
    stringParser "where"
    spaceParser
    fargTypes <- many funcArgDefnParser
    pure $ FuncDefExpr fname fargNames fretNames fargTypes
    
    where
        -- nameParser = many (many letterParser <|> (some letterParser <* stringParser ", " ))
        nameParser = spaceParser *> 
            many ((varNameParser <* spaceParser <* stringParser "," <* spaceParser) <|> varNameParser)
            <* spaceParser

data FuncArgDefn = FuncArgDefn {
    aName :: String,
    aType :: TypeExpr,
    aDesc :: String
} deriving (Show, Eq)

-- should look like <retname>: <retType> [[<desc>]]
funcArgDefnParser :: Parser FuncArgDefn
funcArgDefnParser = do
    argName <- varNameParser <* stringParser ":" <* spaceParser
    argType <- typeExprParser
    FuncArgDefn argName argType <$> descParser



data TypeExpr = SingleBasicType PrimitiveType | MultiBasicTypes [PrimitiveType]
    deriving (Show, Eq)


typeExprParser :: Parser TypeExpr
typeExprParser =
    SingleBasicType <$> primitiveTypeParser
    <|> MultiBasicTypes <$> parensParser (many $
        primitiveTypeParser <* stringParser "|"
        <|> primitiveTypeParser
    )

newtype PrimitiveType = PrimitiveType String deriving (Show, Eq)

primitiveTypeParser :: Parser PrimitiveType
primitiveTypeParser  = fmap PrimitiveType $
    spaceParser *> (
        stringParser "string" <|> stringParser "integer"
        <|> stringParser "boolean" <|> stringParser "nil"
        <|> stringParser "number"
    ) <* spaceParser


-- data PrimitiveType = STRING | INT | BOOL | NIL | NUMBER deriving (Show, Eq)
-- primitiveTypeParser :: Parser PrimitiveType
-- primitiveTypeParser  = Parser $ \str ->
--     let res = stringParser "string" <|> stringParser "integer" <|> stringParser "boolean" <|> stringParser "nil"
--             <|> stringParser "number"
--         in case runParser res str of
--         Left errMsg -> Left errMsg
--         Right (x, rest) -> 
--             Right (
--                 case x of
--                     "string" -> STRING
--                 , 
--                 rest
--             )
--     where
--         evalParser pat s = run
-- primitiveTypeParser t = Parser $ case t of
--     STRING -> \str -> runParser (stringParser "string") str
-- typeExprParser :: Parser TypeExpr
-- typeExprParser = SingleBasicType <$> bracketsParser (many $ (intsParser <* stringParser ",") <|> intsParser)
--             <|> OneInt <$> intsParser
--     where 


main = do
    -- (putStr "Digits: " >> getLine) >>= putStrLn . parseToString intsParser
    -- (putStr "Digits: " >> getLine) >>= putStrLn . parseToString intsExprParser
    -- (putStr "Type: " >> getLine) >>= putStrLn . parseToString typeExprParser
    -- args <- getArgs
    -- putStrLn . parseToString typeExprParser $ head args
    -- getArgs >>= putStrLn . parseToString funcRetExprParser . head
    getArgs >>= putStrLn . parseToString funcDefExprParser . head
    -- getArgs >>= putStrLn . parseToString typeExprParser . head

    -- matchInput <$> (putStr "Pattern: " >> getLine) <*> (putStr "Input: " >> getLine)
    --    >>= putStrLn