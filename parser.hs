-- parser code inspired by this stackoverflow post: https://codereview.stackexchange.com/questions/253497/simple-haskell-parser
-- and this articled linked in said post: https://www.cs.nott.ac.uk/%7Epszgmh/monparsing.pdf

import Control.Monad

import Data.Functor ((<&>))
import Control.Applicative
    ( Alternative(many), (<|>), empty, some, liftA3, Applicative (liftA2) )
import Distribution.Compat.CharParsing (letter, CharParsing (string))
import Data.Char (isLetter, isDigit)
import System.Directory.Internal.Prelude (getArgs)

type ErrorMsg = String

type ParserResult a = (Either ErrorMsg a, String)
newtype Parser a = Parser { runParser :: String -> ParserResult a}
  deriving (Functor)


fmapRight :: (a -> b) -> Parser a -> Parser b
fmapRight f p = Parser $ \str -> let (res, str') = runParser p str in ( , str') $ case res of
        Left err -> Left err
        Right res'' -> Right $ f res''
pureRight :: Applicative f => Parser a -> Parser (f a)
pureRight = fmapRight pure
-- pureRight p = Parser $ \str -> let (res, str') = runParser p str in ( , str') $ case res of 
--         Left err -> Left err
--         Right res'' -> Right $ pure res''



instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    -- given a Parser `p`, and a function `f: A -> Parser B`, 
    -- apply `p` to `str`
    -- if this results in an error, return it and abort
    -- otherwise, if `p` parses `str` successfully, then 
        -- `runParser p str` will return a pair `(a, str')`, where `a \in A` and `str'` is what's left of `str`.
    --     -- so, we call `f a`, which returns a parser `p_B`, and then we call that on `str'`. 
    -- p >>= f = Parser $ \str -> case runParser p str of
    --         (Right a, str') -> runParser (f a) str'
    --         (Left errMsg, str') -> (Left errMsg, str')
        -- so, we call `f a`, which returns a parser `p_B`, and then we call that on `str'`. 
    p >>= f = Parser $ \str -> let (res, str') = runParser p str in case res of
            Left errMsg -> (Left errMsg, str')
            Right a -> runParser (f a) str'

instance Applicative Parser where
  pure :: a -> Parser a
  -- pure function sends a string `str` to `Parser ( Right (a, str))`
  pure a = Parser (Right a, )

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) f p = f >>= (p <&>)

  -- the following are equivalent:
--   (<*>) f p = f >>= (p <&>)
--   (<*>) f p = f >>= (\f' -> p >>= \a -> pure (f' a))
--   (<*>) = ap
--   (<*>) f p = do { f' <- f; p' <- p; return (f' p') }

instance MonadFail Parser where
    fail errMsg = Parser (Left errMsg, )

instance Alternative Parser where
  empty = fail "<empty>"
  -- try parser `p`, then try parser `q` if `p` fails
  p <|> q = Parser $ \str -> let x@(res, str') = runParser p str in case res of
        Left _ -> runParser q str;
        _ -> x


instance MonadPlus Parser

-- will always eat up a character, if such a character exists
eatChar :: Parser Char
eatChar = Parser f where
    f "" = (Left "Unexpected EOF", "")
    f (x:xs) = (Right x, xs)
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
parseToString p = show . runParser p
-- parseToString p str = let (res, str') = runParser p str in case res of
--     Left errMsg -> errMsg
--     Right x -> show x


varNameParser :: Parser String
varNameParser = liftA2 (:) (satisfyChar isLetter) (many (satisfyChar pred))
    where pred ch = isLetter ch || isDigit ch || ch == '_'
-- varNameParser = satisfyChar isLetter >> some $ satisfyChar isLetter

spaceParser :: Parser String
spaceParser = many $ charParser ' '

surroundBySpaces :: Parser a -> Parser a
surroundBySpaces p = spaceParser *> p <* spaceParser


optionalIgnore :: (Alternative f) => Parser a1 -> Parser (f a2) -> Parser a1
optionalIgnore p q = p <* (q <|> pure empty)

optional :: (Alternative f) => Parser a1 -> Parser (f a2) -> Parser (f a2)
optional p q = p >> (q <|> pure empty)

optional' :: Alternative f => Parser (f a) -> Parser (f a)
optional' = optional (pure (empty :: Parser a))

class LuaExpr a where
    parser :: Parser a

commaParser = optional' $ surroundBySpaces (stringParser' ",")

data VarDefnExpr = VarDefnExpr{vName:: String, vType:: Maybe TypeExpr} deriving (Show, Eq)

instance LuaExpr VarDefnExpr where
    parser = VarDefnExpr <$> varNameParser <*> typesParser
        where typesParser = optional' (surroundBySpaces (charParser' ':') *> pureRight parser)

data FuncDefExpr = FuncDefExpr{
    fgenericNames :: [String],
    fargNames     :: [VarDefnExpr],
    fretNames     :: [VarDefnExpr],
    fargDefns     :: [VarDefnExpr] -- used to be other type
} deriving (Show, Eq)



instance LuaExpr FuncDefExpr where
    parser = liftM4 FuncDefExpr genericsParser argsParser retsParser defnsParser
        where
            surroundedStrParser' = surroundBySpaces . stringParser'
            argsParser = (surroundBySpaces . parensParser' . many . surroundBySpaces) (parser  <* commaParser <|> parser)
                        <|> (surroundBySpaces . pureRight) parser
            retsParser = surroundedStrParser' "->" *> argsParser
            defnsParser = optional' $ surroundedStrParser' "where" *> many parser
            genericsParser = optional' (charParser '<' *> genericsNameParser <* charParser '>')
                where genericsNameParser = some (surroundBySpaces varNameParser <* commaParser)




data TypeExpr = Primitive String | NonPrimitive String | Multi [TypeExpr] | Function FuncDefExpr
    deriving (Show, Eq)

instance LuaExpr TypeExpr where

    parser = Primitive <$> primitiveParser
        <|> NonPrimitive <$> nonPrimitiveParser
        <|> Function <$> parser
        <|> Multi <$> multiParser
        where
            primitiveParser = stringParser "string" <|> intParser <|> boolParser <|> stringParser "nil" <|> numParser
                    where   intParser = optionalIgnore (stringParser' "int") (stringParser' "eger")
                            boolParser = optionalIgnore (stringParser' "bool") (stringParser' "ean")
                            numParser = optionalIgnore (stringParser' "num") (stringParser' "ber")
            nonPrimitiveParser = varNameParser

            commaParser = surroundBySpaces (stringParser' ",") <|> stringParser ""
            multiParser = parensParser' $ some ((parser :: Parser TypeExpr) <* commaParser)


newtype PrimitiveType = PrimitiveType String deriving (Show, Eq)

instance LuaExpr PrimitiveType where
    parser = PrimitiveType <$> surroundBySpaces typesParser
        where typesParser = stringParser "string" <|> stringParser "integer" <|> stringParser "boolean"
                        <|> stringParser "nil" <|> stringParser "number"
-- data TypeExpr = SingleBasicType PrimitiveType | MultiBasicTypes [PrimitiveType]
--     deriving (Show, Eq)

-- instance LuaExpr TypeExpr where
--     parser = SingleBasicType <$> primitiveTypeParser <|> MultiBasicTypes <$> multiTypeParser
--         where
--             primitiveTypeParser = parser :: Parser PrimitiveType
--             multiTypeParser = parensParser' $ many (primitiveTypeParser <* stringParser' "|" <|> primitiveTypeParser)

-- newtype PrimitiveType = PrimitiveType String deriving (Show, Eq)

-- instance LuaExpr PrimitiveType where
--     parser = PrimitiveType <$> surroundBySpaces typesParser
--         where typesParser = stringParser "string" <|> stringParser "integer" <|> stringParser "boolean"
--                         <|> stringParser "nil" <|> stringParser "number"




main = do
    -- (putStr "Digits: " >> getLine) >>= putStrLn . parseToString intsParser
    -- (putStr "Digits: " >> getLine) >>= putStrLn . parseToString intsExprParser
    -- (putStr "Type: " >> getLine) >>= putStrLn . parseToString typeExprParser
    -- args <- getArgs
    -- putStrLn . parseToString typeExprParser $ head args
    -- getArgs >>= putStrLn . parseToString funcRetExprParser . head
    getArgs >>= putStrLn . parseToString (parser:: Parser VarDefnExpr) . head
    -- getArgs >>= putStrLn . parseToString typeExprParser . head

    -- matchInput <$> (putStr "Pattern: " >> getLine) <*> (putStr "Input: " >> getLine)
    --    >>= putStrLn