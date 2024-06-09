-- parser code inspired by this stackoverflow post: https://codereview.stackexchange.com/questions/253497/simple-haskell-parser
-- and this articled linked in said post: https://www.cs.nott.ac.uk/%7Epszgmh/monparsing.pdf
{-# LANGUAGE LambdaCase #-}

module Parser 
( Parser
, LuaExpr(parser)
, VarDefn
, TypeDefn
) where

import Control.Monad

import Data.Functor ((<&>))
import Control.Applicative
    ( Alternative(many), (<|>), empty, some, liftA2, liftA3 )
-- import Distribution.Compat.CharParsing (letter, CharParsing (string))
import Data.Char (isLetter, isDigit, isUpper, isAlphaNum, isAlpha)
import System.Directory.Internal.Prelude (getArgs)

type ErrorMsg = String

type ParserResult a = (Either ErrorMsg a, String)
newtype Parser a = Parser { runParser :: String -> ParserResult a}
  deriving (Functor)



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
eatCharFailMsg :: String -> Parser Char
eatCharFailMsg errMsg = Parser $ \case{
    "" -> (Left errMsg, "");
    ch:rest -> (Right ch, rest);
}
eatChar = eatCharFailMsg "Unexpected EOF"

satisfy :: Show a => (a -> Bool) -> Parser a -> Parser a
-- satisfy pred parser = do {a <- parser; guard (pred a); return a}
satisfy pred parser =
    parser >>= \a -> if pred a then pure a else fail (show a ++ " didnt satisfy a predicate")

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
-- stringParser' str = (mapM charParser str) <|> fail ("Failed to match " ++ str)
stringParser' str = (mapM charParser' str)

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
parensParser' p = charParser' '(' *> p <* charParser' ')'

bracketsParser :: Parser a -> Parser a
bracketsParser p = charParser '[' *> p <* charParser ']'
bracketsParser' :: Parser a -> Parser a
bracketsParser' p = charParser' '[' *> p <* charParser' ']'
cbracketsParser' :: Parser a -> Parser a
cbracketsParser' p = charParser' '{' *> p <* charParser' '}'

-- TODO: make parser ignore repeated ']'


matchInput :: String -> String -> String
matchInput = parseToString . stringParser


parseToString :: Show a => Parser a -> String -> String
parseToString p str = let (result, leftover) = runParser p str in
    (case result of
        Left err -> show "ERROR: " ++ err
        Right res -> "SUCCESS: " ++ show res
    ) ++ case leftover of
        [] -> ""
        _ -> ".\n\tunparsed: " ++ leftover



varNameParser :: Parser String
varNameParser = (:) <$> satisfyChar isLetter <*> many (satisfyChar (\ch -> isLetter ch || isDigit ch || ch == '_'))
-- varNameParser = (satisfyChar isLetter) >>= \str -> Parser $ runParser (stringParser "")
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

data VarDefn = VarDefn{vName:: String, vType:: Maybe TypeDefn, vDesc:: String} deriving (Show, Eq)


-- -- commentParser = tail <$> selector ' '
-- commentParser = tail <$> selector ' ' 
--     where
--         eater = eatCharFailMsg "Unterminated comment!"
--         selector :: Char -> Parser String
--         selector = \case{
--             ']' -> eater >>= \case{
--                 ']' -> pure "";
--                 ch -> (']':) . (ch:) <$> (eater >>= selector)
--             };
--             ch -> (ch:) <$> (eater >>= selector);
--         }
commentParser = stringParser "[[" *> commentParserHelper
    where
        commentParserHelper = many (satisfyChar (/= ']')) >>= (\str -> (str ++) <$> bracketEater)
        bracketEater = Parser $ \case{
            (']':']':rest) -> (Right "", rest);
            (']':rest)     -> runParser ((']':) <$> commentParserHelper) rest;
            s              -> (Left "Unterminated comment!", s);
        };
                                    --   f (']':']':rest) = (Right "", rest)
                                    --   f (']':rest) = runParser ((']':) <$> commentParserHelper) rest
                                    --   f s = (Left "Unterminated comment!", s)
        -- bracketEater = Parser f where 
        --                               f (']':']':rest) = (Right "", rest)
        --                               f (']':rest) = runParser ((']':) <$> commentParserHelper) rest
        --                               f s = (Left "Unterminated comment!", s)
        --     where
        --         f (']':']':rest) = (Right "", rest)
        --         f (']':rest) = runParser ((']':) <$> commentParserHelper) rest
        --         f s = (Left "Unterminated comment!", s)
        -- bracketEater = Parser f
        --     where
        --         f (']':']':rest) = (Right "", rest)
        --         f (']':rest) = runParser ((']':) <$> commentParserHelper) rest
        --         f s = (Left "Unterminated comment!", s)

            --     $ \case{
            -- ']':rest -> case rest of {
            --     [] -> (Left "Unterminated comment!", ""); -- there needs to be at least two characters
            --     ']':rest' -> (Right "", rest');
            --     _ -> runParser ((']':) <$> commentParserHelper) rest;
            -- };
            -- s -> (Left "did not find bracket!", s);



instance LuaExpr VarDefn where
    parser = VarDefn <$> varNameParser <*> typesParser <*> descParser
        where
        typesParser = optional' $ surroundBySpaces (charParser' ':') *> (pure <$> parser)
        descParser = optional' $ spaceParser *> commentParser


-- custom types must come in one of two forms:
-- 1) they start with a capital letter
-- 2) they start with any letter, and include a single "." at some point in the type name.
customTypeParser :: Parser String
customTypeParser = 
        (:) <$> satisfyChar isUpper <*> many (satisfyChar (\ch -> isAlphaNum ch || ch == '_'))
    <|> do tblName <- varNameParser <* charParser' '.'
           name <- varNameParser
           pure (tblName ++ ('.':name))
    -- do tblName <- varNameParser <* charParser' '.'
    --        name <- varNameParser
    --        pure (tblName ++ ('.':name))
data PrimitiveType = INT | BOOL | NUM | STRING | NIL deriving (Show, Eq)


instance LuaExpr PrimitiveType where
    parser = Parser $ \case{
        'i':'n':'t':rest -> case rest of
            'e':'g':'e':'r':rest' -> (Right INT, rest')
            _ -> (Right INT, rest)
        ;
        'b':'o':'o':'l':rest -> case rest of
            'e':'a':'n':rest' -> (Right BOOL, rest')
            _ -> (Right BOOL, rest)
        ;
        's':'t':'r':'i':'n':'g':rest -> (Right STRING, rest);
        'n':'i':'l':rest -> (Right NIL, rest);
        'n':'u':'m':rest -> (Right NUM, rest);
        rest -> (Left "Could not parse primitive", rest)
    }

data TypeDefn = Primitive PrimitiveType
    | NonPrimitive String
    | Function {generics :: [String], args :: [VarDefn], rets :: [VarDefn], defns :: [VarDefn]}
    | Multi [TypeDefn]
    | Array [TypeDefn]
    | Table {keys :: TypeDefn, vals :: TypeDefn}
    | Dict [VarDefn]
    deriving (Show, Eq)

instance LuaExpr TypeDefn where

    parser = Primitive <$> parser
         <|> NonPrimitive <$> customTypeParser
         <|> functionParser
         <|> Multi <$> parensParser' multiTypesParser
         <|> Array <$> bracketsParser' multiTypesParser
         <|> tableParser
         <|> dictParser
        where
            -- function parsing 
            functionParser = Function <$> genericsParser <*> argsParser <*> retsParser <*> defnsParser
                where
                    genericsParser = optional' (charParser '<' *> genericsNameParser <* charParser '>')
                        where genericsNameParser = some (surroundBySpaces customTypeParser <* commaParser)

                    argsParser = surroundBySpaces $
                                (parensParser' . many . surroundBySpaces) (optionalIgnore parser commaParser)
                                <|> fmap pure parser
                    -- requires naming return parameters
                    -- TODO: remove restriction on `retsParsers` that requires return parameters to be named
                    retsParser = surroundBySpaces (stringParser' "->") *> argsParser
                    defnsParser = optional' $ surroundBySpaces (stringParser' "where") *> many parser

            tableParser = cbracketsParser' (Table <$> parser <*> (commaParser *> parser))
            multiTypesParser = surroundBySpaces . some $ optionalIgnore (parser :: Parser TypeDefn)  (surroundBySpaces (stringParser' "|"))
            dictParser = cbracketsParser' (Dict <$> surroundBySpaces (some (parser <* commaParser)))


main = do
    -- (putStr "Digits: " >> getLine) >>= putStrLn . parseToString intsParser
    -- (putStr "Digits: " >> getLine) >>= putStrLn . parseToString intsExprParser
    -- (putStr "Type: " >> getLine) >>= putStrLn . parseToString TypeDefnParser
    -- args <- getArgs
    -- putStrLn . parseToString TypeDefnParser $ head args
    -- getArgs >>= putStrLn . parseToString funcRetExprParser . head
    -- getArgs >>= putStrLn . parseToString (commentParser) . head
    -- getArgs >>= putStrLn . parseToString (commentParser) . head
    getArgs >>= putStrLn . parseToString (parser:: Parser VarDefn) . head
    getArgs >>= putStrLn . parseToString (parser:: Parser TypeDefn) . head
    -- getArgs >>= putStrLn . parseToString TypeDefnParser . head

    -- matchInput <$> (putStr "Pattern: " >> getLine) <*> (putStr "Input: " >> getLine)
    --    >>= putStrLn