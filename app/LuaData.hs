{-# LANGUAGE LambdaCase #-}

module LuaData 
( LuaData(writeLua, writeMarkdown, getGenerics)
, LuaType
, LuaArg
, FuncDef
, TableParams
) where
import Data.List (intercalate)
-- import Language.Haskell.TH.Ppr (ppr_typedef)
import qualified Data.Maybe


type Name = String
type Description = Maybe String
type Parameters = [LuaArg]
type ReturnValues = [LuaArg]

ifNonEmpty :: ([a] -> String) -> [a] ->  String
ifNonEmpty _ [] = ""
ifNonEmpty f ls = f ls

getStr :: Maybe String -> String
getStr = Data.Maybe.fromMaybe ""

fmtTypes :: [LuaType] -> String
fmtTypes [] = "unknown"
fmtTypes [t] = writeLua t
fmtTypes ts = '(' : intercalate "|" (writeLua <$> ts) ++ ")"

-- any piece of data that should be written to docs
class LuaData a where
    writeLua :: a -> String
    writeLua = writeLua
    writeMarkdown :: a -> String
    getGenerics :: a -> [LuaType]


-- a function, integer, string, etc
data LuaType = Primitive String |
    Generic String |
    Custom String |
    LuaFunction{pars::Parameters, rets::ReturnValues} |
    LuaArray [LuaType] |
    LuaTable {keyTypes :: [LuaType], valTypes :: [LuaType]}
    deriving(Show, Eq)
pSTRING = Primitive "string"
pINT = Primitive "integer"
pBOOL = Primitive "boolean"
pNUM = Primitive "number"
pNIL = Primitive "nil"



instance LuaData LuaType where

    writeLua LuaFunction{pars, rets} = case rets of
        [] -> "fun(" ++ fmttedPars ++ ")"
        _  -> "(fun(" ++ fmttedPars ++ "):" ++ fmttedRets ++ ")"
        where
            fmttedPars = intercalate ", " (writeLua <$> pars)
            fmttedRets = intercalate ", " ((\ret -> fmtTypes  (ptypes ret) ++ pname ret) <$> rets)

    writeLua (LuaArray xs) = case xs of
        [] -> "any[]"
        [x] -> writeLua x ++ "[]"
        _ -> '(': intercalate "|" (writeLua <$> xs) ++ ")[]"
    writeLua LuaTable{keyTypes, valTypes} = "table<" ++ fmtTypes keyTypes ++ ',':fmtTypes valTypes ++ ">"
    writeLua (Generic str) = str
    writeLua (Custom str) = str
    writeLua (Primitive str) = str

    getGenerics :: LuaType -> [LuaType]
    getGenerics = \case{
        (LuaFunction pars rets) -> concatMap (concatMap getGenerics) [pars, rets];
        (LuaTable keys vals) -> concatMap (concatMap getGenerics) [keys, vals];
        (LuaArray types) -> concatMap getGenerics types;
        gen@(Generic _) -> [gen];
        _ -> []
    }

    writeMarkdown _ = ""

-- a parameter for a function/method/class. this is a name, description, and list of supported types.
-- types can themselves be functions with nested parameters

data LuaArg = FuncParameter{pname::Name, pdesc::Description, ptypes:: [LuaType]}
    deriving (Show, Eq)

instance LuaData LuaArg where

    writeLua FuncParameter{pname, pdesc, ptypes} = pname ++ retTypesStr ++ maybe "" (' ':) pdesc
        where retTypesStr = ((": "++) . fmtTypes) `ifNonEmpty` ptypes
    -- writeLua (LuaArg name desc types) 

    getGenerics = concatMap getGenerics . ptypes
    writeMarkdown :: LuaArg -> String
    writeMarkdown =  const ""

data FuncDef = FuncDef{fname :: Name, fdesc :: Description, fn :: LuaType }
    deriving (Show, Eq)
instance LuaData FuncDef where
    getGenerics FuncDef{fn} = getGenerics fn

    writeLua (FuncDef fname fdesc fn@(LuaFunction pars rets)) =
        funcDesc -- function description
        ++ genericStr -- generic labels
        ++ concatMap writePar pars -- formatted parameters
        ++ concatMap writeRet rets -- formatted return values
        ++ "function " ++ fname ++ "(" ++ intercalate ", " (writeLua <$> pars) ++ ") end"
        where
            funcDesc = getStr $ (\str -> "--[[ " ++ str ++ "]]\n") <$> fdesc
            genericStr = case getGenerics fn of
                [] -> ""
                generics -> "---@generic " ++ intercalate ", " (writeLua <$> generics) ++ "\n"
            writePar :: LuaArg -> String
            writePar (FuncParameter pname pdesc ptypes) = unwords ["---@param", pname, fmtTypes ptypes, getStr pdesc, "\n"]
            writeRet :: LuaArg -> String
            writeRet (FuncParameter pname pdesc ptypes) = unwords ["---@return", fmtTypes ptypes, pname, getStr pdesc, "\n"]


    writeMarkdown _ = ""


data TableParams = TableParams Name Description [LuaArg]
    deriving (Show, Eq)
params (TableParams _ _ pars ) = pars
instance LuaData TableParams where
    writeLua (TableParams tname tdesc tpars) =
        "--[[" ++ getStr tdesc ++ "]]\n"
        ++ "---@class " ++ tname ++ "\n"
        ++ concatMap writeInline tpars
        where
            writeInline FuncParameter{pname, pdesc, ptypes} = concat ["---@field ", pname, fmtTypes ptypes, maybe "" (' ':) pdesc,  "\n"]

getMethod :: String -> FuncDef -> FuncDef
-- getMethod clsName (FuncDef fname fdesc func) = FuncDef (clsName ++ ':':fname) fdesc func
getMethod clsName fdef = fdef {fname=clsName ++ ':':fname fdef}

getAPIFunc apiName fdef = fdef {fname=apiName ++ '.':fname fdef}


writeLuaClassHeader cls@LuaClass{cname=clsName, cdesc,fields, methods} isLocal = 
    maybe "" (\x ->"--[[" ++ x ++ "]]\n") cdesc
    -- class name and parents
    ++ header
    -- fields
    ++ concatMap (\f -> "---@field" ++ writeLua f ++ "\n") fields
    -- lua code
    ++ (if isLocal then "local " else "") ++ clsName ++ " = {}\n\n"
    -- methods
    ++ intercalate "\n\n" (writeLua . getMethod clsName <$> methods)
    where
        header = case cname <$> parents cls of 
            [] -> "---@class " ++ clsName ++ "\n"
            parentNames -> "---@class " ++ clsName ++ ": " ++ intercalate ", " parentNames ++ "\n"

-- writeLuaClassHeader :: Name -> Description -> [String] -> [LuaArg] -> [FuncDef] -> Bool -> String
-- writeLuaClassHeader cname cdesc parentNames fields methods isLocal =
--     -- description (if it exists)
--     maybe "" (\x ->"--[[" ++ x ++ "]]\n") cdesc
--     -- class name and parents
--     ++ "---@class " ++ cname ++ ((": "++) . intercalate ", ") `ifNonEmpty` parentNames ++ "\n"
--     -- fields
--     ++ concatMap (\f -> "---@field" ++ writeLua f ++ "\n") fields
--     -- lua code
--     ++ (if isLocal then "local " else "") ++ cname ++ " = {}\n\n"
--     -- methods
--     ++ intercalate "\n\n" (writeLua . getMethod cname <$> methods)

data LuaClass = LuaClass {cname :: String, cdesc :: Description, parents :: [LuaClass], fields :: [LuaArg], methods :: [FuncDef]}
    deriving (Show, Eq)
getMethodName :: LuaClass -> String -> String
getMethodName cls = ((cname cls ++ ":") ++)
writeMethod :: LuaClass -> FuncDef -> String
-- writeMethod cls (FuncDef fname fdesc func) = writeLua (FuncDef (getMethodName cls fname) fdesc func) ++ "\n\n"
writeMethod cls fdef = writeLua fdef{fname=getMethodName cls (fname fdef)} ++ "\n\n"

instance LuaData LuaClass where
    writeLua = flip writeLuaClassHeader True

    writeMarkdown _ = ""
    getGenerics LuaClass{fields,methods} = concatMap getGenerics methods ++ concatMap getGenerics fields

data LuaAPI = LuaAPI {aname :: String, adesc :: Description, functions :: [FuncDef], constants :: [LuaArg]}
    deriving (Show, Eq)

instance LuaData LuaAPI where

    writeLua LuaAPI {aname, adesc, functions, constants} =
        -- LuaAutoGenHeader 
        writeLuaClassHeader LuaClass{cname=aname, cdesc=adesc, methods=[], fields=constants, parents=[]} False
        ++ intercalate "\n\n" (writeLua . getAPIFunc aname <$> functions)




main = do {
    let
        param1 = FuncParameter "a" (Just "help me") [pSTRING, pINT]
        param2 = FuncParameter "b" (Just "broccoli") [pNUM, pBOOL]
        ret1 = FuncParameter "a" (Just "help me") [pSTRING, pINT]
        ret2 = FuncParameter "b" (Just "broccoli") [pNUM, pBOOL]
        f1 = LuaFunction [param1, param2] [ret1, ret2]
        insertdef = FuncDef "insert" (Just "desc") (LuaFunction [param1, param2] [ret1, ret2])
        cls1 = LuaClass {cname="Test_Class", cdesc=Just "Test Class Description", parents=[],
            fields=[
                FuncParameter "a" (Just "help me") [pSTRING, pINT],
                FuncParameter  "b" (Just "broccoli") [pNUM, pBOOL]
            ],
            methods=[
                FuncDef "method_1" (Just "desc 1") (LuaFunction
                    [param1, param2, FuncParameter "param3" (Just "Paramter 3 description") [Generic "K", Custom "Lua.mcm.DecimalSlider"]]
                    [ret1, ret2])
            ]
        }
        tableapi = LuaAPI {aname="table", adesc=Just "Extends LUA table Api",
            constants=[
                FuncParameter "size" (Just "size of table") [pINT],
                FuncParameter "name" (Just "name") [pSTRING]
                ],
            functions=[
                insertdef
                ]
        }
    in
        do
            putStrLn $ writeLua param1;
            -- putStrLn $ name param2;
            -- putStrLn $ name f1;
            putStrLn $ writeLua f1;
            putStrLn $ writeLua insertdef;
            putStrLn $ writeLua cls1;
            putStrLn $ writeLua tableapi
            -- putStrLn $ writeLua f1;
}




