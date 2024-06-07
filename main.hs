import Data.List (intercalate)
import Language.Haskell.TH.Ppr (ppr_typedef)
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

intercalateMap :: [a1] -> (a2 -> [a1]) -> [a2] -> [a1]
intercalateMap str fn = intercalate str . map fn

fmtTypes :: [LuaType] -> String
fmtTypes [] = "unknown"
fmtTypes types = intercalateMap "|" writeLua types

-- any piece of data that should be written to docs
class LuaData a where
    name :: a -> String
    description :: a -> Maybe String
    description = desc
    desc :: a -> Maybe String
    desc = description
    writeLua :: a -> String
    writeLua = writeLua
    writeMarkdown :: a -> String
    getGenerics :: a -> [LuaType]



-- a function, integer, string, etc
data LuaType = STRING |
    INT |
    NUMBER |
    BOOL |
    NIL |
    GENERIC String |
    CUSTOM String |
    LuaFunction Parameters ReturnValues |
    LuaArray [LuaType] |
    LuaTable {keyTypes :: [LuaType], valTypes :: [LuaType]}



instance LuaData LuaType where
    name (LuaFunction args rets) = writeLua $ LuaFunction args rets
    name STRING = "string"
    name INT = "integer"
    name NUMBER = "number"
    name BOOL = "boolean"
    name NIL = "nil"
    name (LuaArray []) = "any[]"
    name (LuaArray [x]) = name x ++ "[]"
    name (LuaArray xs) = '(': intercalateMap "|" writeLua xs ++ ")[]"
    name (LuaTable {keyTypes=keyTypes, valTypes=valTypes}) =
        "table<" ++ fmtTypes keyTypes ++ ',':fmtTypes valTypes ++ ">"
    name (CUSTOM str) = str
    name (GENERIC str) = str

    writeLua (LuaFunction pars []) = "fun(" ++ intercalateMap ", " writeLua pars ++ ")"
    writeLua (LuaFunction pars rets) = "(fun(" ++ fmttedPars ++ "):" ++ fmttedRets ++ ")"
        where
            fmttedPars = intercalateMap ", " writeLua pars
            fmttedRets = intercalateMap ", " (intercalateMap "|" writeLua . types) rets
    writeLua x = name x

    getGenerics :: LuaType -> [LuaType]
    getGenerics gen@(GENERIC _) = [gen]
    getGenerics (LuaFunction pars rets) = concatMap (concatMap getGenerics) [pars, rets]
    getGenerics _ = []

    writeMarkdown _ = ""

-- a parameter for a function/method/class. this is a name, description, and list of supported types.
-- types can themselves be functions with nested parameters

data LuaArg = FuncParameter Name Description [LuaType]
types :: LuaArg -> [LuaType]
types (FuncParameter _ _ ptypes) = ptypes

instance LuaData LuaArg where
    name (FuncParameter pname _ _ ) = pname
    desc (FuncParameter _ pdesc _ ) = pdesc

    writeLua (FuncParameter name desc []) = name ++ maybe "" (' ':) desc
    writeLua (FuncParameter name desc types) =  name ++ ": " ++  fmtTypes types ++ maybe "" (' ':) desc
    -- writeLua (LuaArg name desc types) 

    getGenerics = concatMap getGenerics . types
    writeMarkdown :: LuaArg -> String
    writeMarkdown =  const ""

data FuncDef = FuncDef Name Description LuaType

instance LuaData FuncDef where
    name (FuncDef fname _ _) = fname
    desc (FuncDef _ fdesc _) = fdesc

    getGenerics (FuncDef _ _ fn) = getGenerics fn

    writeLua (FuncDef fname fdesc fn@(LuaFunction pars rets)) =
        funcDesc -- function description
        ++ genericStr -- generic labels
        ++ concatMap writePar pars -- formatted parameters
        ++ concatMap writeRet rets -- formatted return values
        ++ "function " ++ fname ++ "(" ++ intercalateMap ", " name pars ++ ") end"
        where
            funcDesc = getStr $ (\str -> "--[[ " ++ str ++ "]]\n") <$> fdesc
            genericStr = case getGenerics fn of
                [] -> ""
                generics -> "---@generic " ++ intercalateMap ", " name generics ++ "\n"
            writePar :: LuaArg -> String
            writePar (FuncParameter pname pdesc ptypes) = unwords ["---@param", pname, fmtTypes ptypes, getStr pdesc, "\n"]
            writeRet :: LuaArg -> String
            writeRet (FuncParameter pname pdesc ptypes) = unwords ["---@return", fmtTypes ptypes, pname, getStr pdesc, "\n"]


    writeMarkdown _ = ""


data TableParams = TableParams Name Description [LuaArg]
params (TableParams _ _ pars ) = pars
instance LuaData TableParams where
    name (TableParams pname _ _ ) = pname
    desc (TableParams _ pdesc _ ) = pdesc
    writeLua (TableParams tname tdesc tpars) =
        "--[[" ++ getStr tdesc ++ "]]\n"
        ++ "---@class " ++ tname ++ "\n"
        ++ concatMap writeInline tpars
        where
            writeInline (FuncParameter name desc types) = 
               "---@field " ++ name ++ fmtTypes types ++ maybe "" (' ':) desc ++ "\n"

getMethod :: String -> FuncDef -> FuncDef
getMethod clsName (FuncDef fname fdesc func) = FuncDef (clsName ++ ':':fname) fdesc func

getAPIFunc apiName (FuncDef fname fdesc func) = FuncDef (apiName ++ '.':fname) fdesc func


writeLuaClassHeader :: Name -> Description -> [String] -> [LuaArg] -> [FuncDef] -> Bool -> String
writeLuaClassHeader cname cdesc parentNames fields methods isLocal =
    -- description (if it exists)
    maybe "" (\x ->"--[[" ++ x ++ "]]\n") cdesc
    -- class name and parents
    ++ "---@class " ++ cname ++ ifNonEmpty ((": "++) . intercalate ", ") parentNames ++ "\n"
    -- fields
    ++ concatMap (\f -> "---@field" ++ writeLua f ++ "\n") fields
    -- lua code
    ++ (if isLocal then "local " else "") ++ cname ++ " = {}\n\n"
    -- methods
    ++ intercalateMap "\n\n" (writeLua . getMethod cname) methods

data LuaClass = LuaClass {cname :: String, cdesc :: Description, parents :: [LuaClass], fields :: [LuaArg], methods :: [FuncDef]}

getMethodName :: LuaClass -> String -> String
getMethodName cls = ((name cls ++ ":") ++)
writeMethod :: LuaClass -> FuncDef -> String
writeMethod cls (FuncDef fname fdesc func) = writeLua (FuncDef (getMethodName cls fname) fdesc func) ++ "\n\n"

instance LuaData LuaClass where
    name = cname
    desc = cdesc

    writeLua (LuaClass {cname=cname, cdesc=cdesc, parents=parents, fields=fields, methods=methods}) =
        writeLuaClassHeader cname cdesc (name <$> parents) fields methods True

    writeMarkdown _ = ""
    getGenerics LuaClass{fields=fields,methods=methods} = concatMap getGenerics methods ++ concatMap getGenerics fields

data LuaAPI = LuaAPI {aname :: String, adesc :: Description, functions :: [FuncDef], constants :: [LuaArg]}


instance LuaData LuaAPI where
    name = aname
    desc = adesc

    writeLua (LuaAPI {aname=cname, adesc=cdesc, functions=methods, constants=fields}) =
        -- LuaAutoGenHeader 
        writeLuaClassHeader cname cdesc [] fields [] False
        ++ intercalateMap "\n\n" (writeLua . getAPIFunc cname) methods




main = do {
    let
        param1 = FuncParameter "a" (Just "help me") [STRING, INT]
        param2 = FuncParameter "b" (Just "broccoli") [NUMBER, BOOL]
        ret1 = FuncParameter "a" (Just "help me") [STRING, INT]
        ret2 = FuncParameter "b" (Just "broccoli") [NUMBER, BOOL]
        f1 = LuaFunction [param1, param2] [ret1, ret2]
        insertdef = FuncDef "insert" (Just "desc") (LuaFunction [param1, param2] [ret1, ret2])
        cls1 = LuaClass {cname="Test_Class", cdesc=Just "Test Class Description", parents=[],
            fields=[
                FuncParameter "a" (Just "help me") [STRING, INT],
                FuncParameter  "b" (Just "broccoli") [NUMBER, BOOL]
            ],
            methods=[
                FuncDef "method_1" (Just "desc 1") (LuaFunction
                    [param1, param2, FuncParameter "param3" (Just "Paramter 3 description") [GENERIC "K", CUSTOM "Lua.mcm.DecimalSlider"]]
                    [ret1, ret2])
            ]
        }
        tableapi = LuaAPI {aname="table", adesc=Just "Extends LUA table Api",
            constants=[
                FuncParameter "size" (Just "size of table") [INT],
                FuncParameter "name" (Just "name") [STRING]
                ],
            functions=[
                insertdef
                ]
        }
    in
        do
            putStrLn $ writeLua param1;
            putStrLn $ name param2;
            putStrLn $ name f1;
            putStrLn $ writeLua f1;
            putStrLn $ writeLua insertdef;
            putStrLn $ writeLua cls1;
            putStrLn $ writeLua tableapi
            -- putStrLn $ writeLua f1;
}




