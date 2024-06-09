{-# LANGUAGE LambdaCase #-}
module LuaData
( LuaData(writeLua, writeMarkdown, getGenerics)
, PrimitiveType(STRING, INT, BOOL, NUM, NIL)
, LuaType(Generic, Custom, Function, Array, Table, Primitive, Dict, Multi, Class, pars, rets, keys, vals)
, LuaVar(name, desc, vtype, LuaVar, LuaField, LuaFuncArg, LuaFuncRet)
-- , FuncDef
) where
import Data.List (intercalate)
-- import Language.Haskell.TH.Ppr (ppr_typedef)
import Control.Applicative (liftA3)
-- import qualified Data.Set as Set
import qualified Data.Set as Set

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr g []
    where g v acc = maybe acc (:acc) (f v)
    -- where g v acc = case f v of 
    --         Nothing -> acc
    --         Just v' -> v': acc

-- any piece of data that should be written to docs
class LuaData a where
    writeLua :: a -> String
    writeLua = writeLuaInline
    writeLuaInline :: a -> String
    writeLuaInline = writeLua
    writeMarkdown :: a -> String
    writeMarkdown = writeMarkdownInline
    writeMarkdownInline :: a -> String
    writeMarkdownInline = writeMarkdown
    getGenerics :: a -> [LuaType]
    getGenerics = pure []


data PrimitiveType = STRING | INT | BOOL | NUM | NIL deriving (Show, Eq, Ord)

instance LuaData PrimitiveType where
    writeLua STRING = "string"
    writeLua INT = "integer"
    writeLua BOOL = "boolean"
    writeLua NUM = "number"
    writeLua NIL = "nil"

-- a function, integer, string, etc
data LuaType = Primitive PrimitiveType
    | Generic String
    | Custom String
    | Multi [LuaType]
    | Function{pars::[LuaVar], rets::[LuaVar]}
    | Array [LuaType]
    | Table {keys :: LuaType, vals :: LuaType}
    | Dict [LuaVar]
    | Class LuaClassDefn
    deriving(Show, Eq, Ord)


fmtVars :: LuaData a => [a] -> [Char]
fmtVars vars = intercalate ", " (map writeLuaInline vars)

instance LuaData LuaType where
    writeLuaInline Function{pars, rets} = case rets of
        [] -> "fun(" ++ fmtVars pars ++ ")"
        _  -> "(fun(" ++ fmtVars pars ++ "):" ++ fmtVars rets ++ ")"
    writeLuaInline (Multi xs) = case xs of
        [] -> "any"
        [x] -> writeLuaInline x
        _ -> '(': intercalate "|" (writeLuaInline <$> xs) ++ ")"

    writeLuaInline (Array xs) = writeLuaInline (Multi xs) ++ "[]"

    writeLuaInline Table{keys, vals} = "table<" ++ writeLuaInline keys ++ ',':writeLuaInline vals ++ ">"
    writeLuaInline (Generic str) = str
    writeLuaInline (Custom str) = str
    writeLuaInline (Primitive p) = writeLuaInline p
    writeLuaInline (Dict vars) = '{': fmtVars vars ++"}"
    writeLuaInline (Class cls) = writeLuaInline cls


    getGenerics :: LuaType -> [LuaType]
    getGenerics tp = case tp of
        (Function pars rets) -> concatMap concatGens [pars, rets]
        (Table keys vals) -> concatMap getGenerics [keys, vals]
        (Array types) -> concatGens types
        (Multi types) -> concatGens types
        (Generic _) -> [tp]
        (Dict vars) -> concatGens vars
        _ -> []
        where
            concatGens :: LuaData a => [a] -> [LuaType]
            concatGens = concatMap getGenerics
    -- getGenerics = \case{
    --     (Function pars rets) -> concatMap (concatMap getGenerics) [pars, rets];
    --     (Table keys vals) -> concatMap (concatMap getGenerics) [keys, vals];
    --     (Array types) -> concatMap getGenerics types;
    --     gen@(Generic _) -> [gen];
    --     _ -> []
    -- }


    writeMarkdown _ = ""



data LuaVar = LuaVar {name:: String, desc:: String, vtype:: Maybe LuaType}
    | LuaFuncArg {name:: String, desc:: String, vtype:: Maybe LuaType}
    | LuaFuncRet {name:: String, desc:: String, vtype:: Maybe LuaType}
    | LuaField {name:: String, desc:: String, vtype:: Maybe LuaType}
    deriving (Show, Eq, Ord)

-- writeType 
instance LuaData LuaVar where
    getGenerics  = maybe [] (Set.toList . Set.fromList . getGenerics ) . vtype
    writeLua (LuaFuncArg name desc vtype) = unwords ["---@param", name, maybe "" writeLua vtype, desc, "\n"]
    writeLua (LuaField name desc vtype) = unwords ["---@field", name, maybe "" writeLua vtype, desc, "\n"]
    writeLua (LuaFuncRet name desc vtype) = unwords ["---@return", maybe "" writeLua vtype, name, desc, "\n"]

    writeLua var = case vtype var of
        (Just (Dict vars)) ->  descStr
            ++ "---@class " ++ name var ++ "\n"
            -- ++ concatMap (writeLua . (\v -> LuaField (name v) (desc v) (vtype v))) vars
            ++ concatMap (writeLua . liftA3 LuaField name desc vtype) vars
        (Just fn@(Function{pars, rets})) ->
            paramsStr ++ descStr
            ++ case getGenerics fn of
                [] -> ""
                generics -> "---@generic " ++ intercalate ", " (writeLua <$> generics) ++ "\n"
            ++ concatMap writeLua pars' -- formatted parameters
            ++ concatMap writeLua rets' -- formatted return values
            ++ funcHeader ++ name var ++ "(" ++ intercalate ", " (map name pars) ++ ") end"
            where
                funcHeader = if '.' `elem` name var then "function " else "local function "
                -- paramsStr = if null tableParams then "" else concatMap ((++"\n\n") . writeLua) tableParams
                convertToClass v = case vtype v of
                        Just (Dict vals) -> v{vtype=Just (Class (LuaClassDefn{methods=[], parents=[], cdesc=desc v,
                                cname=name var ++ "." ++ name v,
                                fields=liftA3 LuaField name desc vtype <$> vals
                            }))}
                        _ -> v
                pars' = convertToClass <$> pars
                rets' = convertToClass <$> rets
                writer x = case vtype x of
                    Just (Class cls) -> writeLua cls ++ "\n"
                    _ -> ""
                paramsStr = concatMap writer pars' ++ concatMap writer rets'
                    -- where
                    --     tableParams = filterMap f pars ++ filterMap f rets
                    --     f v = case vtype v of
                    --         Just (Dict vals) -> Just LuaClassDefn{methods=[], parents=[], cdesc=desc v,
                    --                 cname=name var ++ "." ++ name v,
                    --                 fields=liftA3 LuaField name desc vtype <$> vals
                    --             }
                    --         _ -> Nothing
        _ -> writeLuaInline var
        where
            descStr = if null (desc var) then "" else  "--[[ " ++ desc var ++ "]]\n"

    -- writeLua (LuaVar dname ddesc (Just (Dict vars))) =
    --     if null ddesc then "" else  "--[[ " ++ ddesc ++ "]]\n"
    --     ++ "---@class " ++ dname ++ "\n"
    --     ++ concatMap (writeLua . (\v -> LuaField (name v) (desc v) (vtype v))) vars
    -- writeLua (LuaField )
    -- writeLua (LuaVar fname desc (Just fn@(Function{pars, rets}))) =
    --     if null desc then "" else  "--[[ " ++ desc ++ "]]\n"
    --     ++ case getGenerics fn of
    --         [] -> ""
    --         generics -> "---@generic " ++ intercalate ", " (writeLua <$> generics) ++ "\n"
    --     ++ concatMap writeLua pars -- formatted parameters
    --     ++ concatMap writeLua rets -- formatted return values
    --     ++ funcHeader ++ fname ++ "(" ++ intercalate ", " (map name pars) ++ ") end"
    --     where
    --         funcHeader = if '.' `elem` fname then "function " else "local function "

    -- writeLua _ = ""

    writeLuaInline var@LuaFuncRet{} = vname ++ maybe "unknown" writeLuaInline (vtype var)
        where vname = if null (name var) then "" else name var ++ ": "

    writeLuaInline var = name var ++ maybe "" ((": "++) . writeLuaInline) (vtype var)



data LuaClassDefn = LuaClassDefn{
    cname:: String,
    cdesc:: String,
    parents:: [LuaClassDefn],
    fields:: [LuaVar],
    methods:: [LuaVar]
} deriving (Show, Eq, Ord)
instance LuaData LuaClassDefn where
    writeLuaInline = cname
    writeLua cls@LuaClassDefn{parents} =
        (if null (cdesc cls) then "" else  "--[[ " ++ cdesc cls ++ "]]\n")
        ++ "---@class " ++ cname cls ++ parentsList ++ "\n"
        ++ concatMap writeLua (fields cls)
        ++ case methods cls of
            [] -> ""
            ms -> if '.' `elem` cname cls then "" else "local " ++ cname cls ++ " = {}\n\n"
                ++ intercalate "\n\n" [writeLua m{name=cname cls ++ "." ++ name m} | m <- ms]
        where
            parentsList = if null parents then "" else ": " ++ intercalate ", " (map cname parents)

    -- writeLuaInline (LuaVar name _ vtype) = case vtype of 

    -- writeLua _ = ""
-- a parameter for a function/method/class. this is a name, description, and list of supported types.
-- types can themselves be functions with nested parameters



-- getMethod :: String -> FuncDef -> FuncDef
-- -- getMethod clsName (FuncDef fname fdesc func) = FuncDef (clsName ++ ':':fname) fdesc func
-- getMethod clsName fdef = fdef {fname=clsName ++ ':':fname fdef}

-- getAPIFunc apiName fdef = fdef {fname=apiName ++ '.':fname fdef}


-- writeLuaClassHeader cls@LuaClass{cname=clsName, cdesc,fields, methods} isLocal =
--     maybe "" (\x ->"--[[" ++ x ++ "]]\n") cdesc
--     -- class name and parents
--     ++ header
--     -- fields
--     ++ concatMap (\f -> "---@field" ++ writeLua f ++ "\n") fields
--     -- lua code
--     ++ (if isLocal then "local " else "") ++ clsName ++ " = {}\n\n"
--     -- methods
--     ++ intercalate "\n\n" (writeLua . getMethod clsName <$> methods)
--     where
--         header = case cname <$> parents cls of
--             [] -> "---@class " ++ clsName ++ "\n"
--             parentNames -> "---@class " ++ clsName ++ ": " ++ intercalate ", " parentNames ++ "\n"

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

-- data LuaClass = LuaClass {cname :: String, cdesc :: Description, parents :: [LuaClass], fields :: [LuaArg], methods :: [FuncDef]}
--     deriving (Show, Eq)
-- getMethodName :: LuaClass -> String -> String
-- getMethodName cls = ((cname cls ++ ":") ++)
-- -- writeMethod :: LuaClass -> FuncDef -> String
-- -- -- writeMethod cls (FuncDef fname fdesc func) = writeLua (FuncDef (getMethodName cls fname) fdesc func) ++ "\n\n"
-- -- writeMethod cls fdef = writeLua fdef{fname=getMethodName cls (fname fdef)} ++ "\n\n"

-- instance LuaData LuaClass where
--     writeLua = flip writeLuaClassHeader True

--     writeMarkdown _ = ""
--     getGenerics LuaClass{fields,methods} = concatMap getGenerics methods ++ concatMap getGenerics fields

data LuaAPI = LuaAPI {aname :: String, adesc :: String, functions :: [LuaVar], constants :: [LuaVar]}
    deriving (Show, Eq)

instance LuaData LuaAPI where

    writeLua LuaAPI {aname, adesc, functions, constants} =
        -- LuaAutoGenHeader 
        writeLua LuaClassDefn{cname=aname, cdesc=adesc, methods=functions, fields=constants, parents=[]}




-- main = do {
--     let
--         param1 = FuncParameter "a" (Just "help me") [pSTRING, pINT]
--         param2 = FuncParameter "b" (Just "broccoli") [pNUM, pBOOL]
--         ret1 = FuncParameter "a" (Just "help me") [pSTRING, pINT]
--         ret2 = FuncParameter "b" (Just "broccoli") [pNUM, pBOOL]
--         f1 = LuaFunction [param1, param2] [ret1, ret2]
--         insertdef = FuncDef "insert" (Just "desc") (LuaFunction [param1, param2] [ret1, ret2])
--         cls1 = LuaClass {cname="Test_Class", cdesc=Just "Test Class Description", parents=[],
--             fields=[
--                 FuncParameter "a" (Just "help me") [pSTRING, pINT],
--                 FuncParameter  "b" (Just "broccoli") [pNUM, pBOOL]
--             ],
--             methods=[
--                 FuncDef "method_1" (Just "desc 1") (LuaFunction
--                     [param1, param2, FuncParameter "param3" (Just "Paramter 3 description") [Generic "K", Custom "Lua.mcm.DecimalSlider"]]
--                     [ret1, ret2])
--             ]
--         }
--         tableapi = LuaAPI {aname="table", adesc=Just "Extends LUA table Api",
--             constants=[
--                 FuncParameter "size" (Just "size of table") [pINT],
--                 FuncParameter "name" (Just "name") [pSTRING]
--                 ],
--             functions=[
--                 insertdef
--                 ]
--         }
--     in
--         do
--             putStrLn $ writeLua param1;
--             -- putStrLn $ name param2;
--             -- putStrLn $ name f1;
--             putStrLn $ writeLua f1;
--             putStrLn $ writeLua insertdef;
--             putStrLn $ writeLua cls1;
--             putStrLn $ writeLua tableapi
--             -- putStrLn $ writeLua f1;
-- }




