module Main where
-- import qualified Data.Maybe

import LuaData
-- import qualified LuaData (LuaType(Array), LuaVar)
import Parser
import Control.Applicative (liftA3, (<|>))
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(..))
import GHC.IO.Handle (hGetContents)
import Data.List (elemIndex)
import Data.Functor ((<&>))
import qualified Data.Maybe as Maybe


decodeHelper :: [String] -> Parser.TypeDefn -> LuaData.LuaType
decodeHelper gens td = case td of
    (Parser.Primitive p) -> case p of
        Parser.STRING -> LuaData.Primitive LuaData.STRING
        Parser.NUM -> LuaData.Primitive LuaData.NUM
        Parser.NIL -> LuaData.Primitive LuaData.NIL
        Parser.BOOL -> LuaData.Primitive LuaData.BOOL
        Parser.INT ->LuaData.Primitive  LuaData.INT
    Parser.NonPrimitive n -> if n `elem` gens
                             then LuaData.Generic n
                             else LuaData.Custom n
    Parser.Array types -> LuaData.Array $ decodeHelper gens <$> types
    Parser.Dict vars -> LuaData.Dict $ decodeVar gens <$> vars
    Parser.Multi vars -> LuaData.Multi $ decodeHelper gens <$> vars
    Parser.Table keys vals -> LuaData.Table (decodeHelper gens keys) (decodeHelper gens vals)
    Parser.Function generics args rets defns ->
        LuaData.Function{
            -- generics=LuaData.Generic <$> generics,
            -- LuaData.pars=args <&> (liftA3 LuaData.LuaFuncArg Parser.vName getDesc (fmap decodeTypeDefn . getType)),
            LuaData.pars=args <&> decodeArgs LuaData.LuaFuncArg,
            LuaData.rets=rets <&> decodeArgs LuaData.LuaFuncRet
            -- LuaData.rets=map (liftA3 LuaData.LuaFuncRet Parser.vName getDesc (fmap decodeTypeDefn . getType)) rets
        }
        where
            -- decodeArgs cons arg = cons
            --     (Parser.vName arg)
            --     (getDesc arg)
            --     (decodeHelper gens' <$> getType arg)
            decodeArgs cons = liftA3 cons Parser.vName getDesc (fmap (decodeHelper gens') . getType)

            gens' = generics ++ gens
            defnNames = vName <$> defns
            -- getDesc par = if vDesc par /= "" then vDesc par else case elemIndex (vName par) defnNames of
            --                 Just n -> vDesc (defns !! n)
            --                 _ -> ""
            getDesc par = if vDesc par /= ""
                          then vDesc par
                          else Maybe.maybe "" (vDesc . (defns !!)) (elemIndex (vName par) defnNames)
            getType par = vType par <|> (elemIndex (vName par) defnNames >>= vType . (defns !!))
            -- getType par = if Data.Maybe.isJust (vType par) then vType par else case elemIndex (vName par) defnNames of
            --                 Just n -> vType (defns !! n)
            --                 _ -> Nothing

                -- True -> if vDesc par == ""
    where
        decodeVar :: [String] -> Parser.VarDefn -> LuaData.LuaVar
        decodeVar gens' (VarDefn x1 x2 x3) = LuaData.LuaVar x1 x3 (decodeHelper gens' <$> x2)

decodeTypeDefn :: Parser.TypeDefn -> LuaData.LuaType
decodeTypeDefn = decodeHelper []
-- decodeTypeDefn (NonPrimitive s) = LuaData.Custom s

-- decodeTypeDefn (Parser.Array types) = LuaData.Array (decodeTypeDefn <$> types)
-- decodeTypeDefn (Parser.Dict vars) = LuaData.Dict (map decodeVar vars)
-- decodeTypeDefn (Parser.Multi vars) = LuaData.Multi $ decodeTypeDefn <$> vars
-- decodeTypeDefn (Parser.Table keys vals) = LuaData.Table (decodeTypeDefn keys) (decodeTypeDefn vals)
-- decodeTypeDefn (Parser.Function generics args rets defns) =
--     LuaData.Function{
--         -- generics=LuaData.Generic <$> generics,
--         LuaData.pars=map (liftA3 LuaData.LuaFuncArg Parser.vName Parser.vDesc (fmap decodeTypeDefn . Parser.vType)) args,
--         LuaData.rets=map (liftA3 LuaData.LuaFuncRet Parser.vName Parser.vDesc (fmap decodeTypeDefn . Parser.vType)) rets
--     }



decodeVar :: Parser.VarDefn -> LuaData.LuaVar
decodeVar (VarDefn x1 x2 x3) = LuaData.LuaVar x1 x3 (decodeTypeDefn <$> x2)



-- instance LuaData LuaType where
--     writeLua

main :: IO ()
main = do
    -- Parser.parser
    -- let firstArg = "checkIsAlive: (params: {dist: num, name: string}) -> isAlive: bool"
    handle <- openFile "test file.txt" ReadMode
    contents <- hGetContents handle
    let firstArg = filter (/='\n') contents
        str = case runParser (Parser.parser :: Parser VarDefn) firstArg of
            (Right result, rest) -> "---------------------------\n\tOUTPUT\n---------------------------\n\n" ++ writeLua decoded
                ++ "\n\n---------------------------\n\tDECODED\n---------------------------\n" ++ show decoded
                ++ "\n\n---------------------------\n\tGENERICS\n---------------------------\n" ++ show (LuaData.getGenerics decoded)
                ++ "\n\n---------------------------\n\tUNPARSED\n---------------------------\n\t" ++ rest
                where decoded = decodeVar result
            (Left err, rest) -> "ERROR: " ++ err ++ "\n\tunparsed: " ++ rest

    putStrLn str


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




