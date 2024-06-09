{-# LANGUAGE LambdaCase #-}
import Data.List (intercalate)
-- import Language.Haskell.TH.Ppr (ppr_typedef)
import qualified Data.Maybe

import LuaData

decodeTypeDefn :: TypeDefn -> LuaData.LuaType



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




