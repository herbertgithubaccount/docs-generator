{-# LANGUAGE LambdaCase #-}
module Main where
-- import qualified Data.Maybe

import LuaData
-- import qualified LuaData (LuaType(Array), LuaVar)
import Parser
import Control.Applicative (liftA3, (<|>))
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(..))
import GHC.IO.Handle (hGetContents)
import Data.List (elemIndex, partition)
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
    Parser.Dict vars -> LuaData.Dict $ decodeVarHelper gens <$> vars
    Parser.Multi vars -> LuaData.Multi $ decodeHelper gens <$> vars
    Parser.Table keys vals -> LuaData.Table (decodeHelper gens keys) (decodeHelper gens vals)
    Parser.Function generics args rets defns ->
        LuaData.Function{
            LuaData.pars=args <&> decodeArgs LuaData.LuaFuncArg,
            LuaData.rets=rets <&> decodeArgs LuaData.LuaFuncRet
        }
        where
            decodeArgs cons = liftA3 cons Parser.vName getDesc (fmap (decodeHelper gens') . getType)

            gens' = generics ++ gens
            defnNames = vName <$> defns
            getDesc par = if vDesc par /= ""
                          then vDesc par
                          else Maybe.maybe "" (vDesc . (defns !!)) (elemIndex (vName par) defnNames)
            getType par = vType par <|> (elemIndex (vName par) defnNames >>= vType . (defns !!))
    -- Parser.Class vars -> let decodedVars = decodeVarHelper gens <$> vars
    --                         --  partitioner v = case vtype v of
    --                         --         Just LuaData.Function{pars} -> name (head pars) == "self"
    --                         --         _ -> False
    --                         --  (methods, fields) = partition partitioner decodedVars
    --                          fields' = liftA3 LuaData.LuaField LuaData.name LuaData.desc LuaData.vtype <$> fields
    --                         --  fields' = fields
    --                      in
    --                         LuaData.Class (LuaClassDefn{cname="", cdesc="", fields=fields', constants=Parser.constants , parents=[], isGlobal=False})
    Parser.Class{Parser.parents, Parser.fields, Parser.constants} ->
        LuaData.Class (LuaClassDefn{cname="", cdesc="",
            LuaData.fields= liftA3 LuaData.LuaField LuaData.name LuaData.desc LuaData.vtype . decodeVarHelper gens <$> fields,
            LuaData.constants=decodeVarHelper gens <$> constants,
            LuaData.parents=parents <&> (\x -> case decodeHelper gens x of
                LuaData.Class (clsdefn@LuaClassDefn{}) -> clsdefn
                _ -> LuaClassDefn{}
            ),
                -- <$> parents, 
            isGlobal=False
        })



decodeTypeDefn :: Parser.TypeDefn -> LuaData.LuaType
decodeTypeDefn = decodeHelper []

decodeVar :: VarDefn -> LuaVar
decodeVar = decodeVarHelper []

decodeVarHelper :: [String] -> Parser.VarDefn -> LuaData.LuaVar
decodeVarHelper gens VarDefn{vName, vType, vDesc}  = LuaData.LuaVar vName vDesc decodedType
    where
        decodedType = vType >>= Just . (\case{
            LuaData.Class clsdefn@LuaClassDefn{} ->
                LuaData.Class (LuaData.updateMethodSignatures clsdefn{cname=vName, cdesc = vDesc})
            ;
            x -> x;
        }) . decodeHelper gens
            -- (Just (LuaData.Class clsdefn@LuaData.LuaClassDefn{})) -> Just (LuaData.Class clsdefn{
            --     cname=vName, cdesc = vDesc
            --     })
            -- x -> x
        -- decodedType = case decodeHelper gens <$> vType of
        --     (Just (LuaData.Class clsdefn@LuaData.LuaClassDefn{})) -> Just (LuaData.Class clsdefn{
        --         cname=vName, cdesc = vDesc
        --         })
        --     x -> x
            -- LuaData.LuaVar varname vardesc (decodeHelper gens' <$> vartype)

-- decodeVar :: Parser.VarDefn -> LuaData.LuaVar
-- decodeVar (VarDefn x1 x2 x3) = LuaData.LuaVar x1 x3 (decodeTypeDefn <$> x2)

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
