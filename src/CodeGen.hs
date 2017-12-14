{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo#-}
module CodeGen where

import Data.Maybe
import Control.Exception
import Control.Monad
import Data.ByteString.Short (ShortByteString)

import qualified LLVM.AST.Global as LLVM
import qualified LLVM.AST.Constant as LLVM hiding (type')
import LLVM.AST.Instruction hiding (args)
import LLVM.AST.Float

-- import qualified LLVM.AST as AST
import LLVM.AST hiding (args,Name,resultType)

import qualified LLVM.AST.Name as LLVM
import qualified LLVM.AST.Type as LLVM

import BruijnEnvironment
import ANormalForm
import Value (Primative(..),BuildIn(..))
import Statments

mkModule :: [Definition] -> Module --TODO better name, option as argument
mkModule definitions = defaultModule { moduleName="test", moduleDefinitions = definitions}

external :: ShortByteString -> [Parameter] -> Type -> Definition
external name args resultType = GlobalDefinition $ functionDefaults
    { LLVM.name = LLVM.Name name
    , LLVM.returnType = resultType
    , LLVM.parameters = (args, False)
    }

-- need to be topologically sorted to work
codeGen :: ANorm -> Definition
codeGen ast = GlobalDefinition  $ result {LLVM.name = "main"}
  where
    result = case ast of
      (Val (Constant v)) -> globalVariableDefaults
        { LLVM.isConstant = True
        , LLVM.type' = llvmType v
        -- , linkage = External
        -- , visibility=Protected
        , LLVM.initializer = Just $ llvmConstant v
        }

      t@(Val Lambda {}) ->
        let (vars,term) = accumulateVars t
            (args,blocks) = codeGenFunction (map (const LLVM.double) vars) term --TODO not only double
        in  functionDefaults
              { LLVM.returnType = LLVM.double -- TODO not only LLVM.double
              , LLVM.basicBlocks = blocks
              , LLVM.parameters = (map (\(argType ,argName) -> Parameter argType argName []) args,False)
              }
      Val v -> error $ "not implemented codeGen: " ++ show v

      t -> functionDefaults
        { LLVM.returnType = LLVM.double -- TODO not only LLVM.double
        , LLVM.basicBlocks = snd $ codeGenFunction [] t
        }

type SymbolTabel = BruijnEnv (Either Label Operand)

insertBackhole :: Int -> SymbolTabel -> SymbolTabel
insertBackhole = bInsertBlackhole

insertValues :: [Value] -> SymbolTabel -> Statments SymbolTabel
insertValues vs table = do
  operands <- mapM (`fromValue` table) vs
  return $ bInserts operands table

codeGenFunction :: [Type] -> ANorm -> ([(Type,LLVM.Name)],[BasicBlock])
codeGenFunction argtypes term = (args, mergBlocks blocks)
  where
    (args,blocks) = toBlock
      (Just "entry")
      argtypes
      (\_args -> do
        let symbolTabel = bInserts  (Right . uncurry LocalReference <$> _args) bEmtyEnv
        result <- codeGenInstructions term symbolTabel
        return $! either (const $! error "returned block address") id result
      )

codeGenInstructions :: ANorm -> SymbolTabel -> Statments (Either Label Operand)
codeGenInstructions (Appl f args) env = llvmInstruc f args env
codeGenInstructions t@(Val Lambda {}) env = do
    let (argsNames,body) = accumulateVars t
    Left <$> codeGenLambda env argsNames body

codeGenInstructions (Val v) env = fromValue v env

codeGenInstructions (Let defs t) env = do
    -- FIXME recursive do
    rec (newEnv, _) <- foldM (instrucDef newEnv) (bInsertBlackhole nDefs env,nDefs-1) defs
    codeGenInstructions t newEnv
  where
    nDefs = length defs

-- TODO better name
instrucDef :: SymbolTabel -> (SymbolTabel,Int) -> Def -> Statments (SymbolTabel, Int)
instrucDef finalEnv (env, n) (Def _ t) = do
  result <- codeGenInstructions t finalEnv
  let newEnv = bReplace (Bound n) result env
  return (newEnv, n-1)

codeGenLambda :: SymbolTabel -> [Name] -> ANorm -> Statments Label
codeGenLambda env argsNames body = genBlock Nothing (map (const LLVM.double) argsNames)
      (\args -> let newEnv = bInserts  (Right . uncurry LocalReference <$> args) env
        in either (const $ error "no yet supported") id <$>
        codeGenInstructions body newEnv
      )

-- TODO this should be part of Buildin definition
llvmInstruc :: Value -> [Value] -> SymbolTabel -> Statments (Either Label Operand)
llvmInstruc (Instruc BuildIn {prettyName = "+"}) [x,y] env = do
    inst <- FAdd noFastMathFlags <$> valueToOpterator x env <*> valueToOpterator y env  <*> return []
    Right . LocalReference LLVM.double <$> toStatments inst
llvmInstruc (Instruc BuildIn {prettyName = "*"}) [x,y]  env = do
    inst <- FMul noFastMathFlags <$> valueToOpterator x env <*> valueToOpterator y env <*> return []
    Right . LocalReference LLVM.double <$> toStatments inst

llvmInstruc (Instruc i) _ _ = error $ "Instruction "++ show i ++" is not inplmented"

llvmInstruc (Constant p) _ _ = error $ "Constant: "++ show p ++" can not be used as a function"
llvmInstruc t@Lambda {} args env =do
  let (argNames, body) = accumulateVars (Val t)
  newEnv <- assert (length argNames == length args) $
                  insertValues args env
  codeGenInstructions body newEnv

llvmInstruc (Var b) args env = case bMaybeLookup b env of
  Nothing -> error $ "missing entry in env for " ++ show b ++", called from llvmInstruc"
  Just (Left label) -> do
      operands  <- mapM (\ v -> fromValue v env) args
      Right <$> callBlock label operands --FIXME could be a function
  Just (Right o) -> ( error $ "apply operator: "  ++ show o )

-- TODO maybe remove is to short, mispelled
-- TODO could made result Just Operand no Statments are produced unless for converting lambda which will become a label
valueToOpterator :: Value -> SymbolTabel -> Statments Operand
valueToOpterator v env = do
  operand <- fromValue v env
  case operand of
    (Left _) -> error "try to use block addres as operator"
    (Right result) -> return result

fromValue :: Value -> SymbolTabel -> Statments (Either Label Operand)
fromValue (Constant v) _ = return $ Right $ ConstantOperand $ llvmConstant v
fromValue  (Var b) env = return $ fromMaybe
  (error $ "missing entry in env for " ++ show b ++", called from fromValue") $
  bMaybeLookup b env
fromValue t@Lambda {} env =
    let (argsNames,body) = accumulateVars (Val t)
    in Left <$> codeGenLambda env argsNames body
fromValue  v _ = error $ "cant transform " ++ show v

--TODO Prim type should maybe already be llvm type
llvmType :: Primative -> LLVM.Type
llvmType MyBool {} = IntegerType 1
llvmType MyDouble {} = FloatingPointType DoubleFP

llvmConstant :: Primative -> LLVM.Constant
llvmConstant (MyBool True ) = LLVM.Int 1 1
llvmConstant (MyBool False) = LLVM.Int 1 0
llvmConstant (MyDouble d) = LLVM.Float $ Double d
