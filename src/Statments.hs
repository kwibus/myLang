{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Statments
  ( Statments
  , Label
  , mergBlocks
  , toBlock
  , toStatments
  , Statments.void
  , call
  , callFunction
  , genBlock
  )
where
import Data.ByteString.Short (ShortByteString)
import Data.List
import Control.Exception
-- import qualified Data.DList as DList
import Data.DList hiding (map)
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State

import LLVM.AST as AST hiding (args,resultType)
import qualified LLVM.AST.CallingConvention as LLVM

import qualified LLVM.AST.Operand as LLVM
import qualified LLVM.AST.Constant as LLVM hiding (type')
import qualified LLVM.AST.AddrSpace as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Name as LLVM

-- TODO name it contnue ore returnblock
-- TODO rename end in function to more disciptive

freshName :: State StmtStack LLVM.Name
freshName = do
  w <- gets nameSupply
  modify $ \s -> s{nameSupply = nameSupply s +1}
  return $ UnName w

-- TODO can we do away with blockstart insructoin blockend
data Block = Block
                BlockStart
                [Named Instruction]
                BlockEnd
            deriving Show

data BlockStart = BlockStart
                   { blockName :: LLVM.Name
                   , blockArgs :: [(Type, Name)]
                   }
                deriving Show
--TODO wright funtion blockName :: Block -> Name

data BlockEnd = ResultBlock Operand Name-- TODO one result for now
                  -- TODO can LLVM.Name be replaced with something that always reference to codeblock
                  -- TODO if you
              | CallBlock LLVM.Name [Operand] LLVM.Name
              | LLVMEnd AST.Terminator -- TODO maybe keep llvm termnoatr out and keep control self
              deriving Show

-- TODO if you want to encapsulate it has to be newtype
-- TODO make monoid
-- TODO reinvent state monad without result
newtype Statments a = Stmt (State StmtStack a)
                      deriving (Monad, Applicative,Functor,MonadFix)

lower:: Statments a -> State StmtStack a
lower (Stmt m) = m

data StmtStack = StmtStack { startInsruction :: BlockStart
                           , instructions :: DList (Named Instruction)
                           , blocks :: DList Block
                           , nameSupply :: Word
                           }deriving Show

data Label = Label LLVM.Name LLVM.Type Operand
             deriving Show

callFunction :: ShortByteString -> [(Type,Operand)] -> Type -> Instruction
callFunction name args resultType =
  let (argTyps, operandArgs) = unzip args
  in ( Call
      Nothing
      LLVM.C
      [] -- return atributes
      (Right (LLVM.ConstantOperand $ LLVM.GlobalReference (PointerType (FunctionType resultType argTyps False) (LLVM.AddrSpace 0)) (Name name)))
      [(o,[]) |o <- operandArgs]
      [] --fucntion atributes
      [] -- instruction Metadata
    )

call ::Label -> [Either Label Operand] -> Statments Operand
call (Label calledBlock resultType resulOperand) args = Stmt $ do
  let operandArgs = map (either undefined id) args --FIXME conver label
  newBlockName <- freshName
  modify $ \(s@StmtStack {startInsruction = start, instructions = i}) ->
    let newBlock = Block start (toList i) (CallBlock calledBlock operandArgs newBlockName)
    in s
      {startInsruction = BlockStart newBlockName []
      ,instructions = empty
      ,blocks = cons newBlock (blocks s)}

  LocalReference resultType <$> lower ( toStatments $ Phi resultType [(resulOperand,calledBlock)] []) --FIXME make label posble result

toStatments :: Instruction -> Statments LLVM.Name
toStatments i = Stmt $ do
  name <- freshName
  modify $ \s -> s{instructions = cons (name := i) $ instructions s}
  return name

--TOD maybe remove
void :: Instruction -> Statments ()
void i = Stmt $ modify $ \s -> s{instructions = cons (Do i) $ instructions s}

-- TODO rename newblock to createBlock
genBlock :: Maybe ShortByteString -> [Type] -> ([(Type,LLVM.Name)]-> Statments Operand) -> Statments Label --make block result
genBlock maybeName typeofArgs fStmt = Stmt $ do
  name <- maybe freshName (return . Name) maybeName --TODO can this not be done in toBlock
  args <- mapM (\t -> fmap (\n -> (t ,n ))freshName ) typeofArgs
  nameSeed <- gets nameSupply
  let (resulOperand,newBlocks,newSeed) = toBlock' nameSeed name True args (fStmt args)
  modify (\s -> s { blocks = blocks s `append` newBlocks
                  , nameSupply = newSeed
                  })
  return $ Label name LLVM.double resulOperand-- FIXME double

-- TODO beter name  (run?)
toBlock :: Maybe LLVM.Name -> [Type] -> ([(Type,LLVM.Name)] -> Statments Operand) -> ([(Type, LLVM.Name)], DList Block)
toBlock maybeName argtyps fStmt = (args,blocks)
  where
    args = zipWith (\t n -> (t,UnName n)) argtyps [1..]
    entryName = fromMaybe (UnName 0) maybeName
    (_,blocks,_) = toBlock' (fromIntegral (length argtyps) +1) entryName False [] (fStmt args)

-- TODO better name
toBlock' :: Word -> LLVM.Name -> Bool -> [(Type,Name)] -> Statments Operand -> (Operand ,DList Block,Word)
toBlock' nameSupplySeed name returnFromBlock args (Stmt stmt) = (a, (snoc (blocks newStmdStack) newBlock ), nameSupply newStmdStack)
    where
      (a, newStmdStack) = runState stmt initStmtStack
      (realNameSupplySeed,realArgs, end) = if returnFromBlock
          then (nameSupplySeed +1, (blockType, UnName nameSupplySeed):args,ResultBlock a $ UnName nameSupplySeed)
          else (nameSupplySeed,args, LLVMEnd $ Ret (Just  a) [])
      newBlock = Block (startInsruction newStmdStack) (toList $instructions newStmdStack) end
      initStmtStack = StmtStack { startInsruction = BlockStart name realArgs
                                , instructions = empty
                                , blocks = empty
                                , nameSupply = realNameSupplySeed }

blockType :: LLVM.Type
blockType =  PointerType LLVM.i8 (LLVM.AddrSpace 0)

-- TODO clean up
--  is this more clear then when you do it when define blocks
--  is it better to have one mape with all properties
--  allow block tail calls
mergBlocks :: [Block] -> [BasicBlock]
mergBlocks blocks = map block2BacsicBlock blocks
  where
    callMap :: Map.Map Name [(Name,[Operand], Name)]
    callMap = foldl registerCall Map.empty blocks

    -- FIXME callMap thirds item is never used and can be removed
    registerCall :: Map.Map Name [(Name,[Operand], Name)] -> Block -> Map.Map Name [(Name,[Operand], Name)]
    registerCall oldCallMap (Block (BlockStart {blockName = bName})
                                _
                                (CallBlock calledBlock args nameContinue)) =
        Map.insertWith  (++) calledBlock [(bName, ConstantOperand (LLVM.BlockAddress "main" nameContinue) : args, nameContinue)] oldCallMap
    registerCall oldCallMap _ = oldCallMap

    returnMap:: Map.Map Name [Name]
    returnMap = foldl registerReturn Map.empty blocks

    registerReturn :: Map.Map Name [Name] -> Block -> Map.Map Name [Name]
    registerReturn oldReturnMap (Block _  _ (CallBlock calledBlock _ nameContinue )) =
        Map.insertWith (++) (endMap Map.! calledBlock) [nameContinue] oldReturnMap
    registerReturn oldReturnMap _ = oldReturnMap

    endMap :: Map.Map Name Name
    endMap = foldl registerEnd Map.empty blocks

    -- TODO rename env (env is to genral)
    allMap :: Map.Map Name Block
    allMap = foldl (\env b@(Block (BlockStart {blockName = bName}) _ _ ) -> Map.insert bName b env ) Map.empty blocks

    -- TODO rename env
    registerEnd :: Map.Map Name Name -> Block -> Map.Map Name Name
    registerEnd oldEndMap block = newEnv
      where
        (end,newEnv) = walk oldEndMap block
        walk :: Map.Map Name Name -> Block -> (Name, Map.Map Name Name)
        walk _env (Block (BlockStart {blockName = bName}) _ endBlock) = case Map.lookup bName _env of
          Just previousFoundEnd -> (previousFoundEnd, _env)
          Nothing -> case endBlock of
            (CallBlock _ _ continue) -> walk (Map.insert bName end _env) (allMap Map.! continue)
            _ -> (bName, Map.insert bName bName _env)

    blockEnd2Termintor name end = case end of
        ResultBlock _ returnBlock -> IndirectBr (LocalReference blockType returnBlock) ( returnMap Map.! name ) []
        CallBlock calledBlockname _ _ -> Br calledBlockname []
        LLVMEnd t -> t

    block2BacsicBlock :: Block -> BasicBlock
    block2BacsicBlock (Block
        BlockStart
          { blockName = name
          , blockArgs = args
          }
        instructions
        end) = BasicBlock name (phis ++ instructions) $ Do $ blockEnd2Termintor name end
            where
        phis :: [Named Instruction]
        phis = --TODO add comments
          let argOrigins = transpose $
                map (\(caller , callerOperands ,_) ->
                  assert (length callerOperands == length args) $
                    map (,caller) callerOperands )
                $ fromMaybe (error $ "not in callMap" ++ show name) $ Map.lookup name callMap

          in zipWith (\(argType,argName) origin -> argName := Phi argType origin []) args argOrigins
