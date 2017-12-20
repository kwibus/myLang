{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Statments
  ( Statments
  , Label
  , mergBlocks
  , genBlock
  , genFuncBlock
  , genMainBlock
  , toStatments
  , Statments.void
  , callBlock
  , callFunction
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

import LLVM.AST as AST hiding (Resume,args,resultType)
import qualified LLVM.AST.CallingConvention as LLVM

import qualified LLVM.AST.Operand as LLVM
import qualified LLVM.AST.Constant as LLVM hiding (type')
import qualified LLVM.AST.AddrSpace as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Name as LLVM

-- TODO rename end in function to more disciptive
-- TODO steal some names from IRBUilder,
--      rename staments emiter

freshName :: State StmtStack LLVM.Name
freshName = do
  w <- gets nameSupply
  modify $ \s -> s{nameSupply = nameSupply s +1}
  return $ UnName w

-- | Data struction that closely mirror BasicBlock but  it differnce in:
--
-- *  phi nod for blockcall are not all inserted (return form block call are)

-- Block is split up in 3 part because the generator for block does not know about end:
-- that will add in running the emitter
-- and in the emiter the insturcion or stored in a dlist

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
              | CallBlock LLVM.Name [Operand] LLVM.Name
              | LLVMEnd AST.Terminator -- TODO maybe keep llvm termnoatr out and keep control self
                                       -- TODO add exit statment
              deriving Show

newtype Statments a = Stmt (State StmtStack a)
                      deriving (Monad, Applicative,Functor,MonadFix)

lower:: Statments a -> State StmtStack a
lower (Stmt m) = m

data StmtStack = StmtStack { startBlock:: BlockStart
                           , emitedInst :: DList (Named Instruction)
                           , emitedBlocks :: DList Block
                           , nameSupply :: Word
                           }deriving Show

data Label = Label LLVM.Name LLVM.Type Operand -- TODO result could be another label
             deriving Show

callFunction :: ShortByteString -> [(Type,Operand)] -> Type -> Instruction -- use typed
callFunction name args resultType =
  let (argTyps, operandArgs) = unzip args
  in Call
      Nothing
      LLVM.C
      [] -- return atributes
      (Right (LLVM.ConstantOperand $ LLVM.GlobalReference (PointerType (FunctionType resultType argTyps False) (LLVM.AddrSpace 0)) (Name name)))
      [(o,[]) |o <- operandArgs]
      [] --fucntion atribues
      [] -- instruction Metadata

-- TODO add function exit

callBlock :: Label -> [Either Label Operand] -> Statments Operand
callBlock (Label calledBlock resultType resulOperand) args = Stmt $ do
  let operandArgs = map (either undefined id) args --FIXME convert label
  newBlockName <- freshName
  modify $ \(s@StmtStack {startBlock= start, emitedInst = i}) ->
    let newBlock = Block start (toList i) (CallBlock calledBlock operandArgs newBlockName)
    in s
      {startBlock = BlockStart newBlockName []
      ,emitedInst = empty
      ,emitedBlocks = cons newBlock (emitedBlocks s)}

  LocalReference resultType <$> lower ( toStatments $ Phi resultType [(resulOperand,calledBlock)] []) --FIXME make label posble result

toStatments :: Instruction -> Statments LLVM.Name
toStatments i = Stmt $ do
  name <- freshName
  modify $ \s -> s{emitedInst = cons (name := i) $ emitedInst s}
  return name

--TODO maybe remove
void :: Instruction -> Statments ()
void i = Stmt $ modify $ \s -> s{emitedInst = cons (Do i) $ emitedInst s}

genBlock :: Maybe ShortByteString -> [Type] -> ([(Type,LLVM.Name)]-> Statments Operand) -> Statments Label --make block result
genBlock maybeName typeofArgs fStmt = Stmt $ do
  name <- maybe freshName (return . Name) maybeName
  args <- mapM (\t -> fmap (\n -> (t ,n ))freshName ) typeofArgs
  nameSeed <- gets nameSupply
  let (resulOperand,newBlocks,newSeed) = toBlock nameSeed name Resume args (fStmt args)
  modify (\s -> s { emitedBlocks = emitedBlocks s `append` newBlocks
                  , nameSupply = newSeed
                  })
  return $ Label name LLVM.double resulOperand -- FIXME double

genFuncBlock:: Maybe LLVM.Name -> [Type] -> ([(Type,LLVM.Name)] -> Statments Operand) -> ([(Type, LLVM.Name)], [Block])
genFuncBlock maybeName argtyps fStmt = (args,toList blocks)
  where
    args = zipWith (\t n -> (t,UnName n)) argtyps [1..] -- TODO always start counting at 1
    entryName = fromMaybe (UnName 0) maybeName
    (_,blocks,_) = toBlock (fromIntegral (length argtyps) +1) entryName Return [] (fStmt args)

genMainBlock:: Maybe LLVM.Name -> [Type] -> ([(Type,LLVM.Name)] -> Statments Operand) -> ([(Type, LLVM.Name)], [Block])
genMainBlock maybeName argtyps fStmt = (args,toList blocks)
  where
    args = zipWith (\t n -> (t,UnName n)) argtyps [1..] -- TODO always start counting at 1
    entryName = fromMaybe (UnName 0) maybeName
    (_,blocks,_) = toBlock (fromIntegral (length argtyps) +1) entryName Exit [] (fStmt args)

-- TODO better name
-- TODO should make terminaton more configurable
data EndFlag = Return | Exit | Resume

toBlock :: Word -> LLVM.Name -> EndFlag -> [(Type,Name)] -> Statments Operand -> (Operand ,DList Block,Word)
toBlock nameSupplySeed name endFlag args (Stmt stmt) = (a, snoc (emitedBlocks newStmdStack) newBlock, nameSupply newStmdStack)
    where
      (a, newStmdStack) = runState stmt initStmtStack
      (realNameSupplySeed,realArgs,prolog, end) = case endFlag of
          Resume -> (nameSupplySeed +1, (blockType, UnName nameSupplySeed):args,[]                                                , ResultBlock a (UnName nameSupplySeed) )
          Return -> (nameSupplySeed   , args                                   ,[]                                                , LLVMEnd $ Ret (Just a) [])
          Exit   -> (nameSupplySeed   , args                                   ,[Do $ callFunction "ext" [(LLVM.i32,a)] LLVM.void], LLVMEnd $ Unreachable [])
      newBlock = Block (startBlock newStmdStack) (apply (emitedInst  newStmdStack) prolog) end
      initStmtStack = StmtStack { startBlock = BlockStart name realArgs
                                , emitedInst = empty
                                , emitedBlocks = empty
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
    callMap :: Map.Map Name [(Name,[Operand])]
    callMap = foldl registerCall Map.empty blocks

    registerCall :: Map.Map Name [(Name,[Operand])] -> Block -> Map.Map Name [(Name,[Operand])]
    registerCall oldCallMap (Block BlockStart {blockName = bName}
                                _
                                (CallBlock calledBlock args nameContinue)) =
        Map.insertWith  (++) calledBlock [(bName, ConstantOperand (LLVM.BlockAddress "main" nameContinue) : args)] oldCallMap
    registerCall oldCallMap _ = oldCallMap

    returnMap:: Map.Map Name [Name]
    returnMap = foldl registerReturn Map.empty blocks

    registerReturn :: Map.Map Name [Name] -> Block -> Map.Map Name [Name]
    registerReturn oldReturnMap (Block _  _ (CallBlock calledBlock _ nameContinue )) =
        Map.insertWith (++) (endMap Map.! calledBlock) [nameContinue] oldReturnMap
    registerReturn oldReturnMap _ = oldReturnMap

    endMap :: Map.Map Name Name
    endMap = foldl registerEnd Map.empty blocks

    blockMap :: Map.Map Name Block
    blockMap = foldl (\oldBlockMap b@(Block BlockStart {blockName = bName} _ _ ) -> Map.insert bName b oldBlockMap) Map.empty blocks

    registerEnd :: Map.Map Name Name -> Block -> Map.Map Name Name
    registerEnd oldEndMap block = newEndMap
      where
        (end,newEndMap) = walk oldEndMap block
        walk :: Map.Map Name Name -> Block -> (Name, Map.Map Name Name)
        walk currentEndMap (Block BlockStart {blockName = bName} _ endBlock) = case Map.lookup bName currentEndMap of
          Just previousFoundEnd -> (previousFoundEnd, currentEndMap)
          Nothing -> case endBlock of
            (CallBlock _ _ continue) -> walk (Map.insert bName end currentEndMap) (blockMap Map.! continue)
            _ -> (bName, Map.insert bName bName currentEndMap)

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
        phis =
          let argOrigins = transpose $ -- change a list of caller (with there there operand aruments) to list args (withe (operand, callername) )
                map (\(caller , callerOperands) ->
                  assert (length callerOperands == length args) $ -- check if every caller calles with corect number of arguments
                    map (,caller) callerOperands )
                $ fromMaybe (error $ "not in callMap" ++ show name) $ Map.lookup name callMap

          in zipWith (\(argType,argName) origin -> argName := Phi argType origin []) args argOrigins
