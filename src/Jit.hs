{-# LANGUAGE OverloadedStrings #-}
module Jit where

import Foreign.Ptr
import Data.ByteString.Char8 as ByteString (unpack)
import Data.ByteString.Short as ByteString hiding (unpack)
import LLVM.Context
import LLVM.Module as Mod
import LLVM.Analysis
import BruijnTerm (BruijnTerm)
import ANormalForm
import Unprocessed hiding (peek)
import CodeGen
import Value
import Type
import qualified LLVM.AST as AST

import TypeCheck
import LLVM.Target
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import Foreign.Storable
import LLVM.Internal.OrcJIT
import System.Posix.DynamicLinker
import Data.Int
import qualified Data.Map.Strict as Map
import Data.IORef

-- foreign import ccall "dynamic"
--   mkMain :: FunPtr (IO Int32) -> IO Int32

import LLVM.Internal.ExecutionEngine --TODO

foreign import ccall unsafe "dynamic" fnToInt64:: FunPtr (IO Int64) -> IO Int64
foreign import ccall unsafe "dynamic" fnToInt32:: FunPtr (IO Int32) -> IO Int32
foreign import ccall unsafe "dynamic" fnToDouble :: FunPtr (IO Double) -> IO Double
foreign import ccall unsafe "dynamic" fnToWord:: FunPtr (IO Word) -> IO Word

-- TODO do we need 2 typs of Jit
-- TODO consient name jit function (Jit /JIT)
-- TODO is it needed to acces result jit in callback
-- TODO mcjit gives FunPtr while orcJit gives wordPtr

castFn :: WordPtr -> TypeInstance  -> IO Primative
castFn addres t = do
   let fnPtr = castPtrToFunPtr $ wordPtrToPtr addres
   case t of
    TDouble -> MyDouble <$> fnToDouble fnPtr
    TBool -> do
      word <- fnToWord fnPtr
      return $ MyBool $ wordToBool word

cast :: WordPtr -> TypeInstance -> IO Primative
cast address TDouble = MyDouble <$> peek  (wordPtrToPtr address)
cast address TBool  = do
     word <- peek  $ wordPtrToPtr address:: IO Word
     return $ MyBool $ wordToBool word

wordToBool :: Word -> Bool
wordToBool word = case word of
        0 -> False
        _ -> True

mcjit :: Context -> (MCJIT -> IO a) -> IO a
mcjit c = withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just (0::Word)  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

runMCJIT :: AST.Module -> ShortByteString  -> (FunPtr () -> IO a) -> IO a
runMCJIT modul functionName f =
  withContext $ \context ->
    mcjit context $ \executionEngine ->
      withModuleFromAST context modul $ \m ->
        withModuleInEngine executionEngine m $ \ee -> do
        maybeA <- getFunction ee (AST.Name functionName)
        case maybeA of
          Nothing ->  error "fail"
          Just funAddress -> f funAddress

symbolResolver :: CompileLayer l => l -> SymbolResolver
symbolResolver compileLayer =
   SymbolResolver
      -- (\m -> findSymbol compileLayer m False)
      (\(MangledSymbol n) -> do
         -- TODO dl <- dlopen "" [RTLD_LAZY, RTLD_GLOBAL]
         ptr <- dlsym Default (unpack n)
         return $ Right $ JITSymbol (ptrToWordPtr $ castFunPtrToPtr  ptr) (JITSymbolFlags True False True True )
      )

nullResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
nullResolver s = putStrLn "nullresolver" >> return (Left (JITSymbolError "unknown symbol"))

withLLVM :: AST.Module -> (Module -> IO a) -> IO a
withLLVM modul f = withContext $ \context -> withModuleFromAST context modul f

-- TODO rename orcJIt
jit :: Module -> ShortByteString -> (WordPtr -> IO a) -> IO a
jit modul name f= do
   resolvers <- newIORef Map.empty
   withHostTargetMachine $ \targetMachine  ->
      withExecutionSession $ \executSessoin ->
        withObjectLinkingLayer executSessoin (\k -> fmap (\rs -> rs Map.! k) (readIORef resolvers)) $ \objectLayer ->
          withIRCompileLayer objectLayer targetMachine $ \compileLayer -> do
            entry <- mangleSymbol compileLayer name
            withModuleKey executSessoin $ \moduleKey ->
               withSymbolResolver executSessoin (SymbolResolver (\sym -> findSymbol compileLayer sym False)) $ \resolver -> do
                 modifyIORef' resolvers (Map.insert moduleKey resolver)
                 withModule compileLayer moduleKey modul  $ do
                    Right (JITSymbol addres _) <- findSymbol compileLayer entry True
                    f addres

-- TODO rename orcJit
runJit :: AST.Module -> ShortByteString -> (WordPtr -> IO a) -> IO a
runJit amod name f =
  withLLVM amod $ \ modul ->
    jit modul name f

-- TODO is this the right place to define it
evalWithJit :: BruijnTerm () () -> IO Primative
evalWithJit term = withLLVM (mkModule [codeGen $ aNormalize term]) $ \ modul ->do
      verify modul --TODO shoule we verify
      jit modul "main" $ \address ->
        if atom $ reproces term --TODO withou reproces
        then cast address primativeType
        else castFn address primativeType
   where
      primativeType = case solver term of
            Right (TVal p) -> p
            t -> error $ "cant jit with type" ++ show t


evalWithMCJit :: BruijnTerm () () -> IO Primative
evalWithMCJit term = runMCJIT (mkModule [codeGen $ aNormalize term]) "main" $ \functionPtr->
      let address = ptrToWordPtr $ castFunPtrToPtr functionPtr
      in if atom $ reproces term --TODO withou reproces
         then cast address primativeType
         else castFn address primativeType
   where
      primativeType = case solver term of
            Right (TVal p) -> p
            t -> error $ "cant jit with type" ++ show t
