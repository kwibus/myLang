{-# LANGUAGE OverloadedStrings #-}
module Jit where

import Foreign.Ptr
import Data.ByteString.Short as ByteString
import LLVM.Context
import LLVM.Module as Mod
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
import  LLVM.OrcJIT.CompileLayer
import Foreign.Storable
import qualified Data.Map.Strict as Map
import Data.IORef

-- foreign import ccall "dynamic"
--   mkMain :: FunPtr (IO Int32) -> IO Int32

foreign import ccall "dynamic" fnToDouble :: FunPtr (IO Double) -> IO Double
foreign import ccall "dynamic" fnToWord:: FunPtr (IO Word) -> IO Word

castFn :: WordPtr -> TypeInstance  -> IO Primative
castFn addres t = do
   let fnPtr = castPtrToFunPtr $ wordPtrToPtr addres
   case t of
    TDouble -> MyDouble <$> fnToDouble fnPtr
    _ -> do
      word <- fnToWord fnPtr
      return $ wordToPrimative word  t

cast :: WordPtr -> TypeInstance -> IO Primative
cast addres TDouble = MyDouble <$> peek  (wordPtrToPtr addres)
cast address t = do
     word <- peek  $ wordPtrToPtr address:: IO Word
     return $ wordToPrimative word t

wordToPrimative :: Word -> TypeInstance -> Primative
wordToPrimative word TBool = MyBool $  case word of
        0 -> False
        _ -> True

-- jit :: Context -> (MCJIT -> IO a) -> IO a
-- jit c = withMCJIT c optlevel model ptrelim fastins
--   where
--     optlevel = Just (0::Word)  -- optimization level
--     model    = Nothing -- code model ( Default )
--     ptrelim  = Nothing -- frame pointer elimination
--     fastins  = Nothing -- fast instruction selection
--
-- runJIT :: AST.Module -> IO Word
-- runJIT modul =
--   withContext $ \context ->
--     jit context $ \executionEngine ->
--       withModuleFromAST context modul $ \m ->
--         withModuleInEngine executionEngine m $ \ee -> do
--         ByteString.putStrLn =<< moduleLLVMAssembly m --TODO add pretty print fucntion
--         maybeA <- getFunction ee "main"
--         case maybeA of
--           Nothing ->  error "fail"
--           Just f ->  castFn f

symbolResolver :: SymbolResolver --FIXME
symbolResolver = SymbolResolver resolveError
 where
 resolveError msym = error $ "symbol" ++ show msym ++ "could not be found"

nullResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
nullResolver s = putStrLn "nullresolver" >> return (Left (JITSymbolError "unknown symbol"))

withLLVM :: AST.Module -> (Module -> IO a) -> IO a
withLLVM modul f = withContext $ \context -> withModuleFromAST context modul f

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

runJit :: AST.Module -> ShortByteString -> (WordPtr -> IO a) -> IO a
runJit amod name f = withLLVM amod $ \ modul ->
   jit modul name f

evalWithJit :: BruijnTerm () () -> IO Primative
evalWithJit term = withLLVM (mkModule $ codeGen $ aNormalize term) $ \ modul ->
      jit modul "main" $ \address ->
      if atom $ reproces term --TODO withou reproces
      then cast address primativeType
      else castFn address primativeType
   where
      primativeType = case solver term of
            Right (TVal p) -> p
            t -> error $ "cant jit with type" ++ show t
