module Compile where

import LLVM.Target
import LLVM.Module
import LLVM.Context
import LLVM.AST as AST

compile :: FilePath -> AST.Module -> IO ()
compile file modul =
  withContext $ \c ->
    withModuleFromAST c  modul $  \m ->
      -- ByteString.putStrLn =<< moduleLLVMAssembly m --TODO add pretty print fucntion
      withHostTargetMachine $ \ targetMachine ->
        writeObjectToFile targetMachine (File file) m
