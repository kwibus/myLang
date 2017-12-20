module Compile where

import LLVM.Target
import LLVM.Module
import LLVM.Context
import LLVM.AST as AST

--TODO add linker
compile :: FilePath -> AST.Module -> IO ()
compile file modul =
  withContext $ \c ->
    withModuleFromAST c  modul $  \m ->
      withHostTargetMachine $ \ targetMachine ->
        writeObjectToFile targetMachine (File file) m
