{-# LANGUAGE OverloadedStrings #-}
module ExternalFunctions where

import Data.Char
-- import Data.Maybe
import Data.ByteString.Short (ShortByteString)

import LLVM.AST hiding (args,Name,resultType)
-- import LLVM.Target
import LLVM.AST.Constant
import LLVM.AST.Type hiding (resultType)
import qualified LLVM.AST.Global as LLVM
import qualified LLVM.AST.Name as LLVM

-- externalWith :: TargetLibraryInfo -> ShortByteString -> IO ShortByteString
-- -- [Parameter] -> Type -> IO ShortByteString
-- externalWith targetLibInfo name = do
--   lib <- fromMaybe (error "not a supported llvm funtion use exterl") <$> getLibraryFunction targetLibInfo name
--   getLibraryFunctionName targetLibInfo lib

external :: ShortByteString -> [Type] -> Type -> Definition
external name args resultType = GlobalDefinition $ functionDefaults
    { LLVM.name = LLVM.Name name
    , LLVM.returnType = resultType
    , LLVM.parameters = (zipWith (\t n -> Parameter t n []) args (UnName <$> [0..]), False)
    }

string  :: String -> Constant
string str = Array i8 $ map (Int 8 .fromIntegral. ord )str ++ [Int 8 0]

write :: Definition
write = external "write" [i32, ptr i8, i64] i64

puts :: Definition
puts = external "puts" [ptr i8] i32

perror :: Definition
perror = external "perror" [ptr i8] void

exit :: Definition
exit = external "exit" [i32] void
