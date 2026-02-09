{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import LLVM.Core qualified as LLVM
import LLVM.InstructionBuilder qualified as LLVM
import System.OsPath (osp)

main :: IO ()
main = do
    context <- LLVM.contextCreate
    let ?context = context
    module_ <- LLVM.moduleCreateWithName "module"

    let fibType = LLVM.functionType [LLVM.int64Type] LLVM.int64Type False
    fib <- LLVM.addFunction module_ "fib" fibType 
    initialBlock <- LLVM.appendBasicBlock fib ""


    builder <- LLVM.createBuilder

    LLVM.positionBuilderAtEnd builder initialBlock

    bool <- LLVM.buildICmp builder LLVM.IntSLE (LLVM.getParam fib 0) (LLVM.constInt LLVM.int64Type 2 False) ""
    
    lessBlock <- LLVM.appendBasicBlock fib ""
    greaterBlock <- LLVM.appendBasicBlock fib ""
    _ <- LLVM.buildCondBr builder bool lessBlock greaterBlock

    LLVM.positionBuilderAtEnd builder lessBlock
    _ <- LLVM.buildRet builder (LLVM.getParam fib 0)

    LLVM.positionBuilderAtEnd builder greaterBlock
    minusOne <- LLVM.buildSub builder (LLVM.getParam fib 0) (LLVM.constInt LLVM.int64Type 1 False) ""
    firstResult <- LLVM.buildCall builder fibType fib [minusOne] ""
    minusTwo <- LLVM.buildSub builder (LLVM.getParam fib 0) (LLVM.constInt LLVM.int64Type 2 False) ""
    secondResult <- LLVM.buildCall builder fibType fib [minusTwo] ""
    result <- LLVM.buildAdd builder firstResult secondResult ""
    _ <- LLVM.buildRet builder result

    mainFunction <- LLVM.addFunction module_ "main" (LLVM.functionType [LLVM.pointerType, LLVM.int32Type] LLVM.int64Type False)

    startBlock <- LLVM.appendBasicBlock mainFunction ""

    LLVM.positionBuilderAtEnd builder startBlock
    fibResult <- LLVM.buildCall builder fibType fib [LLVM.constInt LLVM.int64Type 10 False] ""
    _ <- LLVM.buildRet builder fibResult

    LLVM.printModuleToFile module_ ([osp|example.ll|])

    LLVM.dumpModule module_

