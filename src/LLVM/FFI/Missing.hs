{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -optc -Wno-discarded-qualifiers #-}
-- Workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/26852
{-# OPTIONS_GHC -optc -Wno-incompatible-pointer-types #-}

module LLVM.FFI.Missing where

import Foreign.C (CInt (..), CString, CUInt (..))
import Foreign.Ptr (Ptr)
import LLVM.FFI.Core qualified as Raw
import LLVM.Internal.Wrappers (FunctionTypeRef, IntPredicate, MetaDataRef, OperandBundleRef, RawFastMathFlags, RawGEPNoWrapFlags, RawIntPredicate, RawRealPredicate)

foreign import capi unsafe "llvm-c/Core.h LLVMPrintModuleToFile"
    printModuleToFile ::
        Raw.ModuleRef -> CString -> Ptr CString -> IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMDumpModule"
    dumpModule ::
        Raw.ModuleRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMDisposeBuilder"
    disposeBuilder ::
        Raw.BuilderRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMPrintModuleToString"
    printModuleToString ::
        Raw.ModuleRef -> IO CString

foreign import capi unsafe "llvm-c/Core.h LLVMPositionBuilderBeforeDbgRecords"
    positionBuilderBeforeDbgRecords ::
        Raw.BuilderRef -> Raw.BasicBlockRef -> Raw.ValueRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMPositionBuilderBeforeInstrAndDbgRecords"
    positionBuilderBeforeInstrAndDbgRecords ::
        Raw.BuilderRef -> Raw.ValueRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetArgOperand"
    setArgOperand ::
        Raw.ValueRef -> CUInt -> Raw.ValueRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetAtomicSingleThread"
    setAtomicSingleThread :: Raw.ValueRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetAtomicSyncScopeID"
    setAtomicSyncScopeID :: Raw.ValueRef -> CUInt -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetCurrentDebugLocation2"
    setCurrentDebugLocation2 :: Raw.BuilderRef -> MetaDataRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetExact"
    setExact :: Raw.ValueRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetFastMathFlags"
    getFastMathFlags :: Raw.ValueRef -> IO RawFastMathFlags

foreign import capi unsafe "llvm-c/Core.h LLVMSetFastMathFlags"
    setFastMathFlags :: Raw.ValueRef -> RawFastMathFlags -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetIsDisjoint"
    setIsDisjoint :: Raw.ValueRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetNNeg"
    setNNeg :: Raw.ValueRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetNSW"
    setNSW :: Raw.ValueRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetNUW"
    setNUW :: Raw.ValueRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetParentCatchSwitch"
    setParentCatchSwitch :: Raw.ValueRef -> Raw.ValueRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetVolatile"
    setVolatile :: Raw.ValueRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMSetWeak"
    setWeak :: Raw.ValueRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetCurrentDebugLocation2"
    getCurrentDebugLocation2 :: Raw.BuilderRef -> IO MetaDataRef

foreign import capi unsafe "llvm-c/Core.h LLVMAddMetadataToInst"
    addMetadataToInst :: Raw.BuilderRef -> Raw.ValueRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMBuilderGetDefaultFPMathTag"
    builderGetDefaultFPMathTag :: Raw.BuilderRef -> IO MetaDataRef

foreign import capi unsafe "llvm-c/Core.h LLVMBuilderSetDefaultFPMathTag"
    builderSetDefaultFPMathTag :: Raw.BuilderRef -> MetaDataRef -> IO ()

-- This needs to use `ccall` instead of `capi` since the `Ptr OperandBundleRef` is represented as a `void**` and if we use
-- capi, that makes the c stub throw a type error because -Wincompatible-pointer-types only makes an exception for `void*`.
-- Passing `void**` to something that expects a `struct LLVMOpaqueOperatorBundle**` fails.
foreign import ccall unsafe "llvm-c/Core.h LLVMBuildCallBr"
    buildCallBr ::
        Raw.BuilderRef ->
        FunctionTypeRef ->
        Raw.ValueRef ->
        Raw.BasicBlockRef ->
        Raw.BasicBlockRef ->
        CUInt ->
        Ptr Raw.ValueRef ->
        CUInt ->
        Ptr OperandBundleRef ->
        CUInt ->
        CString ->
        IO Raw.ValueRef

foreign import ccall unsafe "llvm-c/Core.h LLVMBuildInvokeWithOperandBundles"
    buildInvokeWithOperandBundles ::
        Raw.BuilderRef ->
        FunctionTypeRef ->
        Raw.ValueRef ->
        Ptr Raw.ValueRef ->
        CUInt ->
        Raw.BasicBlockRef ->
        Raw.BasicBlockRef ->
        Ptr OperandBundleRef ->
        CUInt ->
        CString ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMBuildCleanupRet"
    buildCleanupRet ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        Raw.BasicBlockRef ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMBuildCatchRet"
    buildCatchRet ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        Raw.BasicBlockRef ->
        IO Raw.ValueRef

foreign import ccall unsafe "llvm-c/Core.h LLVMBuildCatchPad"
    buildCatchPad ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        Ptr Raw.ValueRef ->
        CUInt ->
        CString ->
        IO Raw.ValueRef

foreign import ccall unsafe "llvm-c/Core.h LLVMBuildCleanupPad"
    buildCleanupPad ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        Ptr Raw.ValueRef ->
        CUInt ->
        CString ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMBuildCatchSwitch"
    buildCatchSwitch ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        Raw.BasicBlockRef ->
        CUInt ->
        CString ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetNumClauses"
    getNumClauses ::
        Raw.ValueRef ->
        IO CUInt

foreign import capi unsafe "llvm-c/Core.h LLVMGetClause"
    getClause ::
        Raw.ValueRef ->
        CUInt ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMAddClause"
    addClause ::
        Raw.ValueRef ->
        Raw.ValueRef ->
        IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMIsCleanup"
    isCleanup ::
        Raw.ValueRef ->
        IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMSetCleanup"
    setCleanup ::
        Raw.ValueRef ->
        Raw.Bool ->
        IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMAddHandler"
    addHandler ::
        Raw.ValueRef ->
        Raw.BasicBlockRef ->
        IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetNumHandlers"
    getNumHandlers ::
        Raw.ValueRef ->
        IO CUInt

foreign import ccall unsafe "llvm-c/Core.h LLVMGetNumHandlers"
    getHandlers ::
        Raw.ValueRef ->
        Ptr Raw.BasicBlockRef ->
        IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetArgOperand"
    getArgOperand ::
        Raw.ValueRef ->
        CUInt ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetParentCatchSwitch"
    getParentCatchSwitch ::
        Raw.ValueRef ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMBuildExactUDiv"
    buildExactUDiv ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        Raw.ValueRef ->
        CString ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetNUW"
    getNUW ::
        Raw.ValueRef ->
        IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMGetNSW"
    getNSW ::
        Raw.ValueRef ->
        IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMGetExact"
    getExact ::
        Raw.ValueRef ->
        IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMGetNNeg"
    getNNeg ::
        Raw.ValueRef ->
        IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMCanValueUseFastMathFlags"
    canValueUseFastMathFlags ::
        Raw.ValueRef ->
        IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMGetIsDisjoint"
    getIsDisjoint ::
        Raw.ValueRef ->
        IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMBuildMemSet"
    buildMemSet ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        Raw.ValueRef ->
        Raw.ValueRef ->
        CUInt ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMBuildMemCpy"
    buildMemCpy ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        CUInt ->
        Raw.ValueRef ->
        CUInt ->
        Raw.ValueRef ->
        IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMBuildMemMove"
    buildMemMove ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        CUInt ->
        Raw.ValueRef ->
        CUInt ->
        Raw.ValueRef ->
        IO Raw.ValueRef

foreign import ccall "llvm-c/Core.h LLVMBuildGEPWithNoWrapFlags"
    buildGEPWithNoWrapFlags ::
        Raw.BuilderRef ->
        Raw.TypeRef ->
        Raw.ValueRef ->
        Ptr Raw.ValueRef ->
        CUInt ->
        CString ->
        RawGEPNoWrapFlags ->
        IO Raw.ValueRef

foreign import capi "llvm-c/Core.h LLVMGetVolatile"
    getVolatile :: Raw.ValueRef -> IO Raw.Bool

foreign import capi "llvm-c/Core.h LLVMGetWeak"
    getWeak :: Raw.ValueRef -> IO Raw.Bool

foreign import capi "llvm-c/Core.h LLVMBuildAddrSpaceCast"
    buildAddrSpaceCast ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        Raw.TypeRef ->
        CString ->
        IO Raw.ValueRef

foreign import capi "llvm-c/Core.h LLVMBuildIntCast2"
    buildIntCast2 ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        Raw.TypeRef ->
        Raw.Bool ->
        CString ->
        IO Raw.ValueRef

foreign import ccall "llvm-c/Core.h LLVMBuildCallWithOperandBundles"
    buildCallWithOperandBundles ::
        Raw.BuilderRef ->
        FunctionTypeRef ->
        Raw.ValueRef ->
        Ptr Raw.ValueRef ->
        CUInt ->
        Ptr OperandBundleRef ->
        CUInt ->
        CString ->
        IO Raw.ValueRef

foreign import capi "llvm-c/Core.h LLVMBuildFreeze"
    buildFreeze ::
        Raw.BuilderRef ->
        Raw.ValueRef ->
        CString ->
        IO Raw.ValueRef

foreign import capi "llvm-c/Core.h LLVMGetNumMaskElements"
    getNumMaskElements ::
        Raw.ValueRef ->
        IO CUInt

foreign import capi "llvm-c/Core.h LLVMGetUndefMaskElem"
    getUndefMaskElem ::
        IO CInt

foreign import capi "llvm-c/Core.h LLVMGetMaskValue"
    getMaskValue ::
        Raw.ValueRef ->
        CUInt ->
        IO CInt

foreign import capi "llvm-c/Core.h LLVMIsAtomicSingleThread"
    isAtomicSingleThread ::
        Raw.ValueRef ->
        IO Raw.Bool

foreign import capi "llvm-c/Core.h LLVMIsAtomic"
    isAtomic ::
        Raw.ValueRef ->
        IO Raw.Bool

foreign import capi "llvm-c/Core.h LLVMGetAtomicSyncScopeID"
    getAtomicSyncScopeID ::
        Raw.ValueRef ->
        IO CUInt

foreign import capi "llvm-c/Core.h LLVMBuildCall2"
    buildCall2 ::
        Raw.BuilderRef ->
        FunctionTypeRef ->
        Raw.ValueRef ->
        Ptr Raw.ValueRef ->
        CUInt ->
        CString ->
        IO Raw.ValueRef

foreign import capi "llvm-c/Core.h LLVMBuildICmp"
    buildICmp ::
        Raw.BuilderRef ->
        RawIntPredicate ->
        Raw.ValueRef ->
        Raw.ValueRef ->
        CString ->
        IO Raw.ValueRef

foreign import capi "llvm-c/Core.h LLVMBuildFCmp"
    buildFCmp ::
        Raw.BuilderRef ->
        RawRealPredicate ->
        Raw.ValueRef ->
        Raw.ValueRef ->
        CString ->
        IO Raw.ValueRef

foreign import capi "llvm-c/Core.h LLVMX86AMXTypeInContext"
    x86AMXTypeInContext ::
        Raw.ContextRef -> IO Raw.TypeRef

foreign import capi "llvm-c/Core.h LLVMTokenTypeInContext"
    tokenTypeInContext ::
        Raw.ContextRef -> IO Raw.TypeRef

foreign import capi "llvm-c/Core.h LLVMMetadataTypeInContext"
    metadataTypeInContext ::
        Raw.ContextRef -> IO Raw.TypeRef

foreign import ccall "llvm-c/Core.h LLVMTargetExtTypeInContext"
    targetExtTypeInContext ::
        Raw.ContextRef ->
        CString ->
        Ptr Raw.TypeRef ->
        CUInt ->
        Ptr CUInt ->
        CUInt ->
        IO Raw.TypeRef

foreign import capi "llvm-c/Core.h LLVMGetTargetExtTypeName"
    getTargetExtTypeName ::
        Raw.TypeRef -> IO CString

foreign import capi "llvm-c/Core.h LLVMGetTargetExtTypeNumTypeParams"
    getTargetExtTypeNumTypeParams ::
        Raw.TypeRef -> IO CUInt

foreign import capi "llvm-c/Core.h LLVMGetTargetExtTypeTypeParam"
    getTargetExtTypeTypeParam ::
        Raw.TypeRef ->
        CUInt ->
        IO Raw.TypeRef

foreign import capi "llvm-c/Core.h LLVMGetTargetExtTypeNumIntParams"
    getTargetExtTypeNumIntParams ::
        Raw.TypeRef -> IO CUInt

foreign import capi "llvm-c/Core.h LLVMGetTargetExtTypeIntParam"
    getTargetExtTypeIntParam ::
        Raw.TypeRef ->
        CUInt ->
        IO CUInt

foreign import capi unsafe "llvm-c/Core.h LLVMInt128TypeInContext"
    int128TypeInContext :: Raw.ContextRef -> IO Raw.TypeRef

foreign import capi unsafe "llvm-c/Core.h LLVMHalfTypeInContext"
    halfTypeInContext :: Raw.ContextRef -> IO Raw.TypeRef

foreign import capi unsafe "llvm-c/Core.h LLVMBFloatTypeInContext"
    bfloatTypeInContext :: Raw.ContextRef -> IO Raw.TypeRef
