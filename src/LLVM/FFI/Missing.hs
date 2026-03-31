{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -optc -Wno-discarded-qualifiers #-}
-- Workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/26852
{-# OPTIONS_GHC -optc -Wno-incompatible-pointer-types #-}

module LLVM.FFI.Missing where

import Data.Word (Word64)
import Foreign.C (CInt (..), CSize (..), CString, CUInt (..))
import Foreign.Ptr (Ptr)
import LLVM.FFI.Core qualified as Raw
import LLVM.Internal.Wrappers (
    CStringLenAsByteString,
    DiagnosticInfoRef,
    FunctionTypeRef,
    GlobalRef,
    IntPredicate,
    MetaDataRef,
    OperandBundleRef,
    RawDLLStorageClass,
    RawFastMathFlags,
    RawGEPNoWrapFlags,
    RawIntPredicate,
    RawLinkage,
    RawRealPredicate,
    RawUnnamedAddr,
    RawVisibility,
    UnownedCString,
    ValueMetadataEntriesRef,
 )

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

foreign import capi unsafe "llvm-c/Core.h LLVMIsInBounds"
    isInBounds :: Raw.ValueRef -> IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMSetIsInBounds"
    setIsInBounds :: Raw.ValueRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetGEPSourceElementType"
    getGEPSourceElementType :: Raw.ValueRef -> IO Raw.TypeRef

foreign import capi unsafe "llvm-c/Core.h LLVMContextShouldDiscardValueNames"
    contextShouldDiscardValueNames :: Raw.ContextRef -> IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMContextSetDiscardValueNames"
    contextSetDiscardValueNames :: Raw.ContextRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetDiagInfoDescription"
    getDiagInfoDescription :: DiagnosticInfoRef -> IO CString

foreign import capi unsafe "llvm-c/Core.h LLVMGetSyncScopeID"
    getSyncScopeId :: Raw.ContextRef -> CString -> CUInt -> IO CUInt

foreign import capi unsafe "llvm-c/Core.h LLVMCreateTypeAttribute"
    createTypeAttribute :: Raw.ContextRef -> CUInt -> Raw.TypeRef -> IO Raw.AttributeRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetTypeAttributeValue"
    getTypeAttributeValue :: Raw.AttributeRef -> IO Raw.TypeRef

-- foreign import capi unsafe "llvm-c/Core.h LLVMCreateConstantRangeAttribute"
--    createConstantRangeAttribute :: Raw.ContextRef -> CUInt -> CUInt -> Ptr CULLong -> Ptr CULLong -> IO Raw.AttributeRef

foreign import capi unsafe "llvm-c/Core.h LLVMIsTypeAttribute"
    isTypeAttribute :: Raw.AttributeRef -> IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMGetTypeByName2"
    getTypeByName2 :: Raw.ContextRef -> CString -> IO Raw.TypeRef

foreign import capi unsafe "llvm-c/Core.h LLVMAddGlobal"
    addGlobal :: Raw.ModuleRef -> Raw.TypeRef -> CString -> IO GlobalRef

foreign import capi unsafe "llvm-c/Core.h LLVMAddGlobalInAddressSpace"
    addGlobalInAddressSpace :: Raw.ModuleRef -> Raw.TypeRef -> CString -> CUInt -> IO GlobalRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetNamedGlobalWithLength"
    getNamedGlobalWithLength :: Raw.ModuleRef -> CString -> CSize -> IO GlobalRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetFirstGlobal"
    getFirstGlobal :: Raw.ModuleRef -> IO GlobalRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetLastGlobal"
    getLastGlobal :: Raw.ModuleRef -> IO GlobalRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetNextGlobal"
    getNextGlobal :: GlobalRef -> IO GlobalRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetPreviousGlobal"
    getPreviousGlobal :: GlobalRef -> IO GlobalRef

foreign import capi unsafe "llvm-c/Core.h LLVMDeleteGlobal"
    deleteGlobal :: GlobalRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetInitializer"
    getInitializer :: GlobalRef -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMSetInitializer"
    setInitializer :: GlobalRef -> Raw.ValueRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMIsThreadLocal"
    isThreadLocal :: GlobalRef -> IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMSetThreadLocal"
    setThreadLocal :: GlobalRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMIsExternallyInitialized"
    isExternallyInitialized :: GlobalRef -> IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMSetExternallyInitialized"
    setExternallyInitialized :: GlobalRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMIsGlobalConstant"
    isGlobalConstant :: GlobalRef -> IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMSetGlobalConstant"
    setGlobalConstant :: GlobalRef -> Raw.Bool -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMConstStringInContext2"
    constStringInContext2 :: Raw.ContextRef -> CString -> CSize -> Raw.Bool -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMIsConstantString"
    isConstantString :: Raw.ValueRef -> IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMGetAsString"
    getAsString :: Raw.ValueRef -> Ptr CSize -> IO CString

foreign import capi unsafe "llvm-c/Core.h LLVMGetRawDataValues"
    getRawDataValues :: Raw.ValueRef -> Ptr CSize -> IO CString

foreign import capi unsafe "llvm-c/Core.h LLVMConstArray2"
    constArray2 :: Raw.TypeRef -> Ptr Raw.ValueRef -> Word64 -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMConstDataArray"
    constDataArray :: Raw.TypeRef -> CStringLenAsByteString -> CSize -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetAggregateElement"
    getAggregateElement :: Raw.ValueRef -> CUInt -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMConstantPtrAuth"
    constantPtrAuth :: Raw.ValueRef -> Raw.ValueRef -> Raw.ValueRef -> Raw.ValueRef -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMConstNSWNeg"
    constNSWNeg :: Raw.ValueRef -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMConstGEPWithNoWrapFlags"
    constGEPWithNoWrapFlags :: Raw.TypeRef -> Raw.ValueRef -> Ptr Raw.ValueRef -> CUInt -> RawGEPNoWrapFlags -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMConstAddrSpaceCast"
    constAddrSpaceCast :: Raw.ValueRef -> Raw.TypeRef -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetBlockAddressFunction"
    getBlockAddressFunction :: Raw.ValueRef -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetBlockAddressBasicBlock"
    getBlockAddressBasicBlock :: Raw.ValueRef -> IO Raw.ValueRef

foreign import capi unsafe "llvm-c/Core.h LLVMIsDeclaration"
    isDeclaration :: GlobalRef -> IO Raw.Bool

foreign import capi unsafe "llvm-c/Core.h LLVMGetLinkage"
    getLinkage :: GlobalRef -> IO RawLinkage

foreign import capi unsafe "llvm-c/Core.h LLVMSetLinkage"
    setLinkage :: GlobalRef -> RawLinkage -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetSection"
    getSection :: GlobalRef -> IO UnownedCString

foreign import capi unsafe "llvm-c/Core.h LLVMSetSection"
    setSection :: GlobalRef -> CString -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetVisibility"
    getVisibility :: GlobalRef -> IO RawVisibility

foreign import capi unsafe "llvm-c/Core.h LLVMSetVisibility"
    setVisibility :: GlobalRef -> RawVisibility -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetDLLStorageClass"
    getDLLStorageClass :: GlobalRef -> IO RawDLLStorageClass

foreign import capi unsafe "llvm-c/Core.h LLVMSetDLLStorageClass"
    setDLLStorageClass :: GlobalRef -> RawDLLStorageClass -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGetUnnamedAddress"
    getUnnamedAddress :: GlobalRef -> IO RawUnnamedAddr

foreign import capi unsafe "llvm-c/Core.h LLVMSetUnnamedAddress"
    setUnnamedAddress :: GlobalRef -> RawUnnamedAddr -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGlobalGetValueType"
    globalGetValueType :: GlobalRef -> IO Raw.TypeRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetAlignment"
    getAlignment :: GlobalRef -> IO CUInt

foreign import capi unsafe "llvm-c/Core.h LLVMSetAlignment"
    setAlignment :: GlobalRef -> CUInt -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGlobalSetMetadata"
    globalSetMetadata :: GlobalRef -> CUInt -> MetaDataRef -> IO ()

-- TODO: somehow this function doesn't actually seem to exist for me?
-- It isn't mentioned in any of the recent release notes however (22, 21, 20, 19, 18, 17, 16)
-- foreign import capi unsafe "llvm-c/Core.h LLVMGlobalAddMetadata"
--    globalAddMetadata :: GlobalRef -> CUInt -> MetaDataRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGlobalEraseMetadata"
    globalEraseMetadata :: GlobalRef -> CUInt -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGlobalClearMetadata"
    globalClearMetadata :: GlobalRef -> IO ()

-- TODO: same problem as globalAddMetadata
-- foreign import capi unsafe "llvm-c/Core.h LLVMGlobalAddDebugInfo"
--     globalAddDebugInfo :: GlobalRef -> MetaDataRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMGlobalCopyAllMetadata"
    globalCopyAllMetadata :: GlobalRef -> Ptr CSize -> IO ValueMetadataEntriesRef

foreign import capi unsafe "llvm-c/Core.h LLVMDisposeValueMetadataEntries"
    disposeValueMetadataEntries :: ValueMetadataEntriesRef -> IO ()

foreign import capi unsafe "llvm-c/Core.h LLVMValueMetadataEntriesGetKind"
    valueMetadataEntriesGetKind :: ValueMetadataEntriesRef -> CUInt -> IO CUInt

foreign import capi unsafe "llvm-c/Core.h LLVMValueMetadataEntriesGetMetadata"
    valueMetadataEntriesGetMetadata :: ValueMetadataEntriesRef -> CUInt -> IO MetaDataRef

foreign import capi unsafe "llvm-c/Core.h LLVMGetPoison"
    getPoison :: Raw.TypeRef -> IO Raw.ValueRef
