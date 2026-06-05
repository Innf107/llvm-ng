{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- Our TH occasionally emits unnecessary 'fromIntegral' calls for simplicity.
-- These will be optimized away but we don't need to be warned about them.
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LLVM.Core (
    -- * Common Operations
    withContext,
    withModule,
    addFunction,
    getNamedFunction,
    appendBasicBlock,
    getParam,
    setIsInBounds,
    isInBounds,
    Target.initializeAllTargets,
    Target.initializeAllTargetInfos,
    Target.initializeNativeTarget,
    verifyModule,
    runPasses,
    runPassesOnFunction,
    defaultPassBuilderOptions,
    PassBuilderOptions (
        verifyEach,
        debugLogging,
        aaPipeline,
        loopInterleaving,
        loopVectorization,
        slpVectorization,
        loopUnrolling,
        forgetAllSCEVInLoopUnroll,
        licmMssaOptCap,
        licmMssaNoAccForPromotionCap,
        callGraphProfile,
        mergeFunctions,
        inlinerThreshold
    ),

    -- * LLVM Types
    functionType,
    intType,
    int1Type,
    int8Type,
    int16Type,
    int32Type,
    int64Type,
    int128Type,
    halfType,
    bfloatType,
    floatType,
    doubleType,
    x86FP80Type,
    fp128Type,
    ppcFP128Type,
    structType,
    arrayType,
    pointerType,
    pointerTypeWithAddressSpace,
    typedPointerType,
    voidType,
    vectorType,
    labelType,
    x86AMXType,
    tokenType,
    metadataType,
    targetExtType,
    getTargetExtTypeName,

    -- ** Operations on Types
    typeIsSized,
    printTypeToText,
    isFunctionVarArg,
    getReturnType,
    getParamTypes,

    -- ** Operations on Values
    typeOf,
    printValueToText,

    -- * Constants
    constInt,
    constIntOfString,
    constReal,
    constRealOfString,
    constIntGetZExtValue,
    getTargetExtTypeNumTypeParams,
    getTargetExtTypeTypeParam,
    getTargetExtTypeNumIntParams,
    getTargetExtTypeIntParam,
    constString,
    isConstantString,
    NullTermination (..),
    getAsString,
    getRawDataValues,
    constStructInContext,
    constArray,
    constDataArray,
    constNamedStruct,
    getAggregateElement,
    constVector,
    constantPtrAuth,
    constNull,
    constAllOnes,
    getUndef,
    getPoison,
    isNull,
    constNullPointer,

    -- * Globals
    addGlobal,
    addGlobalInAddressSpace,
    getNamedGlobal,
    Wrappers.globalAsValue,
    Wrappers.unsafeValueAsGlobal,
    getFirstGlobal,
    getLastGlobal,
    getNextGlobal,
    getPreviousGlobal,
    deleteGlobal,
    getInitializer,
    setInitializer,
    isThreadLocal,
    setThreadLocal,
    isGlobalConstant,
    setGlobalConstant,
    isExternallyInitialized,
    setExternallyInitialized,

    -- ** Operations on Globals
    isDeclaration,
    getLinkage,
    setLinkage,
    getSection,
    setSection,
    getVisibility,
    setVisibility,
    getDLLStorageClass,
    setDLLStorageClass,
    getUnnamedAddress,
    setUnnamedAddress,
    globalGetValueType,
    getAlignment,
    setAlignment,
    globalSetMetadata,
    -- globalAddMetadata,
    globalEraseMetadata,
    globalClearMetadata,
    -- globalAddDebugInfo,
    copyAllMetadata,

    -- ** Operations on function definitions
    setFunctionCallConv,
    deleteFunction,
    hasPersonalityFn,
    getPersonalityFn,
    setPersonalityFn,
    lookupIntrinsicID,
    getIntrinsicID,
    getIntrinsicDeclaration,
    intrinsicGetType,
    intrinsicGetName,
    intrinsicOverloadedName,
    intrinsicIsOverloaded,
    getFunctionCallConv,
    getGC,
    setGC,
    getPrefixData,
    setPrefixData,
    getPrologueData,
    setPrologueData,
    getAttributeCountAtIndex,
    getAttributesAtIndex,
    addAttributeAtIndex,
    getEnumAttributeAtIndex,
    getStringAttributeAtIndex,
    removeEnumAttributeAtIndex,
    removeStringAttributeAtIndex,
    addTargetDependentFunctionAttr,

    -- * Instructions

    -- ** Call Sites and Invocations
    getNumArgOperands,
    setInstructionCallConv,
    getInstructionCallConv,
    setInstrParamAlignment,
    addCallSiteAttribute,
    getCallSiteAttributeCount,
    getCallSiteAttributes,
    getCallSiteEnumAttribute,
    getCallSiteStringAttribute,
    removeCallSiteEnumAttribute,
    removeCallSiteStringAttribute,
    getCalledFunctionType,
    getCalledValue,
    getNumOperandBundles,
    getOperandBundleAtIndex,
    isTailCall,
    setTailCall,
    getTailCallKind,
    setTailCallKind,
    getNormalDest,
    getUnwindDest,
    setNormalDest,
    setUnwindDest,
    getCallBrDefaultDest,
    getCallBrNumIndirectDests,
    getCallBrIndirectDest,

    -- * Calling conventions
    ccallConv,
    fastCallConv,
    coldCallConv,
    gHCCallConv,
    hiPECallConv,
    anyRegCallConv,
    preserveMostCallConv,
    preserveAllCallConv,
    swiftCallConv,
    cXXFASTTLSCallConv,
    tailCallConv,
    x86StdcallCallConv,
    x86FastcallCallConv,
    aRMAPCSCallConv,
    aRMAAPCSCallConv,
    aRMAAPCSVFPCallConv,
    mSP430INTRCallConv,
    x86ThisCallCallConv,
    pTXKernelCallConv,
    pTXDeviceCallConv,
    sPIRFUNCCallConv,
    sPIRKERNELCallConv,
    intelOCLBICallConv,
    x8664SysVCallConv,
    win64CallConv,
    x86VectorCallCallConv,
    hHVMCallConv,
    hHVMCCallConv,
    x86INTRCallConv,
    aVRINTRCallConv,
    aVRSIGNALCallConv,
    aVRBUILTINCallConv,
    aMDGPUVSCallConv,
    aAMDGPUGSCallConv,
    aMDGPUPSCallConv,
    aMDGPUCSCallConv,
    aMDGPUKERNELCallConv,
    x86RegCallCallConv,
    aMDGPUHSCallConv,
    mSP430BUILTINCallConv,
    aMDGPULSCallConv,
    aMDGPUESCallConv,
    customCallingConvention,

    -- * Constant Expressions
    alignOf,
    sizeOf,
    constNeg,
    constNSWNeg,
    constNot,
    constAdd,
    constNSWAdd,
    constNUWAdd,
    constSub,
    constNSWSub,
    constNUWSub,
    constXor,
    constGEP,
    constInBoundsGEP,
    constGEPWithNoWrapFlags,
    constTrunc,
    constPtrToInt,
    constIntToPtr,
    constBitCast,
    constAddrSpaceCast,
    constTruncOrBitCast,
    constPointerCast,
    constExtractElement,
    constInsertElement,
    constShuffleVector,
    blockAddress,
    getBlockAddressFunction,
    getBlockAddressBasicBlock,

    -- * Modules
    setTarget,

    -- * Verification
    verifyModule,
    verifyFunction,
    viewFunctionCFG,
    viewFunctionCFGOnly,

    -- * Opaque Types

    {- | Most of the types exposed by this library are opaque wrappers around the types provided by the underlying LLVM C bindings.

    One notable exception to this is 'FunctionType', which exists in LLVM's C++ API but not in the C API.
    This type is included here for safety reasons (LLVM can segfault when given a non-function 'Type' where a 'FunctionType' is expected).

    In the C++ API, 'FunctionType' is a C++ subtype of 'Type', but here you will have to manually call 'functionTypeAsType'.
    -}
    Module,
    BasicBlock,
    Context,
    Value,
    Global,
    Type,
    Attribute,
    AttributeKind,
    unsafeTypeAsFunctionType,
    functionTypeAsType,
    MetaData,
    FastMathFlags,
    FunctionType,
    Raw.Linkage,
    Raw.Visibility,
    TailCallKind (..),
    OperandBundle,
    CallingConvention,
    VerifierFailureAction (..),

    -- * Debugging
    dumpModule,
    printModuleToFile,
    printModuleToText,

    -- * Other
    IntPredicate (..),
    RealPredicate (..),
    getGEPSourceElementType,
) where

import Control.Exception (bracket, mask_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Foreign qualified as Text.Foreign
import Data.Vector.Storable qualified as Storable
import Data.Vector.Storable.Mutable qualified as Storable.Mutable
import Data.Vector.Strict qualified as Strict
import Foreign (Ptr, Storable (peek), alloca, newForeignPtr, nullPtr, poke)
import Foreign.C (CUInt, withCString)
import GHC.Stack (HasCallStack)
import LLVM.Core.Context
import LLVM.FFI.Core qualified as Raw
import LLVM.FFI.Missing (disposePassBuilderOptions)
import LLVM.FFI.Missing qualified as Missing
import LLVM.Internal.Error (handleErrorRef, withErrorMessage)
import LLVM.Internal.TH (wrapAs, wrapAsPure, wrapDirectly, wrapDirectlyPure)
import LLVM.Internal.Wrappers (
    Attribute,
    BasicBlock (..),
    CallingConvention (MkCallingConvention),
    Context (..),
    FastMathFlags,
    FunctionType (MkFunctionType),
    Global (MkGlobal),
    IntPredicate (..),
    MetaData,
    Module (..),
    OperandBundle,
    PassBuilderOptionsRef,
    RealPredicate (..),
    TailCallKind (..),
    TargetData,
    Type (..),
    Value (..),
    VerifierFailureAction (..),
    functionTypeAsType,
    unsafeTypeAsFunctionType,
    unwrapVerifierFailureAction,
    withTargetMachine,
    withTypeArray,
    withUnsignedArray,
    wrapVerifierFailureAction,
 )
import LLVM.Internal.Wrappers qualified as Wrappers
import LLVM.Target qualified as Target
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

{- | Create a new, empty module in a specific context.

The module is alive during the scope of this continuation.
Accessing after its scope will cause segmentation faults.
-}
withModule :: (?context :: Context, MonadIO io) => Text -> (Module -> io r) -> io r
withModule name cont = do
    let MkContext contextPtr = ?context
    rawModule <- liftIO $ Text.Foreign.withCString name \nameCString -> do
        Raw.moduleCreateWithNameInContext nameCString contextPtr
    cont (MkModule rawModule)

-- | Add a function to a module under a specified name.
wrapDirectly 'Missing.addFunction "Add a function to a module under a specified name."

{- | Obtain a Function value from a Module by its name.

This is a wrapper around LLVMGetNamedFunctionWithLength
-}
wrapAs "getNamedFunction" 'Missing.getNamedFunctionWithLength "Obtain a Function value from a Module by its name.\n\nThis is a wrapper around LLVMGetNamedFunctionWithLength."

{- | Obtain a function type consisting of a specified signature.

The function is defined as a tuple of a list of parameter types, a return Type, and whether the function is variadic.
-}
functionType :: Storable.Vector Type -> Type -> Bool -> FunctionType
functionType parameterTypes (MkType returnType) isVarArg = unsafePerformIO do
    ref <- withTypeArray parameterTypes \ptr size -> do
        Raw.functionType returnType ptr size (Raw.consBool isVarArg)
    pure (unsafeTypeAsFunctionType (MkType ref))

wrapAsPure "intType" 'Raw.intTypeInContext "Construct an integer type with the given number of bits"

wrapAsPure "int1Type" 'Raw.int1TypeInContext "Construct a 1 bit integer type"

wrapAsPure "int8Type" 'Raw.int8TypeInContext "Construct an 8 bit integer type"

wrapAsPure "int16Type" 'Raw.int16TypeInContext "Construct a 16 bit integer type"

wrapAsPure "int32Type" 'Raw.int32TypeInContext "Construct a 32 bit integer type"

wrapAsPure "int64Type" 'Raw.int64TypeInContext "Construct a 64 bit integer type"

wrapAsPure "int128Type" 'Missing.int128TypeInContext "Construct a 128 bit integer type"

wrapAsPure "halfType" 'Missing.halfTypeInContext "Obtain a 16-bit floating point type"

wrapAsPure "bfloatType" 'Missing.bfloatTypeInContext "Obtain a 16-bit brain floating point type from a context"

wrapAsPure "floatType" 'Raw.floatTypeInContext "Obtain a 32-bit floating point type from a context"

wrapAsPure "doubleType" 'Raw.doubleTypeInContext "Obtain a 64-bit floating point type from a context"

wrapAsPure "x86FP80Type" 'Raw.x86FP80TypeInContext "Obtain a 80-bit floating point type (X87) from a context."

wrapAsPure "fp128Type" 'Raw.fp128TypeInContext "Obtain a 128-bit floating point type (112-bit mantissa) from a context."

wrapAsPure "ppcFP128Type" 'Raw.ppcFP128TypeInContext "Obtain a 128-bit floating point type (two 64-bits) from a context"

wrapAsPure "structType" 'Raw.structTypeInContext "Create a new structure type in a context."

wrapAsPure "arrayType" 'Missing.arrayType2 "Create a fixed size array type that refers to a specific type.\n\nThe created type will exist in the context that its element type exists in.\nIn particular, even though this function doesn't have a 'Context' constraint, it is *not* safe to let the returned type escape the scope of its context"

{- | Create a pointer type that points to a defined type.

The created type will exist in the context that its pointee type exists in.

By default, modern versions of LLVM ignore the pointed-to type. (https://llvm.org/docs/OpaquePointers.html#the-opaque-pointer-type
)
For this reason, you will probably want to use 'pointerType' or 'pointerTypeWithAddressSpace' instead.
-}
typedPointerType :: Type -> Word -> Type
typedPointerType (MkType elementType) addressSpace =
    MkType $ unsafePerformIO (Raw.pointerType elementType (fromIntegral addressSpace))

wrapAsPure "voidType" 'Raw.voidTypeInContext ""

{- | Create a generic pointer type.

If you need to support a non-default address space, use 'pointerTypeWithAddressSpace' and if you want to create
    a typed pointer, use 'typedPointerType'.
-}
pointerType :: (?context :: Context) => Type
pointerType = typedPointerType voidType 0

{- | Create a generic pointer type with a specified address space.

If you want to create a typed poniter, use 'typedPointerType'.
-}
pointerTypeWithAddressSpace :: (?context :: Context) => Word -> Type
pointerTypeWithAddressSpace addressSpace = typedPointerType voidType addressSpace

{- | Create a vector type that contains a defined type and has a specific number of elements.

The created type will exist in the context thats its element type exists in.
In particular, even though this function does not have a 'Context' constraint, it is *not* safe to let the returned type escape past the scope of the element's context
-}
vectorType :: Type -> Int -> Type
vectorType (MkType elementType) elementCount =
    MkType $ unsafePerformIO (Raw.vectorType elementType (fromIntegral elementCount))

wrapAsPure "labelType" 'Raw.labelTypeInContext ""

wrapAsPure "x86AMXType" 'Missing.x86AMXTypeInContext ""

wrapAsPure "tokenType" 'Missing.tokenTypeInContext ""

wrapAsPure "metadataType" 'Missing.metadataTypeInContext ""

targetExtType :: (?context :: Context) => Text -> Storable.Vector Type -> Storable.Vector Int -> Type
targetExtType name typeParams intParams = MkType $ do
    let MkContext contextRef = ?context
    unsafePerformIO $
        Text.Foreign.withCString name \nameCString ->
            withTypeArray typeParams \typeParamPtr typeParamLength ->
                withUnsignedArray intParams \intParamPtr intParamLength -> do
                    Missing.targetExtTypeInContext contextRef nameCString typeParamPtr typeParamLength intParamPtr intParamLength

-- | Obtain the name for this target extension type.
getTargetExtTypeName :: Type -> Text
getTargetExtTypeName (MkType typeRef) = unsafePerformIO do
    cstring <- Missing.getTargetExtTypeName typeRef
    Text.Foreign.peekCString cstring

wrapDirectlyPure 'Missing.getTargetExtTypeNumTypeParams "Obtain the number of type parameters for this target extension type."

wrapDirectlyPure 'Missing.getTargetExtTypeTypeParam "Get the type parameter at the given index for the target extension type."

wrapDirectlyPure 'Missing.getTargetExtTypeNumIntParams "Obtain the number of int parameters for this target extension type."

wrapDirectlyPure 'Missing.getTargetExtTypeIntParam "Get the int parameter at the given index for the target extension type."

wrapDirectly 'Missing.addGlobal ""

wrapDirectly 'Missing.addGlobalInAddressSpace ""

getNamedGlobal :: (MonadIO io) => Module -> Text -> io (Maybe Wrappers.Global)
getNamedGlobal (MkModule moduleRef) name = liftIO do
    Text.Foreign.withCStringLen name \(cstring, len) -> do
        value <- Missing.getNamedGlobalWithLength moduleRef cstring (fromIntegral len)
        if value == nullPtr
            then
                pure Nothing
            else
                pure $ Just (Wrappers.MkGlobal value)

wrapDirectly 'Missing.getFirstGlobal ""

wrapDirectly 'Missing.getLastGlobal ""

wrapDirectly 'Missing.getNextGlobal ""

wrapDirectly 'Missing.getPreviousGlobal ""

wrapDirectly 'Missing.deleteGlobal ""

wrapDirectly 'Missing.getInitializer ""

wrapDirectly 'Missing.setInitializer ""

wrapDirectly 'Missing.isThreadLocal ""

wrapDirectly 'Missing.setThreadLocal ""

wrapDirectly 'Missing.isGlobalConstant ""

wrapDirectly 'Missing.setGlobalConstant ""

-- TODO: wrapDirectly 'Missing.getThreadLocalMode ""
-- TODO: wrapDirectly 'Missing.setThreadLocalMode ""

wrapDirectly 'Missing.isExternallyInitialized ""

wrapDirectly 'Missing.setExternallyInitialized ""

wrapDirectly 'Missing.dumpModule "Dump a representation of a module to stderr."

-- | Print a representation of a module to a file.
printModuleToFile :: (MonadIO io) => Module -> OsPath -> io ()
printModuleToFile (MkModule module_) filePath = liftIO do
    -- TODO: use the underlying ShortByteString directly instead of going via string
    filePathString <- OsPath.decodeFS filePath
    withCString filePathString \filePathCString -> do
        withErrorMessage (Just ("printModuleToFile _ \"" <> Text.pack filePathString <> "\"")) \errorMessagePtr -> do
            Missing.printModuleToFile module_ filePathCString errorMessagePtr

printModuleToText :: Module -> Text
printModuleToText (MkModule module_) = unsafePerformIO $ mask_ do
    cstring <- Missing.printModuleToString module_
    result <- Text.Foreign.peekCString cstring
    Raw.disposeMessage cstring
    pure result

instance Show Module where
    show module_ = Text.unpack (printModuleToText module_)

-- | Append a basic block to the end of a function.
appendBasicBlock :: (?context :: Context, MonadIO io) => Value -> Text -> io BasicBlock
appendBasicBlock (MkValue function) name = liftIO $ do
    let MkContext contextRef = ?context
    Text.Foreign.withCString name \cname ->
        MkBlock <$> Raw.appendBasicBlockInContext contextRef function cname

wrapDirectlyPure 'Raw.constInt "Obtain a constant value for an integer type.\n\nThe returned value corresponds to a llvm::ConstantInt."

-- TODO: constIntOfArbitraryPrecision

wrapDirectlyPure 'Raw.constIntOfString "Obtain a constant value for an integer parsed from a string. "

wrapDirectlyPure 'Raw.constReal "Obtain a constant value referring to a double floating point value. "

wrapDirectlyPure 'Raw.constRealOfString "Obtain a constant value referring to a double floating point value. "

wrapDirectlyPure 'Raw.constIntGetZExtValue "Obtain the sign extended value for an integer constant value. "

data NullTermination
    = NullTerminate
    | Don'tNullTerminate

{- | Create a ConstantDataSequential and initialize it with a string.

This wraps LLVMConstStringInContext2.
-}
constString :: (?context :: Context) => Text -> NullTermination -> Value
constString text nullTermination = unsafePerformIO $ do
    let MkContext contextRef = ?context
    Text.Foreign.withCStringLen text \(cstring, length) -> do
        let don'tNullTerminate = case nullTermination of
                NullTerminate -> Raw.false
                Don'tNullTerminate -> Raw.true
        MkValue <$> Missing.constStringInContext2 contextRef cstring (fromIntegral length) don'tNullTerminate

wrapDirectlyPure 'Missing.isConstantString "Returns true if the specified constant is an array of i8. "

-- | Get the given constant data sequential as a 'Text'.
getAsString :: (MonadIO io) => Value -> io Text
getAsString (MkValue valueRef) = liftIO do
    alloca \lengthPtr -> do
        cstring <- Missing.getAsString valueRef lengthPtr
        length <- peek lengthPtr
        Text.Foreign.peekCStringLen (cstring, fromIntegral length)

{- | Get the raw, underlying bytes of the given constant data sequential.

This is the same as 'getAsString' except it works for all constant data sequentials, not just i8 arrays.
-}
getRawDataValues :: (MonadIO io) => Value -> io ByteString
getRawDataValues (MkValue valueRef) = liftIO do
    alloca \lengthPtr -> do
        cstring <- Missing.getRawDataValues valueRef lengthPtr
        length <- peek lengthPtr
        ByteString.packCStringLen (cstring, fromIntegral length)

wrapDirectly 'Raw.constStructInContext "Create an anonymous ConstantStruct with the specified values."

wrapAsPure "constArray" 'Missing.constArray2 "Create a ConstantArray from values.\n\nThis is a wrapper around LLVMConstArray2"

wrapDirectlyPure 'Missing.constDataArray "Create a ConstantDataArray from raw values.\n\nElementTy must be one of i8, i16, i32, i64, half, bfloat, float, or double. Data points to a contiguous buffer of raw values in the host endianness. The element count is inferred from the element type and the data size in bytes."

wrapDirectlyPure 'Raw.constNamedStruct "Create a non-anonymous ConstantStruct from values. "

wrapDirectlyPure 'Missing.getAggregateElement "Get element of a constant aggregate (struct, array or vector) at the specified index. "

wrapDirectlyPure 'Raw.constVector "Create a ConstantVector from values."

wrapDirectlyPure 'Missing.constantPtrAuth "Create a ConstantPtrAuth constant with the given values."

wrapDirectly 'Missing.isInBounds "Check whether the given GEP operator is inbounds."

wrapDirectly 'Missing.setIsInBounds "Set the given GEP instruction to be inbounds or not."

wrapDirectlyPure 'Missing.getGEPSourceElementType "Get the source element type of the given GEP operator."

-- TODO: NoWrap flags

-- TODO: wrapDirectly 'Raw.getConstOpcode ""
wrapDirectly 'Raw.alignOf ""
wrapDirectly 'Raw.sizeOf ""
wrapDirectly 'Raw.constNeg ""
wrapDirectly 'Missing.constNSWNeg ""
wrapDirectly 'Raw.constNot ""
wrapDirectly 'Raw.constAdd ""
wrapDirectly 'Raw.constNSWAdd ""
wrapDirectly 'Raw.constNUWAdd ""
wrapDirectly 'Raw.constSub ""
wrapDirectly 'Raw.constNSWSub ""
wrapDirectly 'Raw.constNUWSub ""
wrapDirectly 'Raw.constXor ""

-- | This is a wrapper around @LLVMConstGEP2@
wrapAs "constGEP" 'Raw.constGEP2 ""

-- | This is a wrapper around @LLVMConstInBoundsGEP2@
wrapAs "constInBoundsGEP" 'Raw.constInBoundsGEP2 ""

wrapDirectly 'Missing.constGEPWithNoWrapFlags ""

wrapDirectly 'Raw.constTrunc ""

wrapDirectly 'Raw.constPtrToInt ""

wrapDirectly 'Raw.constIntToPtr ""

wrapDirectly 'Raw.constBitCast ""

wrapDirectly 'Missing.constAddrSpaceCast ""

wrapDirectly 'Raw.constTruncOrBitCast ""

wrapDirectly 'Raw.constPointerCast ""

wrapDirectly 'Raw.constExtractElement ""

wrapDirectly 'Raw.constInsertElement ""

wrapDirectly 'Raw.constShuffleVector ""

wrapDirectly 'Raw.blockAddress ""

wrapDirectly 'Missing.getBlockAddressFunction "Gets the function associated with a given BlockAddress constant value."

wrapDirectly 'Missing.getBlockAddressBasicBlock "Gets the basic block associated with a given BlockAddress constant value. "

-- We cannot provide LLVMGetGlobalParent since that would need to return an LLVMModuleRef with the same finalizer as all other references to the same module

wrapDirectly 'Missing.isDeclaration ""

wrapDirectly 'Missing.getLinkage ""

wrapDirectly 'Missing.setLinkage ""

wrapDirectly 'Missing.getSection ""

wrapDirectly 'Missing.setSection ""

wrapDirectly 'Missing.getVisibility ""

wrapDirectly 'Missing.setVisibility ""

wrapDirectly 'Missing.getDLLStorageClass ""

wrapDirectly 'Missing.setDLLStorageClass ""

wrapDirectly 'Missing.getUnnamedAddress ""

wrapDirectly 'Missing.setUnnamedAddress ""

wrapDirectly 'Missing.globalGetValueType ""

wrapDirectly 'Missing.getAlignment "Obtain the preferred alignment of the value. "

wrapDirectly 'Missing.setAlignment "Set the preferred alignment of the value."

wrapDirectly 'Missing.globalSetMetadata "Sets a metadata attachment, erasing the existing metadata attachment if it already exists for the given kind."

-- wrapDirectly 'Missing.globalAddMetadata "Adds a metadata attachment."

wrapDirectly 'Missing.globalEraseMetadata "Erases a metadata attachment of the given kind if it exists."

wrapDirectly 'Missing.globalClearMetadata "Removes all metadata attachments from this value."

-- wrapDirectly 'Missing.globalAddDebugInfo "Add debuginfo metadata to this global."

{- | Retrieves an array of metadata entries representing the metadata attached to this value.
Each entry consists of its kind and the actual metadata value.
-}
copyAllMetadata :: (MonadIO io) => Global -> io (Strict.Vector (Int, MetaData))
copyAllMetadata (MkGlobal global) = liftIO do
    alloca \sizePtr -> do
        pointer <- Missing.globalCopyAllMetadata global sizePtr
        size <- peek sizePtr
        vector <- Strict.generateM (fromIntegral size) \i -> do
            kind <- Missing.valueMetadataEntriesGetKind pointer (fromIntegral i)
            metadataRef <- Missing.valueMetadataEntriesGetMetadata pointer (fromIntegral i)
            pure (fromIntegral kind, Wrappers.MkMetaData metadataRef)
        Missing.disposeValueMetadataEntries pointer
        pure vector

wrapDirectlyPure 'Raw.constNull "Obtain a constant value referring to the null instance of a type.\n\nIf you want to create a null /pointer/, use 'constNullPointer' instead."

wrapDirectlyPure 'Raw.constAllOnes "Obtain a constant value referring to the instance of a type consisting of all ones. "

wrapDirectlyPure 'Raw.getUndef "Obtain a constant value referring to an undefined value of a type."

wrapDirectlyPure 'Missing.getPoison "Obtain a constant value referring to a poison value of a type. "

wrapDirectlyPure 'Raw.isNull "Determine whether a value instance is null."

-- | Obtain a constant value of a null pointer
constNullPointer :: (?context :: Context) => Value
constNullPointer = unsafePerformIO do
    let MkType pointerTypeRef = pointerType
    MkValue <$> Raw.constPointerNull pointerTypeRef

wrapDirectlyPure 'Missing.isFunctionVarArg "Returns whether a function type is variadic."

wrapDirectlyPure 'Missing.getReturnType "Obtain the Type this function Type returns."

-- | Obtain the types of a function's parameters.
getParamTypes :: (MonadIO io) => FunctionType -> io (Storable.Vector Type)
getParamTypes functionType = liftIO do
    let MkType typeRef = functionTypeAsType functionType
    paramCount <- Raw.countParamTypes typeRef

    array <- Storable.Mutable.new @_ @Raw.TypeRef (fromIntegral paramCount)

    Storable.Mutable.unsafeWith array \pointer -> do
        Raw.getParamTypes typeRef pointer

    typeRefs <- Storable.unsafeFreeze array

    pure (Storable.unsafeCoerceVector @Raw.TypeRef @Type typeRefs)

-- TODO: LLVMGetTypeKind

wrapDirectlyPure 'Raw.typeIsSized "Whether the type has a known size.\n\nThings that don't have a size are abstract types, labels, and void."

wrapAsPure "printTypeToText" 'Missing.printTypeToString "Return a 'Text' representation of the type."

instance Show Type where
    show :: Type -> String
    show type_ = Text.unpack (printTypeToText type_)

instance Show FunctionType where
    show functionType = show (functionTypeAsType functionType)

wrapDirectly 'Missing.getNumArgOperands "Obtain the argument count for a call instruction.\n\nThis expects a 'Value' that corresponds to a llvm::CallInst, llvm::InvokeInst, or llvm::FuncletPadInst."

wrapDirectly 'Missing.setInstructionCallConv "Set the calling convention for a call instruction.\n\nThis expects a 'Value' that corresponds to a llvm::CallInst or llvm::InvokeInst."

wrapDirectly 'Missing.getInstructionCallConv "Obtain the calling convention for a call instruction.\n\nThis expects a 'Value' that corresponds to a llvm::CallInst or llvm::InvokeInst."

wrapDirectly 'Missing.setInstrParamAlignment ""

wrapDirectly 'Missing.addCallSiteAttribute ""

wrapDirectly 'Missing.getCallSiteAttributeCount ""

getCallSiteAttributes :: (MonadIO io) => Value -> Int -> io (Storable.Vector Attribute)
getCallSiteAttributes value@(MkValue valueRef) attributeIndex = liftIO do
    attributeCount <- getCallSiteAttributeCount value attributeIndex
    vector <- Storable.Mutable.new @_ @Attribute attributeCount
    Storable.Mutable.unsafeWith vector \pointer -> do
        Missing.getCallSiteAttributes valueRef (fromIntegral attributeIndex) (coerce @(Ptr Attribute) @(Ptr Raw.AttributeRef) pointer)
    Storable.unsafeFreeze vector

wrapDirectly 'Missing.getCallSiteEnumAttribute ""

wrapDirectly 'Missing.getCallSiteStringAttribute ""

wrapDirectly 'Missing.removeCallSiteEnumAttribute ""

wrapDirectly 'Missing.removeCallSiteStringAttribute ""

wrapDirectly 'Missing.getCalledFunctionType "Obtain the function type called by this instruction. "

wrapDirectly 'Raw.getCalledValue "Obtain the 'Value' of to the function invoked by this instruction. "

wrapDirectly 'Missing.getNumOperandBundles "Obtain the number of operand bundles attached to this instruction.\n\nThis only works on llvm::CallInst and llvm::InvokeInst instructions."

wrapDirectly 'Missing.getOperandBundleAtIndex "Obtain the operand bundle attached to this instruction at the given index.\n\nThis only works on llvm::CallInst and llvm::InvokeInst instructions."

wrapDirectly 'Raw.isTailCall "Obtain whether a call instruction is a tail call.\n\nThis only works on llvm::CallInst instructions."

wrapDirectly 'Raw.setTailCall "Set whether a call instruction is a tail call.\n\nThis only works on llvm::CallInst instructions."

wrapDirectly 'Missing.getTailCallKind "Obtain a tail call kind of the call instruction. "

wrapDirectly 'Missing.setTailCallKind "Set the call kind of the call instruction. "

wrapDirectly 'Missing.getNormalDest "Return the normal destination basic block.\n\nThis only works on llvm::InvokeInst instructions."

wrapDirectly 'Missing.getUnwindDest "Return the unwind destination basic block. .\n\nThis only works on llvm::InvokeInst instructions."

wrapDirectly 'Missing.setNormalDest "Set the normal destination basic block.\n\nThis only works on llvm::InvokeInst instructions."

wrapDirectly 'Missing.setUnwindDest "Set the unwind destination basic block. .\n\nThis only works on llvm::InvokeInst instructions."

wrapDirectly 'Missing.getCallBrDefaultDest "Get the default destination of a CallBr instruction."

wrapDirectly 'Missing.getCallBrNumIndirectDests "Get the number of indirect destinations of a CallBr instruction."

wrapDirectly 'Missing.getCallBrIndirectDest "Get the indirect destination of a CallBr instruction at the given index."

wrapDirectly 'Missing.setFunctionCallConv "Set the calling convention of a function."

ccallConv :: CallingConvention
ccallConv = MkCallingConvention 0
fastCallConv :: CallingConvention
fastCallConv = MkCallingConvention 8
coldCallConv :: CallingConvention
coldCallConv = MkCallingConvention 9
gHCCallConv :: CallingConvention
gHCCallConv = MkCallingConvention 10
hiPECallConv :: CallingConvention
hiPECallConv = MkCallingConvention 11
anyRegCallConv :: CallingConvention
anyRegCallConv = MkCallingConvention 13
preserveMostCallConv :: CallingConvention
preserveMostCallConv = MkCallingConvention 14
preserveAllCallConv :: CallingConvention
preserveAllCallConv = MkCallingConvention 15
swiftCallConv :: CallingConvention
swiftCallConv = MkCallingConvention 16
cXXFASTTLSCallConv :: CallingConvention
cXXFASTTLSCallConv = MkCallingConvention 17
tailCallConv :: CallingConvention
tailCallConv = MkCallingConvention 18
x86StdcallCallConv :: CallingConvention
x86StdcallCallConv = MkCallingConvention 64
x86FastcallCallConv :: CallingConvention
x86FastcallCallConv = MkCallingConvention 65
aRMAPCSCallConv :: CallingConvention
aRMAPCSCallConv = MkCallingConvention 66
aRMAAPCSCallConv :: CallingConvention
aRMAAPCSCallConv = MkCallingConvention 67
aRMAAPCSVFPCallConv :: CallingConvention
aRMAAPCSVFPCallConv = MkCallingConvention 68
mSP430INTRCallConv :: CallingConvention
mSP430INTRCallConv = MkCallingConvention 69
x86ThisCallCallConv :: CallingConvention
x86ThisCallCallConv = MkCallingConvention 70
pTXKernelCallConv :: CallingConvention
pTXKernelCallConv = MkCallingConvention 71
pTXDeviceCallConv :: CallingConvention
pTXDeviceCallConv = MkCallingConvention 72
sPIRFUNCCallConv :: CallingConvention
sPIRFUNCCallConv = MkCallingConvention 75
sPIRKERNELCallConv :: CallingConvention
sPIRKERNELCallConv = MkCallingConvention 76
intelOCLBICallConv :: CallingConvention
intelOCLBICallConv = MkCallingConvention 77
x8664SysVCallConv :: CallingConvention
x8664SysVCallConv = MkCallingConvention 78
win64CallConv :: CallingConvention
win64CallConv = MkCallingConvention 79
x86VectorCallCallConv :: CallingConvention
x86VectorCallCallConv = MkCallingConvention 80
hHVMCallConv :: CallingConvention
hHVMCallConv = MkCallingConvention 81
hHVMCCallConv :: CallingConvention
hHVMCCallConv = MkCallingConvention 82
x86INTRCallConv :: CallingConvention
x86INTRCallConv = MkCallingConvention 83
aVRINTRCallConv :: CallingConvention
aVRINTRCallConv = MkCallingConvention 84
aVRSIGNALCallConv :: CallingConvention
aVRSIGNALCallConv = MkCallingConvention 85
aVRBUILTINCallConv :: CallingConvention
aVRBUILTINCallConv = MkCallingConvention 86
aMDGPUVSCallConv :: CallingConvention
aMDGPUVSCallConv = MkCallingConvention 87
aAMDGPUGSCallConv :: CallingConvention
aAMDGPUGSCallConv = MkCallingConvention 88
aMDGPUPSCallConv :: CallingConvention
aMDGPUPSCallConv = MkCallingConvention 89
aMDGPUCSCallConv :: CallingConvention
aMDGPUCSCallConv = MkCallingConvention 90
aMDGPUKERNELCallConv :: CallingConvention
aMDGPUKERNELCallConv = MkCallingConvention 91
x86RegCallCallConv :: CallingConvention
x86RegCallCallConv = MkCallingConvention 92
aMDGPUHSCallConv :: CallingConvention
aMDGPUHSCallConv = MkCallingConvention 93
mSP430BUILTINCallConv :: CallingConvention
mSP430BUILTINCallConv = MkCallingConvention 94
aMDGPULSCallConv :: CallingConvention
aMDGPULSCallConv = MkCallingConvention 95
aMDGPUESCallConv :: CallingConvention
aMDGPUESCallConv = MkCallingConvention 96

customCallingConvention :: CUInt -> CallingConvention
customCallingConvention = MkCallingConvention

wrapDirectly 'Raw.deleteFunction "Remove a function from its containing module and deletes it."

wrapDirectly 'Missing.hasPersonalityFn "Check whether the given function has a personality function."

wrapDirectly 'Missing.getPersonalityFn "Obtain the personality function attached to the function."

wrapDirectly 'Missing.setPersonalityFn "Set the personality function attached to the function."

wrapDirectly 'Missing.lookupIntrinsicID "Obtain the intrinsic ID number which matches the given function name."

wrapDirectly 'Missing.getIntrinsicID "Obtain the ID number from a function instance."

wrapDirectly 'Missing.getIntrinsicDeclaration "Get or insert the declaration of an intrinsic.\n\nFor overloaded intrinsics, parameter types must be provided to uniquely identify an overload."

wrapDirectly 'Missing.intrinsicGetType "Retrieves the type of an intrinsic.\n\nFor overloaded intrinsics, parameter types must be provided to uniquely identify an overload."

-- TODO: error handling

-- | Retrieves the name of an intrinsic.
intrinsicGetName :: Int -> Text
intrinsicGetName intrinsicID = unsafePerformIO do
    alloca \sizePtr -> do
        cstring <- Missing.intrinsicGetName (fromIntegral intrinsicID) sizePtr
        size <- peek sizePtr
        Text.Foreign.peekCStringLen (cstring, fromIntegral size)

{- | Copies the name of an overloaded intrinsic identified by a given list of parameter types.

This uses LLVMIntrinsicCopyOverloadedName2 internally.
-}
intrinsicOverloadedName :: Module -> Int -> Storable.Vector Type -> Text
intrinsicOverloadedName module_ intrinsicID arguments = unsafePerformIO do
    let MkModule moduleRef = module_
    withTypeArray arguments \argumentPtr argumentSize -> do
        alloca \sizePtr -> do
            cstring <- Missing.intrinsicCopyOverloadedName2 moduleRef (fromIntegral intrinsicID) argumentPtr (fromIntegral argumentSize) sizePtr
            size <- peek sizePtr
            text <- Text.Foreign.peekCStringLen (cstring, fromIntegral size)

            Raw.disposeMessage cstring
            pure text

wrapDirectly 'Missing.intrinsicIsOverloaded "Obtain if the intrinsic identified by the given ID is overloaded."

wrapDirectly 'Missing.getFunctionCallConv "Obtain the calling function of a function."

wrapDirectly 'Missing.getGC "Obtain the name of the garbage collector to use during code generation."

wrapDirectly 'Missing.setGC "Define the garbage collector to use during code generation. "

{- | Gets the prefix data associated with a function.

Only valid on functions.
See https://llvm.org/docs/LangRef.html#prefix-data
-}
getPrefixData :: Value -> IO (Maybe Value)
getPrefixData (MkValue function) =
    fmap Raw.deconsBool (Missing.hasPrefixData function) >>= \case
        False -> pure Nothing
        True -> do
            data_ <- Missing.getPrefixData function
            pure (Just (MkValue data_))

wrapDirectly 'Missing.setPrefixData "Sets the prefix data for the function."

getPrologueData :: Value -> IO (Maybe Value)
getPrologueData (MkValue function) =
    fmap Raw.deconsBool (Missing.hasPrologueData function) >>= \case
        False -> pure Nothing
        True -> do
            data_ <- Missing.getPrologueData function
            pure (Just (MkValue data_))

wrapDirectly 'Missing.setPrologueData "Sets the prologue data for the function.\n\nOnly valid on functions. See https://llvm.org/docs/LangRef.html#prologue-data"

wrapDirectly 'Missing.getAttributeCountAtIndex ""

getAttributesAtIndex :: Value -> Int -> IO (Storable.Vector Attribute)
getAttributesAtIndex (MkValue function) attributeIndex = do
    count <- Missing.getAttributeCountAtIndex function (fromIntegral attributeIndex)
    vector <- Storable.Mutable.new (fromIntegral count)

    Storable.Mutable.unsafeWith vector \pointer -> Missing.getAttributesAtIndex function (fromIntegral attributeIndex) (coerce pointer)

    Storable.unsafeFreeze vector

wrapDirectly 'Missing.addAttributeAtIndex "Add an attribute to a function."

wrapDirectly 'Missing.getEnumAttributeAtIndex ""

wrapDirectly 'Missing.getStringAttributeAtIndex ""

wrapDirectly 'Missing.removeEnumAttributeAtIndex ""

wrapDirectly 'Missing.removeStringAttributeAtIndex ""

wrapDirectly 'Missing.addTargetDependentFunctionAttr ""

wrapDirectly 'Missing.setTarget "Set the target triple for a module."

verifyModule :: (MonadIO io) => Module -> io ()
verifyModule module_ = liftIO $ do
    let MkModule moduleRef = module_
    withErrorMessage (Just "verifyModule") (Missing.verifyModule moduleRef (unwrapVerifierFailureAction ReturnStatusAction))

wrapDirectly 'Missing.verifyFunction ""

wrapDirectly 'Missing.viewFunctionCFG ""

wrapDirectly 'Missing.viewFunctionCFGOnly ""

wrapDirectlyPure 'Missing.typeOf "Obtain the type of a value. "

wrapAsPure "printValueToText" 'Missing.printValueToString "Return a string representation of the value. "

instance Show Value where
    show value = Text.unpack (printValueToText value)

getParam :: (HasCallStack) => Value -> Int -> Value
getParam (MkValue function) index = unsafePerformIO do
    parameterCount <- Raw.countParams function
    if index >= 0 && index < fromIntegral parameterCount
        then
            MkValue <$> Raw.getParam function (fromIntegral index)
        else
            error $ "getParam: Index " <> show index <> " out of bounds for a function with " <> show parameterCount <> " parameters"

data PassBuilderOptions = MkPassBuilderOptions
    { verifyEach :: Maybe Bool
    , debugLogging :: Maybe Bool
    , aaPipeline :: Maybe Text
    , loopInterleaving :: Maybe Bool
    , loopVectorization :: Maybe Bool
    , slpVectorization :: Maybe Bool
    , loopUnrolling :: Maybe Bool
    , forgetAllSCEVInLoopUnroll :: Maybe Bool
    , licmMssaOptCap :: Maybe Int
    , licmMssaNoAccForPromotionCap :: Maybe Int
    , callGraphProfile :: Maybe Bool
    , mergeFunctions :: Maybe Bool
    , inlinerThreshold :: Maybe Int
    }
defaultPassBuilderOptions :: PassBuilderOptions
defaultPassBuilderOptions =
    MkPassBuilderOptions
        { verifyEach = Nothing
        , debugLogging = Nothing
        , aaPipeline = Nothing
        , loopInterleaving = Nothing
        , loopVectorization = Nothing
        , slpVectorization = Nothing
        , loopUnrolling = Nothing
        , forgetAllSCEVInLoopUnroll = Nothing
        , licmMssaOptCap = Nothing
        , licmMssaNoAccForPromotionCap = Nothing
        , callGraphProfile = Nothing
        , mergeFunctions = Nothing
        , inlinerThreshold = Nothing
        }

withPassBuilderOptions :: PassBuilderOptions -> (PassBuilderOptionsRef -> IO a) -> IO a
withPassBuilderOptions options cont =
    bracket
        Missing.createPassBuilderOptions
        disposePassBuilderOptions
        \optionsRef -> do
            for_ options.verifyEach \verifyEach -> Missing.passBuilderOptionsSetVerifyEach optionsRef (Raw.consBool verifyEach)
            for_ options.debugLogging \debugLogging -> Missing.passBuilderOptionsSetDebugLogging optionsRef (Raw.consBool debugLogging)
            for_ options.aaPipeline \aaPipeline -> Text.Foreign.withCString aaPipeline $ Missing.passBuilderOptionsSetAAPipeline optionsRef
            for_ options.loopInterleaving \loopInterleaving -> Missing.passBuilderOptionsSetLoopInterleaving optionsRef (Raw.consBool loopInterleaving)
            for_ options.loopVectorization \loopVectorization -> Missing.passBuilderOptionsSetLoopVectorization optionsRef (Raw.consBool loopVectorization)
            for_ options.slpVectorization \slpVectorization -> Missing.passBuilderOptionsSetSLPVectorization optionsRef (Raw.consBool slpVectorization)
            for_ options.loopUnrolling \loopUnrolling -> Missing.passBuilderOptionsSetLoopUnrolling optionsRef (Raw.consBool loopUnrolling)
            for_ options.forgetAllSCEVInLoopUnroll \forgetAllSCEVInLoopUnroll -> Missing.passBuilderOptionsSetForgetAllSCEVInLoopUnroll optionsRef (Raw.consBool forgetAllSCEVInLoopUnroll)
            for_ options.licmMssaOptCap \licmMssaOptCap -> Missing.passBuilderOptionsSetLicmMssaOptCap optionsRef (fromIntegral licmMssaOptCap)
            for_ options.licmMssaNoAccForPromotionCap \licmMssaNoAccForPromotionCap -> Missing.passBuilderOptionsSetLicmMssaNoAccForPromotionCap optionsRef (fromIntegral licmMssaNoAccForPromotionCap)
            for_ options.callGraphProfile \callGraphProfile -> Missing.passBuilderOptionsSetCallGraphProfile optionsRef (Raw.consBool callGraphProfile)
            for_ options.mergeFunctions \mergeFunctions -> Missing.passBuilderOptionsSetMergeFunctions optionsRef (Raw.consBool mergeFunctions)
            for_ options.inlinerThreshold \inlinerThreshold -> Missing.passBuilderOptionsSetInlinerThreshold optionsRef (fromIntegral inlinerThreshold)
            cont optionsRef

{- | Construct and run a set of passes over a module.

This function takes a string with the passes that should be used.
The format of this string is the same as opt's -passes argument for the new pass manager.
Individual passes may be specified, separated by commas.
Full pipelines may also be invoked using default<O3> and friends.

See opt for full reference of the Passes format.
-}
runPasses :: Module -> Text -> Maybe Wrappers.TargetMachine -> PassBuilderOptions -> IO ()
runPasses (MkModule module_) passes targetMachine options = do
    withPassBuilderOptions options \optionsRef -> do
        Text.Foreign.withCString passes \passesCString -> do
            withMaybeTargetMachine targetMachine \targetMachineRef -> do
                errorRef <- Missing.runPasses module_ passesCString targetMachineRef optionsRef
                handleErrorRef (Just "runPasses") errorRef

withMaybeTargetMachine :: Maybe Wrappers.TargetMachine -> (Wrappers.TargetMachineRef -> IO a) -> IO a
withMaybeTargetMachine maybe cont = case maybe of
    Nothing -> cont nullPtr
    Just targetMachine -> withTargetMachine targetMachine cont

{- |  Construct and run a set of passes over a function.

This function behaves the same as LLVMRunPasses, but operates on a single function instead of an entire module.
-}
runPassesOnFunction :: Value -> Text -> Maybe Wrappers.TargetMachine -> PassBuilderOptions -> IO ()
runPassesOnFunction (MkValue function_) passes targetMachine options = do
    withPassBuilderOptions options \optionsRef -> do
        Text.Foreign.withCString passes \passesCString -> do
            withMaybeTargetMachine targetMachine \targetMachineRef -> do
                errorRef <- Missing.runPassesOnFunction function_ passesCString targetMachineRef optionsRef
                handleErrorRef (Just "runPassesOnFunction") errorRef
