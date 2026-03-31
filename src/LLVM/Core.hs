{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- Our TH occasionally emits unnecessary 'fromIntegral' calls for simplicity.
-- These will be optimized away but we don't need to be warned about them.
{-# OPTIONS_GHC -Wno-identities #-}

module LLVM.Core (
    -- * Common Operations
    contextCreate,
    moduleCreateWithName,
    addFunction,
    getNamedFunction,
    appendBasicBlock,
    getParam,
    setIsInBounds,
    isInBounds,

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
    ppcfp128Type,
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
    unsafeTypeAsFunctionType,
    functionTypeAsType,
    MetaData,
    FastMathFlags,
    FunctionType,
    Raw.Linkage,
    Raw.Visibility,

    -- * Debugging
    dumpModule,
    printModuleToFile,
    printModuleToString,

    -- * Other
    IntPredicate (..),
    RealPredicate (..),
    getGEPSourceElementType,
) where

import Control.Exception (mask_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Foreign qualified as Text.Foreign
import Data.Vector.Storable qualified as Storable
import Data.Vector.Strict qualified as Strict
import Foreign (Ptr, Storable (peek), alloca, nullPtr, poke)
import Foreign.C (CUInt, withCString)
import Foreign.Concurrent (newForeignPtr)
import LLVM.Core.Context
import LLVM.FFI.Core qualified as Raw
import LLVM.FFI.Missing qualified as Missing
import LLVM.Internal.Error (withErrorMessage)
import LLVM.Internal.TH (wrapAs, wrapDirectly, wrapDirectlyPure)
import LLVM.Internal.Wrappers (
    BasicBlock (..),
    Context (..),
    FastMathFlags,
    FunctionType (MkFunctionType),
    Global (MkGlobal),
    IntPredicate (..),
    MetaData,
    Module (..),
    RealPredicate (..),
    Type (..),
    Value (..),
    functionTypeAsType,
    unsafeTypeAsFunctionType,
    withContext,
    withModule,
    withTypeArray,
    withUnsignedArray,
 )
import LLVM.Internal.Wrappers qualified as Wrappers
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

{- | Create a new, empty module in a specific context.

The mdoule has an attached finalizer and will automatically be garbage collected.
-}
moduleCreateWithName :: (?context :: Context, MonadIO io) => Text -> io Module
moduleCreateWithName name = liftIO do
    rawModule <- Text.Foreign.withCString name \nameCString -> do
        withContext ?context \contextPtr -> do
            Raw.moduleCreateWithNameInContext nameCString contextPtr
    MkModule <$> newForeignPtr rawModule (Raw.disposeModule rawModule)

-- | Add a function to a module under a specified name.
addFunction :: (MonadIO io) => Module -> Text -> FunctionType -> io Value
addFunction module_ name functionType = liftIO do
    let MkType type_ = functionTypeAsType functionType
    function <- Text.Foreign.withCString name \nameCStr -> do
        withModule module_ \modulePtr -> do
            Raw.addFunction modulePtr nameCStr type_
    pure (MkValue function)

{- | Obtain a Function value from a Module by its name.

This is a wrapper around LLVMGetNamedFunctionWithLength
-}
getNamedFunction :: (MonadIO io) => Module -> Text -> io (Maybe Value)
getNamedFunction module_ name = liftIO do
    withModule module_ \moduleRef ->
        Text.Foreign.withCStringLen name \(cstring, length) -> do
            pointer <- Missing.getNamedFunctionWithLength moduleRef cstring (fromIntegral length)
            if pointer == nullPtr
                then pure Nothing
                else pure (Just (MkValue pointer))

{- | Obtain a function type consisting of a specified signature.

The function is defined as a tuple of a list of parameter types, a return Type, and whether the function is variadic.
-}
functionType :: Storable.Vector Type -> Type -> Bool -> FunctionType
functionType parameterTypes (MkType returnType) isVarArg = unsafePerformIO do
    ref <- withTypeArray parameterTypes \ptr size -> do
        Raw.functionType returnType ptr size (Raw.consBool isVarArg)
    pure (unsafeTypeAsFunctionType (MkType ref))

-- | Construct an integer type with the given number of bits
intType :: (?context :: Context) => Int -> Type
intType numBits = unsafePerformIO do
    ref <- withContext ?context \contextPtr -> do
        Raw.intTypeInContext contextPtr (fromIntegral numBits)
    pure (MkType ref)

-- | Construct a 1 bit integer type
int1Type :: (?context :: Context) => Type
int1Type = unsafePerformIO do
    ref <- withContext ?context \contextPtr -> do
        Raw.int1TypeInContext contextPtr
    pure (MkType ref)

-- | Construct an 8 bit integer type
int8Type :: (?context :: Context) => Type
int8Type = unsafePerformIO do
    ref <- withContext ?context \contextPtr -> do
        Raw.int8TypeInContext contextPtr
    pure (MkType ref)

-- | Construct a 16 bit integer type
int16Type :: (?context :: Context) => Type
int16Type = unsafePerformIO do
    ref <- withContext ?context \contextPtr -> do
        Raw.int16TypeInContext contextPtr
    pure (MkType ref)

-- | Construct a 32 bit integer type
int32Type :: (?context :: Context) => Type
int32Type = unsafePerformIO do
    ref <- withContext ?context \contextPtr -> do
        Raw.int32TypeInContext contextPtr
    pure (MkType ref)

-- | Construct a 64 bit integer type
int64Type :: (?context :: Context) => Type
int64Type = unsafePerformIO do
    ref <- withContext ?context \contextPtr -> do
        Raw.int64TypeInContext contextPtr
    pure (MkType ref)

-- | Construct a 128 bit integer type
int128Type :: (?context :: Context) => Type
int128Type = unsafePerformIO do
    MkType <$> withContext ?context Missing.int128TypeInContext

-- | Obtain a 16-bit floating point type from a context.
halfType :: (?context :: Context) => Type
halfType = unsafePerformIO do
    MkType <$> withContext ?context Missing.halfTypeInContext

-- | Obtain a 16-bit brain floating point type from a context.
bfloatType :: (?context :: Context) => Type
bfloatType = unsafePerformIO do
    MkType <$> withContext ?context Missing.bfloatTypeInContext

-- | Obtain a 32-bit floating point type from a context.
floatType :: (?context :: Context) => Type
floatType = unsafePerformIO do
    MkType <$> withContext ?context Raw.floatTypeInContext

-- | Obtain a 64-bit floating point type from a context.
doubleType :: (?context :: Context) => Type
doubleType = unsafePerformIO do
    MkType <$> withContext ?context Raw.doubleTypeInContext

-- | Obtain a 80-bit floating point type (X87) from a context.
x86FP80Type :: (?context :: Context) => Type
x86FP80Type = unsafePerformIO do
    MkType <$> withContext ?context Raw.x86FP80TypeInContext

-- | Obtain a 128-bit floating point type (112-bit mantissa) from a context.
fp128Type :: (?context :: Context) => Type
fp128Type = unsafePerformIO do
    MkType <$> withContext ?context Raw.fp128TypeInContext

-- | Obtain a 128-bit floating point type (two 64-bits) from a context.
ppcfp128Type :: (?context :: Context) => Type
ppcfp128Type = unsafePerformIO do
    MkType <$> withContext ?context Raw.ppcFP128TypeInContext

-- | Create a new structure type in a context.
structType :: (?context :: Context) => Storable.Vector Type -> Bool -> Type
structType elements packed = MkType $ unsafePerformIO do
    withTypeArray elements \ptr size ->
        withContext ?context \context ->
            Raw.structTypeInContext context ptr size (Raw.consBool packed)

{- | Create a fixed size array type that refers to a specific type.

The created type will exist in the context that its element type exists in.
-}
arrayType :: Type -> Int -> Type
arrayType (MkType elementType) elementCount =
    -- TODO: LLVMArrayType is deprecated and this should really use LLVMArrayType2
    MkType $ unsafePerformIO (Raw.arrayType elementType (fromIntegral elementCount))

{- | Create a pointer type that points to a defined type.

The created type will exist in the context that its pointee type exists in.

By default, modern versions of LLVM ignore the pointed-to type. (https://llvm.org/docs/OpaquePointers.html#the-opaque-pointer-type
)
For this reason, you will probably want to use 'pointerType' or 'pointerTypeWithAddressSpace' instead.
-}
typedPointerType :: Type -> Word -> Type
typedPointerType (MkType elementType) addressSpace =
    MkType $ unsafePerformIO (Raw.pointerType elementType (fromIntegral addressSpace))

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
-}
vectorType :: Type -> Int -> Type
vectorType (MkType elementType) elementCount =
    MkType $ unsafePerformIO (Raw.vectorType elementType (fromIntegral elementCount))

voidType :: (?context :: Context) => Type
voidType = MkType $ unsafePerformIO $ withContext ?context Raw.voidTypeInContext

labelType :: (?context :: Context) => Type
labelType = MkType $ unsafePerformIO $ withContext ?context Raw.labelTypeInContext

x86AMXType :: (?context :: Context) => Type
x86AMXType = MkType $ unsafePerformIO $ withContext ?context Missing.x86AMXTypeInContext

tokenType :: (?context :: Context) => Type
tokenType = MkType $ unsafePerformIO $ withContext ?context Missing.tokenTypeInContext

metadataType :: (?context :: Context) => Type
metadataType = MkType $ unsafePerformIO $ withContext ?context Missing.metadataTypeInContext

targetExtType :: (?context :: Context) => Text -> Storable.Vector Type -> Storable.Vector Int -> Type
targetExtType name typeParams intParams = MkType $
    unsafePerformIO $
        withContext ?context $ \contextRef ->
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
getNamedGlobal module_ name = liftIO do
    withModule module_ \moduleRef -> do
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

-- | Dump a representation of a module to stderr.
dumpModule :: (MonadIO io) => Module -> io ()
dumpModule module_ = liftIO $
    withModule module_ \moduleRef ->
        Missing.dumpModule moduleRef

-- | Print a representation of a module to a file.
printModuleToFile :: (MonadIO io) => Module -> OsPath -> io ()
printModuleToFile module_ filePath = liftIO do
    -- TODO: use the underlying ShortByteString directly instead of going via string
    filePathString <- OsPath.decodeFS filePath
    withCString filePathString \filePathCString -> do
        withModule module_ \module_ -> do
            withErrorMessage (Just ("printModuleToFile _ \"" <> Text.pack filePathString <> "\"")) \errorMessagePtr -> do
                Missing.printModuleToFile module_ filePathCString errorMessagePtr

printModuleToString :: Module -> Text
printModuleToString module_ = unsafePerformIO do
    withModule module_ \module_ -> mask_ do
        cstring <- Missing.printModuleToString module_
        result <- Text.Foreign.peekCString cstring
        Raw.disposeMessage cstring
        pure result

-- | Append a basic block to the end of a function.
appendBasicBlock :: (?context :: Context, MonadIO io) => Value -> Text -> io BasicBlock
appendBasicBlock (MkValue function) name = liftIO $
    withContext ?context \contextRef ->
        Text.Foreign.withCString name \cname ->
            MkBlock <$> Raw.appendBasicBlockInContext contextRef function cname

wrapDirectlyPure 'Raw.getParam ""

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
constString :: (?context :: Context, MonadIO io) => Text -> NullTermination -> io Value
constString text nullTermination = liftIO $ withContext ?context \contextRef ->
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

wrapAs "constArray" 'Missing.constArray2 "Create a ConstantArray from values.\n\nThis is a wrapper around LLVMConstArray2"

wrapDirectly 'Missing.constDataArray "Create a ConstantDataArray from raw values.\n\nElementTy must be one of i8, i16, i32, i64, half, bfloat, float, or double. Data points to a contiguous buffer of raw values in the host endianness. The element count is inferred from the element type and the data size in bytes."

wrapDirectly 'Raw.constNamedStruct "Create a non-anonymous ConstantStruct from values. "

wrapDirectly 'Missing.getAggregateElement "Get element of a constant aggregate (struct, array or vector) at the specified index. "

wrapDirectly 'Raw.constVector "Create a ConstantVector from values."

wrapDirectly 'Missing.constantPtrAuth "Create a ConstantPtrAuth constant with the given values."

wrapDirectly 'Missing.isInBounds "Check whether the given GEP operator is inbounds."

wrapDirectly 'Missing.setIsInBounds "Set the given GEP instruction to be inbounds or not."

wrapDirectly 'Missing.getGEPSourceElementType "Get the source element type of the given GEP operator."

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
        Strict.generateM (fromIntegral size) \i -> do
            kind <- Missing.valueMetadataEntriesGetKind pointer (fromIntegral i)
            metadataRef <- Missing.valueMetadataEntriesGetMetadata pointer (fromIntegral i)
            pure (fromIntegral kind, Wrappers.MkMetaData metadataRef)

wrapDirectly 'Raw.constNull "Obtain a constant value referring to the null instance of a type.\n\nIf you want to create a null /pointer/, use 'constNullPointer' instead."

wrapDirectly 'Raw.constAllOnes "Obtain a constant value referring to the instance of a type consisting of all ones. "

wrapDirectly 'Raw.getUndef "Obtain a constant value referring to an undefined value of a type."

wrapDirectly 'Missing.getPoison "Obtain a constant value referring to a poison value of a type. "

wrapDirectly 'Raw.isNull "Determine whether a value instance is null."

-- | Obtain a constant value of a null pointer
constNullPointer :: (?context :: Context) => Value
constNullPointer = unsafePerformIO do
    let MkType voidTypeRef = voidType
    MkValue <$> Raw.constPointerNull voidTypeRef
