{-# LANGUAGE TemplateHaskell #-}

module LLVM.Core (
    Module,
    Context,
    Value,
    Type,
    FunctionType,
    unsafeTypeAsFunctionType,
    unsafeFunctionTypeAsType,
    MetaData,
    FastMathFlags,
    IntPredicate (..),
    RealPredicate (..),
    BasicBlock (..),
    contextCreate,
    moduleCreateWithName,
    addFunction,
    functionType,
    intType,
    int1Type,
    int8Type,
    int16Type,
    int32Type,
    int64Type,
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
    dumpModule,
    printModuleToFile,
    printModuleToString,
    appendBasicBlock,
    getParam,
    constInt,
    constIntOfString,
    constReal,
    constRealOfString,
    constIntGetZExtValue,
    getTargetExtTypeNumTypeParams,
    getTargetExtTypeTypeParam,
    getTargetExtTypeNumIntParams,
    getTargetExtTypeIntParam,
) where

import Control.Exception (mask_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Foreign qualified as Text.Foreign
import Data.Vector.Storable qualified as Storable
import Foreign (Ptr)
import Foreign.C (CUInt, withCString)
import Foreign.Concurrent (newForeignPtr)
import LLVM.FFI.Core qualified as Raw
import LLVM.FFI.Missing qualified as Missing
import LLVM.Internal.Error (withErrorMessage)
import LLVM.Internal.TH (wrapDirectly, wrapDirectlyPure)
import LLVM.Internal.Wrappers (
    BasicBlock (..),
    Context (..),
    FastMathFlags,
    FunctionType (MkFunctionType),
    IntPredicate (..),
    MetaData,
    Module (..),
    RealPredicate (..),
    Type (..),
    Value (..),
    unsafeFunctionTypeAsType,
    unsafeTypeAsFunctionType,
    withContext,
    withModule,
    withTypeArray,
    withUnsignedArray,
 )
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

{- | Create a new context.

The context has an attached finalizer and will automatically be garbage collected.
-}
contextCreate :: IO Context
contextCreate = do
    rawContext <- Raw.contextCreate
    MkContext <$> newForeignPtr rawContext (Raw.contextDispose rawContext)

{- | Create a new, empty module in a specific context.

The mdoule has an attached finalizer and will automatically be garbage collected.
-}
moduleCreateWithName :: (?context :: Context) => Text -> IO Module
moduleCreateWithName name = do
    rawModule <- Text.Foreign.withCString name \nameCString -> do
        withContext ?context \contextPtr -> do
            Raw.moduleCreateWithNameInContext nameCString contextPtr
    MkModule <$> newForeignPtr rawModule (Raw.disposeModule rawModule)

-- | Add a function to a module under a specified name.
addFunction :: Module -> Text -> FunctionType -> IO Value
addFunction module_ name functionType = do
    let MkType type_ = unsafeFunctionTypeAsType functionType
    function <- Text.Foreign.withCString name \nameCStr -> do
        withModule module_ \modulePtr -> do
            Raw.addFunction modulePtr nameCStr type_
    pure (MkValue function)

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

{- TODO: llvm-ffi doesn't have int128TypeInContext yet -}

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

-- | Dump a representation of a module to stderr.
dumpModule :: Module -> IO ()
dumpModule module_ =
    withModule module_ \moduleRef ->
        Missing.dumpModule moduleRef

-- | Print a representation of a module to a file.
printModuleToFile :: Module -> OsPath -> IO ()
printModuleToFile module_ filePath = do
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
appendBasicBlock :: (?context :: Context) => Value -> Text -> IO BasicBlock
appendBasicBlock (MkValue function) name =
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
