{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LLVM.Internal.Wrappers where

import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Foreign qualified as Text.Foreign
import Data.Vector.Storable qualified as Storable
import Data.Vector.Strict qualified as Strict
import Foreign (ForeignPtr, Storable (sizeOf), newForeignPtr, plusPtr, withForeignPtr)
import Foreign.C (CInt, CSize, CUInt, withCString)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (FinalizerPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import LLVM.FFI.Core qualified as Raw
import LLVM.FFI.Target qualified as Raw
import LLVM.Internal.TH.Util (cEnum, foreignPointerWrapper)
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

newtype Context = MkContext (ForeignPtr Raw.Context)

withContext :: Context -> (Raw.ContextRef -> IO a) -> IO a
withContext (MkContext foreignPtr) cont = withForeignPtr foreignPtr cont

newtype Module = MkModule (ForeignPtr Raw.Module)

withModule :: Module -> (Raw.ModuleRef -> IO a) -> IO a
withModule (MkModule foreignPtr) cont = withForeignPtr foreignPtr cont

newtype Builder = MkBuilder (ForeignPtr Raw.Builder)

withBuilder :: Builder -> (Raw.BuilderRef -> IO a) -> IO a
withBuilder (MkBuilder foreignPtr) cont = withForeignPtr foreignPtr \rawPtr -> cont rawPtr

newtype BasicBlock = MkBlock (Raw.BasicBlockRef)
    deriving newtype (Storable)

newtype Value = MkValue (Raw.ValueRef)
    deriving newtype (Storable)

withValueArray :: Storable.Vector Value -> (Ptr Raw.ValueRef -> CUInt -> IO a) -> IO a
withValueArray vector cont = do
    -- This is safe since `Value` newtype derives its `Storable` instance from the underlying TypeRef
    let vectorOfPointers = Storable.unsafeCoerceVector @Value @Raw.ValueRef vector
    Storable.unsafeWith vectorOfPointers \ptr -> cont ptr (fromIntegral (Storable.length vector))

newtype Type = MkType (Raw.TypeRef)
    deriving newtype (Storable)

newtype FunctionType = MkFunctionType (FunctionTypeRef)

data OpaqueFunctionType
type FunctionTypeRef = Ptr OpaqueFunctionType

{- | Cast a FunctionType to the underlying Type.

This is a safe operation since function types are subtypes of regular types in LLVM and the C API
represents function types as regular types anyway
-}
functionTypeAsType :: FunctionType -> Type
functionTypeAsType (MkFunctionType ptr) = MkType (coerce ptr)

{- | Upcast a type to a function type. The type needs to be a valid function type but this condition is not checked.

This is an unsafe operation since LLVM will in general not check if
types are function types or regular types and may segfault when given the wrong one
-}
unsafeTypeAsFunctionType :: Type -> FunctionType
unsafeTypeAsFunctionType (MkType ptr) = MkFunctionType (coerce ptr)

newtype Global = MkGlobal GlobalRef

data OpaqueGlobal
type GlobalRef = Ptr Global

{- | Cast a global to a value.

This is a safe operation since the underlying C API represents references to globals as references to values anyway.
-}
globalAsValue :: Global -> Value
globalAsValue = coerce

{- | Upcast a global variable to a value. For this to be safe, the passed value needs to refer
to a global variable but this condition is not checked.

This is an unsafe operation since LLVM will in general not check if
values passed to functions that expect globals are actually globals
-}
unsafeValueAsGlobal :: Value -> Global
unsafeValueAsGlobal = coerce

withTypeArray :: Storable.Vector Type -> (Ptr Raw.TypeRef -> CUInt -> IO a) -> IO a
withTypeArray vector cont = do
    -- This is safe since `Type` newtype derives its `Storable` instance from the underlying TypeRef
    let vectorOfPointers = Storable.unsafeCoerceVector @Type @Raw.TypeRef vector
    Storable.unsafeWith vectorOfPointers \ptr -> cont ptr (fromIntegral (Storable.length vector))

withUnsignedArray :: Storable.Vector Int -> (Ptr CUInt -> CUInt -> IO a) -> IO a
withUnsignedArray vector cont = do
    let vectorOfCUInts = Storable.map (fromIntegral) vector
    Storable.unsafeWith vectorOfCUInts \ptr -> cont ptr (fromIntegral (Storable.length vector))

data OpaqueMetaData
type MetaDataRef = Ptr OpaqueMetaData

newtype MetaData = MkMetaData MetaDataRef

type RawFastMathFlags = CUInt

newtype FastMathFlags = MkFastMathFlags CUInt

data OpaqueOperandBundle
type OperandBundleRef = Ptr OpaqueOperandBundle

newtype OperandBundle = MkOperandBundle (ForeignPtr OpaqueOperandBundle)

-- | Type synonym that instructs the TH machinery to attach a finalizer to this returned operand bundle
type OwnedOperandBundleRef = OperandBundleRef

-- This is defined here to avoid the cyclical dependency with LLVM.FFI.Missing
foreign import capi unsafe "llvm-c/Core.h &LLVMDisposeOperandBundle"
    disposeOperandBundle :: FinalizerPtr OpaqueOperandBundle

wrapOwnedOperandBundle :: OperandBundleRef -> IO OperandBundle
wrapOwnedOperandBundle ref = do
    MkOperandBundle <$> newForeignPtr disposeOperandBundle ref

withOperandBundleArray :: Strict.Vector OperandBundle -> (Ptr OperandBundleRef -> CUInt -> IO a) -> IO a
withOperandBundleArray array cont = withAllForeignPointers [] (Strict.toList array) \bundleRefs -> do
    let size = Strict.length array
    let vector = Storable.fromListN size bundleRefs
    Storable.unsafeWith vector \ptr -> cont ptr (fromIntegral size)
  where
    withAllForeignPointers unwrapped operandBundles cont = case operandBundles of
        [] -> cont (reverse unwrapped)
        (MkOperandBundle foreignPtr : rest) -> do
            withForeignPtr foreignPtr \rawPtr -> do
                withAllForeignPointers (rawPtr : unwrapped) rest cont

data OpaqueDiagnosticInfo
type DiagnosticInfoRef = Ptr OpaqueDiagnosticInfo

newtype DiagnosticInfo = MkDiagnosticInfo DiagnosticInfoRef

newtype Attribute = MkAttribute Raw.AttributeRef
    deriving newtype (Storable)

unsafeVectorFromCArray :: forall a. (Storable a) => Ptr a -> Int -> IO (Storable.Vector a)
unsafeVectorFromCArray ptr size = do
    Storable.generateM size (\i -> peek (ptr `plusPtr` (i * sizeOf (undefined :: a))))

type RawGEPNoWrapFlags = CUInt
newtype GEPNoWrapFlags = MkGEPNoWrapFlags RawGEPNoWrapFlags

type RawIntPredicate = CInt
data IntPredicate
    = -- | equal
      IntEQ
    | -- | not equal
      IntNE
    | -- | unsigned greater than
      IntUGT
    | -- | unsigned greater or equal
      IntUGE
    | -- | unsigned less than
      IntULT
    | -- | unsigned less or equal
      IntULE
    | -- | signed greater than
      IntSGT
    | -- | signed greater or equal
      IntSGE
    | -- | signed less than
      IntSLT
    | -- | signed less or equal
      IntSLE
    deriving (Show)

unwrapIntPredicate :: IntPredicate -> RawIntPredicate
unwrapIntPredicate = \case
    IntEQ -> 32
    IntNE -> 33
    IntUGT -> 34
    IntUGE -> 35
    IntULT -> 36
    IntULE -> 37
    IntSGT -> 38
    IntSGE -> 39
    IntSLT -> 40
    IntSLE -> 41

type RawRealPredicate = CInt
data RealPredicate
    = -- | Always false (always folded)
      RealPredicateFalse
    | -- | True if ordered and equal
      RealOEQ
    | -- | True if ordered and greater than
      RealOGT
    | -- | True if ordered and greater than or equal
      RealOGE
    | -- | True if ordered and less than
      RealOLT
    | -- | True if ordered and less than or equal
      RealOLE
    | -- | True if ordered and operands are unequal
      RealONE
    | -- | True if ordered (no nans)
      RealORD
    | -- | True if unordered: isnan(X) | isnan(Y)
      RealUNO
    | -- | True if unordered or equal
      RealUEQ
    | -- | True if unordered or greater than
      RealUGT
    | -- | True if unordered, greater than, or equal
      RealUGE
    | -- | True if unordered or less than
      RealULT
    | -- | True if unordered, less than, or equal
      RealULE
    | -- | True if unordered or not equal
      RealUNE
    | -- | Always true (always folded)
      RealPredicateTrue
    deriving (Show)

unwrapRealPredicate :: RealPredicate -> RawRealPredicate
unwrapRealPredicate = \case
    RealPredicateFalse -> 0
    RealOEQ -> 1
    RealOGT -> 2
    RealOGE -> 3
    RealOLT -> 4
    RealOLE -> 5
    RealONE -> 6
    RealORD -> 7
    RealUNO -> 8
    RealUEQ -> 9
    RealUGT -> 10
    RealUGE -> 11
    RealULT -> 12
    RealULE -> 13
    RealUNE -> 14
    RealPredicateTrue -> 15

wrapMessage :: CString -> IO Text
wrapMessage cstring = do
    text <- Text.Foreign.peekCString cstring
    Raw.disposeMessage cstring
    pure text

-- | Type alias around 'CString' that instructs the TH machinery to wrap it in a 'Text' and to assume that it is followed by a length parameter.
type CStringLenAsText = CString

-- | Type alias around 'CString' that instructs the TH machinery to wrap it in a 'ByteString' instead of a 'Text' and to assume that it is followed by a length parameter.
type CStringLenAsByteString = CString

-- | Type alias around 'CString' that instructs the TH machinery to copy it without trying to free the underlying pointer
type UnownedCString = CString

-- | Type alias around 'CString' that instructs the TH machinery to wrap it with 'wrapMessage'
type MessageCString = CString

type RawLinkage = CUInt

wrapLinkage :: RawLinkage -> Raw.Linkage
wrapLinkage = \case
    0 -> Raw.ExternalLinkage
    1 -> Raw.AvailableExternallyLinkage
    2 -> Raw.LinkOnceAnyLinkage
    3 -> Raw.LinkOnceODRLinkage
    4 -> Raw.LinkOnceODRAutoHideLinkage
    5 -> Raw.WeakAnyLinkage
    6 -> Raw.WeakODRLinkage
    7 -> Raw.AppendingLinkage
    8 -> Raw.InternalLinkage
    9 -> Raw.PrivateLinkage
    10 -> Raw.DLLImportLinkage
    11 -> Raw.DLLExportLinkage
    12 -> Raw.ExternalWeakLinkage
    13 -> Raw.GhostLinkage
    14 -> Raw.CommonLinkage
    15 -> Raw.LinkerPrivateLinkage
    16 -> Raw.LinkerPrivateWeakLinkage
    value -> error $ "wrapLinkage: invalid linkage value: " <> show value

unwrapLinkage :: Raw.Linkage -> RawLinkage
unwrapLinkage = \case
    Raw.ExternalLinkage -> 0
    Raw.AvailableExternallyLinkage -> 1
    Raw.LinkOnceAnyLinkage -> 2
    Raw.LinkOnceODRLinkage -> 3
    Raw.LinkOnceODRAutoHideLinkage -> 4
    Raw.WeakAnyLinkage -> 5
    Raw.WeakODRLinkage -> 6
    Raw.AppendingLinkage -> 7
    Raw.InternalLinkage -> 8
    Raw.PrivateLinkage -> 9
    Raw.DLLImportLinkage -> 10
    Raw.DLLExportLinkage -> 11
    Raw.ExternalWeakLinkage -> 12
    Raw.GhostLinkage -> 13
    Raw.CommonLinkage -> 14
    Raw.LinkerPrivateLinkage -> 15
    Raw.LinkerPrivateWeakLinkage -> 16

type RawVisibility = CUInt

wrapVisibility :: RawVisibility -> Raw.Visibility
wrapVisibility = \case
    0 -> Raw.DefaultVisibility
    1 -> Raw.HiddenVisibility
    2 -> Raw.ProtectedVisibility
    value -> error $ "wrapVisibility: invalid visibility value: " <> show value

unwrapVisibility :: Raw.Visibility -> RawVisibility
unwrapVisibility = \case
    Raw.DefaultVisibility -> 0
    Raw.HiddenVisibility -> 1
    Raw.ProtectedVisibility -> 2

type RawDLLStorageClass = CUInt

data DLLStorageClass
    = DefaultStorageClass
    | ImportStorageClass
    | ExportStorageClass
    deriving (Show)

wrapDLLStorageClass :: RawDLLStorageClass -> DLLStorageClass
wrapDLLStorageClass = \case
    0 -> DefaultStorageClass
    1 -> ImportStorageClass
    2 -> ExportStorageClass
    value -> error $ "wrapDLLStorageClass: invalid DLLStorageClass value: " <> show value

unwrapDLLStorageClass :: DLLStorageClass -> RawDLLStorageClass
unwrapDLLStorageClass = \case
    DefaultStorageClass -> 0
    ImportStorageClass -> 1
    ExportStorageClass -> 2

type RawUnnamedAddr = CUInt

data UnnamedAddr
    = NoUnnamedAddr
    | LocalUnnamedAddr
    | GlobalUnnamedAddr
    deriving (Show)

wrapUnnamedAddr :: RawUnnamedAddr -> UnnamedAddr
wrapUnnamedAddr = \case
    0 -> NoUnnamedAddr
    1 -> LocalUnnamedAddr
    2 -> GlobalUnnamedAddr
    value -> error $ "wrapUnnamedAddr: invalid unnamed address value: " <> show value

unwrapUnnamedAddr :: UnnamedAddr -> RawUnnamedAddr
unwrapUnnamedAddr = \case
    NoUnnamedAddr -> 0
    LocalUnnamedAddr -> 1
    GlobalUnnamedAddr -> 2

type RawTailCallKind = CUInt

data TailCallKind
    = TailCallKindNone
    | TailCallKindTail
    | TailCallKindMustTail
    | TailCallKindNoTail
    deriving (Show)

wrapTailCallKind :: RawTailCallKind -> TailCallKind
wrapTailCallKind = \case
    0 -> TailCallKindNone
    1 -> TailCallKindTail
    2 -> TailCallKindMustTail
    3 -> TailCallKindNoTail
    value -> error $ "wrapTailCallKind: invalid unnamed address value: " <> show value

unwrapTailCallKind :: TailCallKind -> RawTailCallKind
unwrapTailCallKind = \case
    TailCallKindNone -> 0
    TailCallKindTail -> 1
    TailCallKindMustTail -> 2
    TailCallKindNoTail -> 3

data OpaqueValueMetadataEntries

type ValueMetadataEntriesRef = Ptr OpaqueValueMetadataEntries

type RawCallingConvention = CUInt

newtype CallingConvention = MkCallingConvention CUInt

foreignPointerWrapper "TargetData"

newtype TargetLibraryInfo = MkTargetLibraryInfo Raw.TargetLibraryInfoRef

newtype PassManager = MkPassManager Raw.PassManagerRef

type RawByteOrdering = CUInt

data ByteOrdering
    = BigEndian
    | LittleEndian

wrapByteOrdering :: RawByteOrdering -> ByteOrdering
wrapByteOrdering = \case
    0 -> BigEndian
    1 -> LittleEndian
    n -> error $ "wrapByteOrdering: Invalid byte ordering value: " <> show n

unwrapByteOrdering :: ByteOrdering -> RawByteOrdering
unwrapByteOrdering = \case
    BigEndian -> 0
    LittleEndian -> 1

data OpaqueTarget

type TargetRef = Ptr OpaqueTarget

newtype Target = MkTarget TargetRef

-- | Type alias that instructs the TH machinery to wrap the result in an option and check whether it is null.
type MightBeNull a = a

foreignPointerWrapper "TargetMachineOptions"

cEnum
    "CodeGenOptLevel"
    [ "CodeGenLevelNone"
    , "CodeGenLevelLess"
    , "CodeGenLevelDefault"
    , "CodeGenLevelAggressive"
    ]

cEnum
    "RelocMode"
    [ "RelocDefault"
    , "RelocStatic"
    , "RelocPIC"
    , "RelocDynamicNoPic"
    , "RelocROPI"
    , "RelocRWPI"
    , "RelocROPI_RWPI"
    ]

cEnum
    "CodeModel"
    [ "CodeModelDefault"
    , "CodeModelJITDefault"
    , "CodeModelTiny"
    , "CodeModelSmall"
    , "CodeModelKernel"
    , "CodeModelMedium"
    , "CodeModelLarge"
    ]

foreignPointerWrapper "TargetMachine"

cEnum
    "GlobalISELAbortMode"
    [ "GlobalISelAbortEnable"
    , "GlobalISelAbortDisable"
    , "GlobalISelAbortDisableWithDiag"
    ]

type CStringAsOSPath = CString

cEnum
    "CodeGenFileType"
    [ "AssemblyFile"
    , "ObjectFile"
    ]
deriving instance Show CodeGenFileType

foreignPointerWrapper "MemoryBuffer"

-- | Type synonym that tells the TH machinery to try to wrap the argument in a foreign pointer with the given dispose function
type AsForeignPtrWith disposeFunction a = a

withOsPath :: OsPath -> (CString -> IO a) -> IO a
withOsPath path cont = do
    filePathString <- OsPath.decodeFS path
    withCString filePathString \filePathCString -> cont filePathCString

-- | Type synonym that tells the TH machinery to interpret this as a pointer to an error message and to emit a call to 'LLVM.Internal.Error.withErrorMessage'
type ErrorMessageCStringPtr = Ptr CString
