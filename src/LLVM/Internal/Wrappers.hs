module LLVM.Internal.Wrappers (
  Context (..),
  withContext,
  Module (..),
  withModule,
  Builder (..),
  withBuilder,
  BasicBlock (..),
  Value (..),
  withValueArray,
  Type (..),
  withTypeArray,
  withUnsignedArray,
  FunctionType (..),
  OpaqueFunctionType,
  FunctionTypeRef,
  unsafeFunctionTypeAsType,
  unsafeTypeAsFunctionType,
  OpaqueMetaData,
  MetaDataRef,
  MetaData (..),
  RawFastMathFlags,
  FastMathFlags (..),
  OpaqueOperandBundle,
  OperandBundleRef,
  OperandBundle (..),
  withOperandBundleArray,
  unsafeVectorFromCArray,
  GEPNoWrapFlags (..),
  RawGEPNoWrapFlags,
  RawIntPredicate,
  IntPredicate (..),
  unwrapIntPredicate,
  RawRealPredicate,
  RealPredicate (..),
  unwrapRealPredicate,
) where

import Data.Coerce (coerce)
import Data.Vector.Storable qualified as Storable
import Foreign (ForeignPtr, Storable (sizeOf), plusPtr, withForeignPtr)
import Foreign.C (CInt, CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import LLVM.FFI.Core qualified as Raw

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

This is an unsafe operation since LLVM will in general not check if
types are function types or regular types and may segfault when given the wrong one
-}
unsafeFunctionTypeAsType :: FunctionType -> Type
unsafeFunctionTypeAsType (MkFunctionType ptr) = MkType (coerce ptr)

{- | Upcast a type to a function type. The type needs to be a valid function type but this condition is not checked.

This is an unsafe operation since LLVM will in general not check if
types are function types or regular types and may segfault when given the wrong one
-}
unsafeTypeAsFunctionType :: Type -> FunctionType
unsafeTypeAsFunctionType (MkType ptr) = MkFunctionType (coerce ptr)

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

newtype OperandBundle = MkOperandBundle OperandBundleRef
  deriving newtype (Storable)

withOperandBundleArray :: Storable.Vector OperandBundle -> (Ptr OperandBundleRef -> CUInt -> IO a) -> IO a
withOperandBundleArray vector cont = do
  -- This is safe since `OperandBundle` newtype derives its `Storable` instance from the underlying OperandBundleRef
  let vectorOfPointers = Storable.unsafeCoerceVector @OperandBundle @OperandBundleRef vector
  Storable.unsafeWith vectorOfPointers \ptr -> cont ptr (fromIntegral (Storable.length vector))

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