{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-identities #-}

module LLVM.InstructionBuilder (
    module LLVM.InstructionBuilder,
    Builder,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text.Foreign qualified as Text.Foreign
import Data.Vector.Storable qualified as Storable
import Data.Vector.Strict qualified as Vector
import Foreign (Storable (sizeOf), allocaBytes)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Ptr (nullPtr)
import LLVM.Core (FastMathFlags)
import LLVM.Core.Phi (addIncoming)
import LLVM.FFI.Core qualified as Raw
import LLVM.FFI.Missing qualified as Missing
import LLVM.Internal.TH (wrapAs, wrapDirectly)
import LLVM.Internal.Wrappers (
    BasicBlock (..),
    Builder (..),
    Context,
    FastMathFlags (..),
    MetaData (..),
    Type (..),
    Value (..),
    unsafeVectorFromCArray,
    withBuilder,
    withContext,
    withValueArray,
 )

createBuilder :: (?context :: Context, MonadIO io) => io Builder
createBuilder = liftIO $ withContext ?context \context -> do
    rawBuilder <- Raw.createBuilderInContext context
    foreignPtr <- newForeignPtr rawBuilder (Missing.disposeBuilder rawBuilder)
    pure (MkBuilder foreignPtr)

-- | Set the builder position before Instr but after any attached debug records, or if Instr is null set the position to the end of Block.
positionBuilder :: (MonadIO io) => Builder -> BasicBlock -> Maybe Value -> io ()
positionBuilder builder (MkBlock blockRef) maybeValue = liftIO $ withBuilder builder \builderRef -> case maybeValue of
    Just (MkValue valueRef) -> Raw.positionBuilder builderRef blockRef valueRef
    Nothing -> Raw.positionBuilder builderRef blockRef nullPtr

wrapAs "positionBuilderBefore" 'Raw.positionBefore "Set the builder position before Instr but after any attached debug records. "

-- | Set the builder position before Instr and any attached debug records, or if Instr is null set the position to the end of Block.
positionBuilderBeforeDbgRecords :: (MonadIO io) => Builder -> BasicBlock -> Maybe Value -> io ()
positionBuilderBeforeDbgRecords builder (MkBlock block) maybeValue = liftIO $ withBuilder builder \builderRef -> case maybeValue of
    Nothing -> Missing.positionBuilderBeforeDbgRecords builderRef block nullPtr
    Just (MkValue valueRef) -> Missing.positionBuilderBeforeDbgRecords builderRef block valueRef

wrapDirectly 'Missing.positionBuilderBeforeInstrAndDbgRecords "Set the builder position before Instr and any attached debug records. "

wrapAs "positionBuilderAtEnd" 'Raw.positionAtEnd "test"

wrapDirectly 'Raw.getInsertBlock ""

wrapDirectly 'Raw.clearInsertionPosition ""

wrapDirectly 'Raw.insertIntoBuilder ""

wrapAs "getCurrentDebugLocation" 'Missing.getCurrentDebugLocation2 "Get location information used by debugging information.\n\nThis uses LLVMGetCurrentDebugLocation2 under the hood."

{- | Set location information used by debugging information.

To clear the location metadata of the given instruction, pass Nothing to loc.

This uses LLVMSetCurrentDebugLocation2 under the hood.
-}
setCurrentDebugLocation :: (MonadIO io) => Builder -> Maybe MetaData -> io ()
setCurrentDebugLocation builder loc = liftIO $ withBuilder builder \builderRef -> case loc of
    Nothing -> Missing.setCurrentDebugLocation2 builderRef nullPtr
    Just (MkMetaData ref) -> Missing.setCurrentDebugLocation2 builderRef ref

wrapDirectly 'Missing.addMetadataToInst "Adds the metadata registered with the given builder to the given instruction."

wrapDirectly 'Missing.builderGetDefaultFPMathTag "Get the dafult floating-point math metadata for a given builder."

wrapDirectly 'Missing.builderSetDefaultFPMathTag "Set the default floating-point math metadata for the given builder. "

-- We cannot wrap LLVMGetBuilderContext since our ForeignPtr setup assumes that it has the only reference to the context.

wrapDirectly 'Raw.buildRetVoid ""

wrapDirectly 'Raw.buildRet ""

wrapDirectly 'Raw.buildAggregateRet ""

wrapDirectly 'Raw.buildBr ""

wrapDirectly 'Raw.buildCondBr ""

wrapDirectly 'Raw.buildSwitch ""

wrapDirectly 'Raw.buildIndirectBr ""

wrapDirectly 'Missing.buildCallBr ""

wrapDirectly 'Raw.buildInvoke2 ""

wrapDirectly 'Missing.buildInvokeWithOperandBundles ""

wrapDirectly 'Raw.buildUnreachable ""

wrapDirectly 'Raw.buildResume ""

wrapDirectly 'Raw.buildLandingPad ""

wrapDirectly 'Missing.buildCleanupRet ""

wrapDirectly 'Missing.buildCatchRet ""

wrapDirectly 'Missing.buildCatchPad ""

wrapDirectly 'Missing.buildCleanupPad ""

wrapDirectly 'Missing.buildCatchSwitch ""

wrapDirectly 'Raw.addCase ""

wrapDirectly 'Raw.addDestination ""

wrapDirectly 'Missing.getNumClauses ""

wrapDirectly 'Missing.getClause ""

wrapDirectly 'Missing.addClause ""

wrapDirectly 'Missing.isCleanup ""

wrapDirectly 'Missing.setCleanup ""

wrapDirectly 'Missing.addHandler ""

wrapDirectly 'Missing.getNumHandlers ""

-- | Obtain the basic blocks acting as handlers for a catchswitch instruction.
getHandlers :: (MonadIO io) => Value -> io (Storable.Vector BasicBlock)
getHandlers value@(MkValue valueRef) = liftIO do
    count <- getNumHandlers value
    vectorOfPointers <- allocaBytes @Raw.BasicBlockRef (count * sizeOf (undefined :: Raw.BasicBlockRef)) \storage -> do
        Missing.getHandlers valueRef storage
        unsafeVectorFromCArray storage count
    pure (Storable.unsafeCoerceVector @Raw.BasicBlockRef @BasicBlock vectorOfPointers)

wrapDirectly 'Missing.getArgOperand ""

wrapDirectly 'Missing.setArgOperand ""

wrapDirectly 'Missing.getParentCatchSwitch "Get the parent catchswitch instruction of a catchpad instruction."

wrapDirectly 'Missing.setParentCatchSwitch "Set the parent catchswitch instruction of a catchpad instruction."

wrapDirectly 'Raw.buildAdd ""

wrapDirectly 'Raw.buildNSWAdd ""

wrapDirectly 'Raw.buildNUWAdd ""

wrapDirectly 'Raw.buildFAdd ""

wrapDirectly 'Raw.buildSub ""

wrapDirectly 'Raw.buildNSWSub ""

wrapDirectly 'Raw.buildNUWSub ""

wrapDirectly 'Raw.buildFSub ""

wrapDirectly 'Raw.buildMul ""

wrapDirectly 'Raw.buildNSWMul ""

wrapDirectly 'Raw.buildNUWMul ""

wrapDirectly 'Raw.buildFMul ""

wrapDirectly 'Raw.buildUDiv ""

wrapDirectly 'Missing.buildExactUDiv ""

wrapDirectly 'Raw.buildSDiv ""

wrapDirectly 'Raw.buildExactSDiv ""

wrapDirectly 'Raw.buildFDiv ""

wrapDirectly 'Raw.buildURem ""

wrapDirectly 'Raw.buildSRem ""

wrapDirectly 'Raw.buildFRem ""

wrapDirectly 'Raw.buildShl ""

wrapDirectly 'Raw.buildLShr ""

wrapDirectly 'Raw.buildAShr ""

wrapDirectly 'Raw.buildAnd ""

wrapDirectly 'Raw.buildOr ""

wrapDirectly 'Raw.buildXor ""

-- TODO: LLVMBuildBinOp

wrapDirectly 'Raw.buildNeg ""

wrapDirectly 'Raw.buildNSWNeg ""

wrapDirectly 'Raw.buildFNeg ""

wrapDirectly 'Raw.buildNot ""

wrapDirectly 'Missing.getNUW ""

wrapDirectly 'Missing.setNUW ""

wrapDirectly 'Missing.getNSW ""

wrapDirectly 'Missing.setNSW ""

wrapDirectly 'Missing.getExact ""

wrapDirectly 'Missing.setExact ""

wrapDirectly 'Missing.getNNeg "Gets if the instruction has the non-negative flag set. "

wrapDirectly 'Missing.setNNeg "Sets the non-negative flag for the instruction.\n\nOnly valid for zext instructions."

wrapDirectly 'Missing.getFastMathFlags "Get the flags for which fast-math-style optimizations are allowed for this value.\n\nOnly valid on floating point instructions."

wrapDirectly 'Missing.setFastMathFlags "Sets the flags for which fast-math-style optimizations are allowed for this value.\n\nOnly valid on floating point instructions"

wrapDirectly 'Missing.canValueUseFastMathFlags "Check if a given value can potentially have fast math flags.\n\nWill return true for floating point arithmetic instructions, and for select, phi, and call instructions whose type is a floating point type, or a vector or array thereof. See https://llvm.org/docs/LangRef.html#fast-math-flags "

wrapDirectly 'Missing.getIsDisjoint "Gets whether the instruction has the disjoint flag set.\n\nOnly valid for or instructions."

wrapDirectly 'Missing.setIsDisjoint "Sets the disjoint flag for the instruction.\n\nOnly valid for or instructions."

wrapDirectly 'Raw.buildMalloc ""

wrapDirectly 'Raw.buildArrayMalloc ""

wrapDirectly 'Missing.buildMemSet "Creates and inserts a memset to the specified pointer and the specified value."

wrapDirectly 'Missing.buildMemCpy "Creates and inserts a memcpy between the specified pointers."

wrapDirectly 'Missing.buildMemMove "Creates and inserts a memmove between the specified pointers."

wrapDirectly 'Raw.buildAlloca ""

wrapDirectly 'Raw.buildArrayAlloca ""

wrapDirectly 'Raw.buildFree ""

wrapAs "buildLoad" 'Raw.buildLoad2 ""

wrapDirectly 'Raw.buildStore ""

wrapAs "buildGetElementPtr" 'Raw.buildGEP2 ""

wrapAs "buildInBoundsGetElementPtr" 'Raw.buildInBoundsGEP2 ""

wrapDirectly 'Missing.buildGEPWithNoWrapFlags "Creates a GetElementPtr instruction. Similar to 'buildGetElementPtr', but allows specifying the no-wrap flags."

wrapAs "buildStructGetElementPtr" 'Raw.buildStructGEP2 ""

wrapDirectly 'Raw.buildGlobalString ""

wrapDirectly 'Missing.getVolatile ""

wrapDirectly 'Missing.setVolatile ""

wrapDirectly 'Missing.getWeak ""

wrapDirectly 'Missing.setWeak ""

-- TODO: LLVMGetOrdering

-- TODO: LLVMSetOrdering

-- TODO: LLVMGetAtomicRMWBinOp

-- TODO: LLVMSetAtomicRMWBinOp

wrapDirectly 'Raw.buildTrunc ""

wrapDirectly 'Raw.buildZExt ""

wrapDirectly 'Raw.buildSExt ""

wrapDirectly 'Raw.buildFPToUI ""

wrapDirectly 'Raw.buildFPToSI ""

wrapDirectly 'Raw.buildUIToFP ""

wrapDirectly 'Raw.buildSIToFP ""

wrapDirectly 'Raw.buildFPTrunc ""

wrapDirectly 'Raw.buildFPExt ""

wrapDirectly 'Raw.buildPtrToInt ""

wrapDirectly 'Raw.buildIntToPtr ""

wrapDirectly 'Raw.buildBitCast ""

wrapDirectly 'Missing.buildAddrSpaceCast ""

wrapDirectly 'Raw.buildZExtOrBitCast ""

wrapDirectly 'Raw.buildSExtOrBitCast ""

wrapDirectly 'Raw.buildTruncOrBitCast ""

-- TODO: wrapDirectly 'Raw.buildCast "" (we don't support `OpCode` yet)

wrapDirectly 'Raw.buildPointerCast ""

wrapAs "buildIntCast" 'Missing.buildIntCast2 ""

wrapDirectly 'Raw.buildFPCast ""

-- TODO: LLVMGetCastOpCode

wrapDirectly 'Missing.buildFCmp ""
wrapDirectly 'Missing.buildICmp ""

-- TODO: wrapDirectly 'Raw.buildFCmp "" (RealPredicate)

{- | Build a PHI node with the given incoming values/blocks.

This is an extension to LLVM's C API: If passed an empty array, this behaves exactly
like @LLVMBuildPhi@. Otherwise, it uses 'addIncoming' to
construct the PHI node in one step.
-}
buildPhi :: (MonadIO io) => Builder -> Type -> Vector.Vector (Value, BasicBlock) -> Text -> io Value
buildPhi builder (MkType typeRef) incomingValues name = liftIO do
    phi <-
        MkValue <$> withBuilder builder \builderRef ->
            Text.Foreign.withCString name \nameString -> do
                Raw.buildPhi builderRef typeRef nameString
    when (not (Vector.null incomingValues)) do
        addIncoming phi incomingValues
    pure phi

wrapAs "buildCall" 'Missing.buildCall2 ""

wrapDirectly 'Missing.buildCallWithOperandBundles ""

wrapDirectly 'Raw.buildSelect ""

wrapDirectly 'Raw.buildVAArg ""

wrapDirectly 'Raw.buildExtractElement ""

wrapDirectly 'Raw.buildInsertElement ""

wrapDirectly 'Raw.buildShuffleVector ""

wrapDirectly 'Raw.buildExtractValue ""

wrapDirectly 'Raw.buildInsertValue ""

wrapDirectly 'Missing.buildFreeze ""

wrapDirectly 'Raw.buildIsNull ""

wrapDirectly 'Raw.buildIsNotNull ""

wrapAs "buildPtrDiff" 'Raw.buildPtrDiff2 ""

-- TODO: LLVMBuildFence

-- TODO: LLVMBuildFenceSyncScope

-- TODO: LLVMBuildAtomicRMW

-- TODO: LLVMBuildAtomicRMWSyncScope

-- TODO: LLVMBuildAtomicCmpXchg

-- TODO: LLVMBuildAtomicCmpXchgSyncScope

wrapDirectly 'Missing.getNumMaskElements "Get the number of elements in the mask of a ShuffleVector instruction. "

wrapDirectly 'Missing.getUndefMaskElem ""

wrapDirectly 'Missing.getMaskValue "Get the mask value at position Elt in the mask of a ShuffleVector instruction.\n\nReturns the result of 'getUndefMaskElem' if the mask value is poison at that position. "

wrapDirectly 'Missing.isAtomicSingleThread ""

wrapDirectly 'Missing.setAtomicSingleThread ""

wrapDirectly 'Missing.isAtomic "Returns whether an instruction is an atomic instruction, e.g., atomicrmw, cmpxchg, fence, or loads and stores with atomic ordering."

wrapDirectly 'Missing.getAtomicSyncScopeID "Returns the synchronization scope ID of an atomic instruction."

wrapDirectly 'Missing.setAtomicSyncScopeID "Sets the synchronization scope ID of an atomic instruction."

-- TODO: LLVMGetCmpXchgFailureOrdering
-- TODO: LLVMSetCmpXchgFailureOrdering
-- TODO: LLVMGetCmpXchgSuccessOrdering
-- TODO: LLVMSetCmpXchgSuccessOrdering
