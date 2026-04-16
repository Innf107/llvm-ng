{-# LANGUAGE TemplateHaskell #-}

module LLVM.Target (
    module LLVM.Target,
    ByteOrdering (..),
    TargetData (..),
    Target (..),
    CodeGenOptLevel (..),
    RelocMode(..),
    CodeModel(..),
    CodeGenFileType(..),
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Foreign qualified as Text.Foreign
import Foreign (peek, poke)
import Foreign qualified
import LLVM.FFI.Missing qualified as Missing
import LLVM.Internal.Error (withErrorMessage)
import LLVM.Internal.TH (wrapAs, wrapDirectly)
import LLVM.Internal.Wrappers
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

wrapDirectly 'Missing.initializeAllTargetInfos "The main program should call this function if it wants access to all available targets that LLVM is configured to support."

wrapDirectly 'Missing.initializeAllTargets "The main program should call this function if it wants to link in all available targets that LLVM is configured to support."

wrapDirectly 'Missing.initializeAllTargetMCs "The main program should call this function if it wants access to all available target MC that LLVM is configured to support."

wrapDirectly 'Missing.initializeAllAsmPrinters "The main program should call this function if it wants all asm printers that LLVM is configured to support, to make them available via the TargetRegistry."

wrapDirectly 'Missing.initializeAllAsmParsers "The main program should call this function if it wants all asm parsers that LLVM is configured to support, to make them available via the TargetRegistry. "

wrapDirectly 'Missing.initializeAllDisassemblers "The main program should call this function if it wants all disassemblers that LLVM is configured to support, to make them available via the TargetRegistry. "

wrapDirectly 'Missing.initializeNativeTarget "The main program should call this function to initialize the native target corresponding to the host.\n\nThis is useful for JIT applications to ensure that the target gets linked in correctly."

wrapDirectly 'Missing.initializeNativeAsmParser "The main program should call this function to initialize the parser for the native target corresponding to the host."

wrapDirectly 'Missing.initializeNativeAsmPrinter "The main program should call this function to initialize the printer for the native target corresponding to the host."

wrapDirectly 'Missing.initializeNativeDisassembler "The main program should call this function to initialize the disassembler for the native target corresponding to the host."

getModuleDataLayout :: (MonadIO io) => Module -> io TargetData
getModuleDataLayout module_ = liftIO $ withModule module_ \moduleRef -> do
    dataRef <- Missing.getModuleDataLayout moduleRef
    MkTargetData <$> Foreign.newForeignPtr Missing.disposeTargetData dataRef

wrapDirectly 'Missing.setModuleDataLayout "Set the data layout for a module."

createTargetData :: (MonadIO io) => Text -> io TargetData
createTargetData dataString = liftIO $ Text.Foreign.withCString dataString \cstring -> do
    dataRef <- Missing.createTargetData cstring
    MkTargetData <$> Foreign.newForeignPtr Missing.disposeTargetData dataRef

-- TODO: this does not take ownership of the target library info so it might be garbage collected and finalized while
-- it's still reachable from a PassManager
wrapDirectly 'Missing.addTargetLibraryInfo "Adds target library information to a pass manager."
wrapAs "stringRepOfTargetData" 'Missing.copyStringRepOfTargetData "Converts target data to a target layout string."
wrapDirectly 'Missing.byteOrder "Returns the byte order of a target, either 'BigEndian' or 'LittleEndian'."
wrapDirectly 'Missing.pointerSize "Returns the pointer size in bytes for a target."
wrapDirectly 'Missing.pointerSizeForAS "Returns the pointer size in bytes for a target for a specified address space."
wrapDirectly 'Missing.intPtrTypeInContext "Returns the integer type that is the same size as a pointer on a target."
wrapDirectly 'Missing.intPtrTypeForASInContext "Returns the integer type that is the same size as a pointer on a target.\n\nThis version allows the address space to be specified."
wrapDirectly 'Missing.sizeOfTypeInBits "Computes the size of a type in bits for a target."
wrapDirectly 'Missing.storeSizeOfType "Computes the storage size of a type in bytes for a target."
wrapDirectly 'Missing.abiSizeOfType "Computes the ABI size of a type in bytes for a target."
wrapDirectly 'Missing.abiAlignmentOfType "Computes the ABI alignment of a type in bytes for a target."
wrapDirectly 'Missing.callFrameAlignmentOfType "Computes the call frame alignment of a type in bytes for a target."
wrapDirectly 'Missing.preferredAlignmentOfType "Computes the preferred alignment of a type in bytes for a target."
wrapDirectly 'Missing.preferredAlignmentOfGlobal "Computes the preferred alignment of a global variable in bytes for a target."
wrapDirectly 'Missing.elementAtOffset "Computes the structure element that contains the byte offset for a target."
wrapDirectly 'Missing.llvmOffsetOfElement "Computes the byte offset of the indexed struct element for a target."
wrapDirectly 'Missing.getFirstTarget "Returns the first 'Target' in the registered targets list."
wrapDirectly 'Missing.getNextTarget "Returns the next 'Target' given a previous one (or 'Nothing' if there's none)"
wrapDirectly 'Missing.getTargetFromName "Finds the target corresponding to the given name"

-- | Finds the target corresponding to the given triple.
getTargetFromTriple :: (MonadIO io) => Text -> io Target
getTargetFromTriple triple =
    liftIO $
        Text.Foreign.withCString triple \tripleCStr -> do
            Foreign.alloca \targetRef -> do
                withErrorMessage (Just $ "getTargetFromTriple \"" <> triple <> "\"") $ Missing.getTargetFromTriple tripleCStr targetRef
                MkTarget <$> peek targetRef

wrapDirectly 'Missing.getTargetName "Returns the name of a target."
wrapDirectly 'Missing.getTargetDescription "Returns the description of a target."
wrapDirectly 'Missing.targetHasJIT "Returns if the target has a JIT."
wrapDirectly 'Missing.targetHasTargetMachine "Returns if the target has a TargetMachine associated."
wrapDirectly 'Missing.targetHasAsmBackend "Returns if the target as an ASM backend (required for emitting output)"
wrapDirectly 'Missing.createTargetMachineOptions "Create a new set of options for a 'TargetMachine'."

wrapDirectly 'Missing.targetMachineOptionsSetCPU ""
wrapDirectly 'Missing.targetMachineOptionsSetFeatures "Set the list of features for the target machine."
wrapDirectly 'Missing.targetMachineOptionsSetABI ""
wrapDirectly 'Missing.targetMachineSetCodeGenOptLevel ""
wrapDirectly 'Missing.targetMachineSetRelocMode ""
wrapDirectly 'Missing.targetMachineSetCodeModel ""
wrapDirectly 'Missing.createTargetMachineWithOptions "Creates a new 'TargetMachine'"
wrapDirectly 'Missing.createTargetMachine "Creates a new 'TargetMachine'"

-- TODO: we might want to combine the get and has functions here
wrapDirectly 'Missing.getTargetMachineTarget "Returns the Target used in a TargetMachine. "
wrapDirectly 'Missing.getTargetMachineTriple "Returns the triple used creating this target machine. "
wrapDirectly 'Missing.getTargetMachineCPU "Returns the cpu used creating this target machine."
wrapDirectly 'Missing.getTargetMachineFeatureString "Returns the feature string used creating this target machine."
wrapDirectly 'Missing.createTargetDataLayout "Create a DataLayout based on the targetMachine."
wrapDirectly 'Missing.setTargetMachineAsmVerbosity "Set the target machine's ASM verbosity."
wrapDirectly 'Missing.setTargetMachineFastISel "Enable fast-path instruction selection."
wrapDirectly 'Missing.setTargetMachineGlobalISel "Enable global instruction selection."
wrapDirectly 'Missing.setTargetMachineGlobalISelAbort "Set abort behaviour when global instruction selection fails to lower/select an instruction."
wrapDirectly 'Missing.setTargetMachineMachineOutliner "Enable the MachineOutliner pass."

{- | Emits an asm or object file for the given module to the 'OsPath'.

This wraps several c++ only classes (among them a file stream). Returns any error as an 'LLVMError' exception.
-}
targetMachineEmitToFile :: (MonadIO io) => TargetMachine -> Module -> OsPath -> CodeGenFileType -> io ()
targetMachineEmitToFile targetMachine module_ filePath codeGenFileType =
    liftIO $ withTargetMachine targetMachine \targetMachineRef -> do
        pathString <- OsPath.decodeFS filePath
        withModule module_ \moduleRef ->
            withOsPath filePath \pathCString -> do
                withErrorMessage (Just $ "targetMachineEmitToFile _ _ \"" <> Text.pack pathString <> "\"" <> Text.pack (show codeGenFileType)) do
                    Missing.targetMachineEmitToFile targetMachineRef moduleRef pathCString (unwrapCodeGenFileType codeGenFileType)

-- | Compile the LLVM IR stored in the given module and store the result in the memory buffer.
targetMachineEmitToMemoryBuffer :: (MonadIO io) => TargetMachine -> Module -> CodeGenFileType -> MemoryBuffer -> io ()
targetMachineEmitToMemoryBuffer targetMachine module_ codegenFileType buffer =
    liftIO $ withTargetMachine targetMachine \targetMachineRef ->
        withModule module_ \moduleRef ->
            withMemoryBuffer buffer \bufferRef ->
                withErrorMessage (Just "targetMachineEmitToMemoryBuffer") \errorMessagePtr -> do
                    Missing.targetMachineEmitToMemoryBuffer targetMachineRef moduleRef (unwrapCodeGenFileType codegenFileType) errorMessagePtr bufferRef

wrapDirectly 'Missing.getDefaultTargetTriple "Get a triple for the host machine as a string."
wrapDirectly 'Missing.normalizeTargetTriple "Normalize a target triple."
wrapDirectly 'Missing.getHostCPUName "Get the host CPU as a string."
wrapDirectly 'Missing.getHostCPUFeatures "Get the host CPU's features as a string."
wrapDirectly 'Missing.addAnalysisPasses "Adds the target-specific analysis passes to the pass manager."
