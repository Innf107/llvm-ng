{-# LANGUAGE TemplateHaskell #-}

module LLVM.Target where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text.Foreign qualified as Text.Foreign
import Foreign qualified
import LLVM.FFI.Missing qualified as Missing
import LLVM.Internal.TH (wrapDirectly)
import LLVM.Internal.Wrappers (Module, TargetData (..), withModule)

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
