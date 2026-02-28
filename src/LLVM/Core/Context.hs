{-# LANGUAGE TemplateHaskell #-}

module LLVM.Core.Context where

import Foreign.Concurrent (newForeignPtr)
import LLVM.FFI.Core qualified as Raw
import LLVM.FFI.Missing qualified as Missing
import LLVM.Internal.TH (wrapDirectly)
import LLVM.Internal.Wrappers (Context (..))

{- | Create a new context.

The context has an attached finalizer and will automatically be garbage collected.
-}
contextCreate :: IO Context
contextCreate = do
    rawContext <- Raw.contextCreate
    MkContext <$> newForeignPtr rawContext (Raw.contextDispose rawContext)

-- TODO: contextSetDiagnosticHandelr, contextGetDiagnosticHandler, contextGetDiagnosticContext, contextSetYieldCallback

-- wrapDirectly 'Missing.contextShouldDiscardValueNames "Retrieve whether the given context is set to discard all value names. "
-- 
-- wrapDirectly 'Missing.contextSetDiscardValueNames "Set whether the given context discards all value names.\nIf true, only the names of GlobalValue objects will be available in the IR. This can be used to save memory and runtime, especially in release mode."
