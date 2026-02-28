{-# LANGUAGE TemplateHaskell #-}

module LLVM.Core.Context (module LLVM.Core.Context, AttributeKind (..)) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text.Foreign qualified as Foreign.Text
import Data.Text.Foreign qualified as Text.Foreign
import Foreign (Storable (peek), alloca)
import Foreign.C (CUInt)
import Foreign.Concurrent (newForeignPtr)
import LLVM.FFI.Core (AttributeKind (AttributeKind))
import LLVM.FFI.Core qualified as Raw
import LLVM.FFI.Missing qualified as Missing
import LLVM.Internal.TH (wrapAs, wrapDirectly)
import LLVM.Internal.Wrappers (Attribute (..), Context (..), DiagnosticInfo (MkDiagnosticInfo), withContext, wrapMessage)
import LLVM.Internal.Wrappers qualified as Wrappers
import System.IO.Unsafe (unsafePerformIO)

{- | Create a new context.

The context has an attached finalizer and will automatically be garbage collected.
-}
contextCreate :: (MonadIO io) => io Context
contextCreate = liftIO do
    rawContext <- Raw.contextCreate
    MkContext <$> newForeignPtr rawContext (Raw.contextDispose rawContext)

-- TODO: contextSetDiagnosticHandelr, contextGetDiagnosticHandler, contextGetDiagnosticContext, contextSetYieldCallback

wrapDirectly 'Missing.contextShouldDiscardValueNames "Retrieve whether the given context is set to discard all value names. "

wrapDirectly 'Missing.contextSetDiscardValueNames "Set whether the given context discards all value names.\nIf true, only the names of GlobalValue objects will be available in the IR. This can be used to save memory and runtime, especially in release mode."

-- | Return a string representation of the DiagnosticInfo.
getDiagInfoDescription :: (MonadIO io) => DiagnosticInfo -> io Text
getDiagInfoDescription (MkDiagnosticInfo ref) = liftIO $ wrapMessage =<< Missing.getDiagInfoDescription ref

-- TODO: getDiagInfoSeverity

-- TODO: getMDKindIDInContext

-- | Maps a synchronization scope name to a ID unique within this context.
getSyncScopeId :: (?context :: Context, MonadIO io) => Text -> io Int
getSyncScopeId name =
    liftIO $
        fromIntegral
            <$> withContext ?context \context ->
                Text.Foreign.withCStringLen name \(cstring, len) -> Missing.getSyncScopeId context cstring (fromIntegral len)

{- | Return an unique id given the name of a enum attribute, or 0 if no attribute by that name exists.
    See http://llvm.org/docs/LangRef.html#parameter-attributes and http://llvm.org/docs/LangRef.html#function-attributes for the list of available attributes.

    NB: Attribute names and/or id are subject to change without going through the C API deprecation cycle.
-}
getEnumAttributeKindForName :: (MonadIO io) => Text -> io Raw.AttributeKind
getEnumAttributeKindForName name = liftIO $ Text.Foreign.withCStringLen name \(cstring, len) -> Raw.getEnumAttributeKindForName cstring (fromIntegral len)

wrapDirectly 'Raw.getLastEnumAttributeKind ""

wrapDirectly 'Raw.createEnumAttribute "Create an enum attribute."

wrapDirectly 'Raw.getEnumAttributeKind "Get the unique id corresponding to the enum attribute passed as argument."

wrapDirectly 'Raw.getEnumAttributeValue "Get the enum attribute's value."

wrapDirectly 'Missing.createTypeAttribute "Create a type attribute."

wrapDirectly 'Missing.getTypeAttributeValue "Get the type attribute's value."

-- TODO: (this is hard because of the weird array arguments) wrapDirectly 'Missing.createConstantRangeAttribute "Create a ConstantRange attribute.\nLowerWords and UpperWords need to be NumBits divided by 64 rounded up elements long."

-- TODO: createDenormalFPEnvAttribute

-- | Create a string attribute.
createStringAttribute :: (MonadIO io, ?context :: Context) => Text -> Text -> io Wrappers.Attribute
createStringAttribute k v = liftIO $ withContext ?context \context -> do
    Foreign.Text.withCStringLen k \(kstr, klen) ->
        Foreign.Text.withCStringLen v \(vstr, vlen) ->
            MkAttribute <$> Raw.createStringAttribute context kstr (fromIntegral klen) vstr (fromIntegral vlen)

-- | Get the string attribute's kind.
getStringAttributeKind :: Wrappers.Attribute -> Text
getStringAttributeKind (MkAttribute attributeRef) = unsafePerformIO do
    alloca @CUInt \lengthPtr -> do
        cstring <- Raw.getStringAttributeKind attributeRef lengthPtr
        length <- peek lengthPtr
        Foreign.Text.peekCStringLen (cstring, fromIntegral length)

-- | Get the string attribute's value.
getStringAttributeValue :: Wrappers.Attribute -> Text
getStringAttributeValue (MkAttribute attributeRef) = unsafePerformIO do
    alloca @CUInt \lengthPtr -> do
        cstring <- Raw.getStringAttributeValue attributeRef lengthPtr
        length <- peek lengthPtr
        Foreign.Text.peekCStringLen (cstring, fromIntegral length)

wrapDirectly 'Raw.isEnumAttribute "Check for the different types of attributes."

wrapDirectly 'Raw.isStringAttribute ""

wrapDirectly 'Missing.isTypeAttribute ""

wrapAs "getTypeByName" 'Missing.getTypeByName2 "Obtain a Type from a context by its registered name.\n\nThis uses LLVMGetTypeByName2."
