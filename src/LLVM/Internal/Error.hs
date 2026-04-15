module LLVM.Internal.Error (withErrorMessage, LLVMError (..)) where

import Control.Exception (Exception, mask, throwIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Foreign qualified as Text.Foreign
import Foreign (Ptr, alloca, peek)
import Foreign.C (CString)
import GHC.Stack.Types (HasCallStack)
import LLVM.FFI.Core qualified as Raw

withErrorMessage :: (HasCallStack) => Maybe Text -> (Ptr CString -> IO Raw.Bool) -> IO ()
withErrorMessage context cont = do
    alloca @CString \errorMessagePtr -> mask \restore -> do
        hasErrored <- restore (cont errorMessagePtr)
        case Raw.deconsBool hasErrored of
            False -> pure ()
            True -> do
                errorMessageCString <- peek errorMessagePtr
                errorMessage <- Text.Foreign.peekCString errorMessageCString
                Raw.disposeMessage errorMessageCString

                throwIO
                    ( MkLLVMError
                        { message = errorMessage
                        , context
                        }
                    )

data LLVMError = MkLLVMError {message :: Text, context :: Maybe Text}

instance Show LLVMError where
    show (MkLLVMError{message, context = Nothing}) = "LLVM error: " <> Text.unpack message
    show (MkLLVMError{message, context = Just context}) = "LLVM error: " <> Text.unpack message <> " (in a call to " <> Text.unpack context <> ")"
instance Exception LLVMError
