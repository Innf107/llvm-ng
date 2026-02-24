{-# LANGUAGE TemplateHaskell #-}

module LLVM.Core.Phi where

import Data.Vector.Storable qualified as Storable
import LLVM.FFI.Core qualified as Raw
import LLVM.Internal.TH (wrapDirectly)
import LLVM.Internal.Wrappers (BasicBlock(..), Value(..))
import Data.Vector.Strict qualified as Vector

{- | Add an incoming value to the end of a PHI list.

(It actually adds an array of values to the end of the PHI list. This is what the official LLVM docs say though.)
-}
addIncoming :: Value -> Vector.Vector (Value, BasicBlock) -> IO ()
addIncoming (MkValue phi) incomingValues = do
    let valueVector = Vector.convert $ Vector.map (\(MkValue ref, _) -> ref) incomingValues
    let blockVector = Vector.convert $ Vector.map (\(_, MkBlock block) -> block) incomingValues
    let length = fromIntegral (Vector.length incomingValues)

    Storable.unsafeWith valueVector \values -> Storable.unsafeWith blockVector \blocks ->
        Raw.addIncoming phi values blocks length

wrapDirectly 'Raw.countIncoming "Obtain the number of incoming basic blocks to a PHI node."

wrapDirectly 'Raw.getIncomingValue "Obtain an incoming value to a PHI node."

wrapDirectly 'Raw.getIncomingBlock "Obtain the blcok of an incoming value to a PHI node."

