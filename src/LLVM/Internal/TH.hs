{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module LLVM.Internal.TH (wrapDirectly, wrapDirectlyPure, wrapAs, wrapAsPure) where

import Data.Traversable (for)
import Language.Haskell.TH (Name, Q)
import Language.Haskell.TH qualified as TH

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.List qualified as List
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text.Foreign qualified as Text.Foreign
import Data.Vector.Storable qualified as Storable
import Data.Word (Word64, Word8)
import Foreign (Ptr)
import Foreign.C (CDouble, CInt, CUInt)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)
import GHC.Stack (HasCallStack)
import LLVM.FFI.Core qualified as Raw
import LLVM.Internal.Wrappers (CStringLenAsByteString)
import LLVM.Internal.Wrappers qualified as Wrappers

wrapDirectly :: Name -> String -> TH.DecsQ
wrapDirectly rawFunctionName docString = wrapAs (TH.nameBase rawFunctionName) rawFunctionName docString

wrapDirectlyPure :: Name -> String -> TH.DecsQ
wrapDirectlyPure rawFunctionName docString = wrapAsPure (TH.nameBase rawFunctionName) rawFunctionName docString

wrapAs :: String -> Name -> String -> TH.DecsQ
wrapAs wrappedFunctionName rawFunctionName docString = wrapBase False wrappedFunctionName rawFunctionName docString

wrapAsPure :: String -> Name -> String -> TH.DecsQ
wrapAsPure wrappedFunctionName rawFunctionName docString = wrapBase True wrappedFunctionName rawFunctionName docString

wrapBase :: Bool -> String -> Name -> String -> TH.DecsQ
wrapBase isPure wrappedFunctionName rawFunctionName docString = do
    type_ <-
        TH.reify rawFunctionName >>= \case
            TH.VarI _ type_ _ -> pure type_
            _ -> fail $ "trying to wrap non-variable name: " <> show rawFunctionName

    let (rawArgumentTypes, rawResultType) = splitType type_

    let (parameterTypes, hasContext) = parseTypes rawArgumentTypes

    parameters <- for parameterTypes \type_ -> do
        name <- TH.newName "x"
        pure (name, type_)

    let parameterPatterns = flip mapMaybe parameters \case
            (_, Context) -> Nothing
            (name, _) -> Just (TH.varP name)

    (wrappedArgumentTypeMaybes, finalArguments, transformers) <-
        List.unzip3 <$> for parameters \(name, argumentType) ->
            case argumentType of
                Plain type_ -> wrapParameter type_ name
                Array elementType -> wrapArray elementType name
                CStringLenAsByteString -> wrapCStringLenAsByteString name
                Context -> do
                    name <- TH.newName "context"
                    pure (Nothing, [TH.varE name], (\body -> [|Wrappers.withContext ?context \ $(TH.varP name) -> $body|]))
    let wrappedArgumentTypes = catMaybes wrappedArgumentTypeMaybes

    (wrappedPureResultType, resultTransformer) <- wrapResult rawResultType

    monad <- TH.newName "io"
    wrappedResultType <-
        if isPure
            then pure wrappedPureResultType
            else [t|$(TH.varT monad) $(pure wrappedPureResultType)|]

    let unconstrainedFunctionType = pure $ foldr (\arg result -> TH.ArrowT `TH.AppT` arg `TH.AppT` result) wrappedResultType wrappedArgumentTypes

    let functionType = case (isPure, hasContext) of
            (True, False) -> unconstrainedFunctionType
            (False, False) -> [t|(MonadIO $(TH.varT monad)) => $(unconstrainedFunctionType)|]
            (True, True) -> [t|(?context :: Wrappers.Context) => $(unconstrainedFunctionType)|]
            (False, True) -> [t|(?context :: Wrappers.Context, MonadIO $(TH.varT monad)) => $(unconstrainedFunctionType)|]

    application <- foldl' TH.appE (TH.varE rawFunctionName) (concat finalArguments)

    let actualBody :: TH.ExpQ =
            [|
                do
                    result <- $(foldr ($) (pure application) transformers)
                    $(resultTransformer [|result|])
                |]
    let functionBody =
            if isPure
                then [|unsafePerformIO $actualBody|]
                else [|liftIO $actualBody|]

    TH.withDecsDoc docString $
        sequenceA
            [ TH.withDecDoc docString $ TH.sigD (TH.mkName wrappedFunctionName) functionType
            , TH.withDecDoc docString $ TH.funD (TH.mkName wrappedFunctionName) [TH.clause parameterPatterns (TH.normalB functionBody) []]
            ]

data ArgumentType
    = Plain TH.Type
    | Array TH.Type
    | CStringLenAsByteString
    | Context

parseTypes :: [TH.Type] -> ([ArgumentType], Bool)
parseTypes types = case types of
    (TH.AppT (TH.ConT ptrName) argument : TH.ConT uintName : rest)
        | ptrName == ''Ptr && (TH.nameBase uintName == "CUInt" || uintName == ''Word64) -> do
            let (parsed, hasContext) = parseTypes rest
            (Array argument : parsed, hasContext)
    (TH.ConT cStringAsByteStringName) : TH.ConT csizeName : rest
        | cStringAsByteStringName == ''CStringLenAsByteString && csizeName == ''CSize -> do
            let (parsed, hasContext) = parseTypes rest
            (CStringLenAsByteString : parsed, hasContext)
    (TH.ConT contextName : rest)
        | contextName == ''Raw.ContextRef -> do
            let (parsed, _hasContext) = parseTypes rest
            (Context : parsed, True)
    (type_ : rest) -> do
        let (parsed, hasContext) = parseTypes rest
        (Plain type_ : parsed, hasContext)
    [] -> ([], False)

wrapParameter :: TH.Type -> Name -> Q (Maybe TH.Type, [TH.ExpQ], TH.ExpQ -> TH.ExpQ)
wrapParameter rawType varName = case rawType of
    TH.ConT typeName
        | typeName == ''Raw.ContextRef -> undefined
        | typeName == ''Raw.ValueRef -> wrapNewtype ''Wrappers.Value 'Wrappers.MkValue
        | typeName == ''Raw.BuilderRef -> wrapWith ''Wrappers.Builder 'Wrappers.withBuilder
        | typeName == ''Raw.ModuleRef -> wrapWith ''Wrappers.Module 'Wrappers.withModule
        | typeName == ''Raw.BasicBlockRef -> wrapNewtype ''Wrappers.BasicBlock 'Wrappers.MkBlock
        | typeName == ''Raw.TypeRef -> wrapNewtype ''Wrappers.Type 'Wrappers.MkType
        | typeName == ''Raw.AttributeKind -> wrapIdentity typeName
        | typeName == ''Raw.AttributeRef -> wrapNewtype ''Wrappers.Attribute 'Wrappers.MkAttribute
        | typeName == ''Wrappers.FunctionTypeRef -> wrapNewtype ''Wrappers.FunctionType 'Wrappers.MkFunctionType
        | typeName == ''Wrappers.GlobalRef -> wrapNewtype ''Wrappers.Global 'Wrappers.MkGlobal
        | typeName == ''Wrappers.RawIntPredicate -> wrapFunction ''Wrappers.IntPredicate 'Wrappers.unwrapIntPredicate
        | typeName == ''Wrappers.RawRealPredicate -> wrapFunction ''Wrappers.RealPredicate 'Wrappers.unwrapRealPredicate
        | typeName == ''Wrappers.MetaDataRef -> wrapNewtype ''Wrappers.MetaData 'Wrappers.MkMetaData
        | typeName == ''Wrappers.RawFastMathFlags -> wrapNewtype ''Wrappers.FastMathFlags 'Wrappers.MkFastMathFlags
        | typeName == ''Wrappers.RawGEPNoWrapFlags -> wrapNewtype ''Wrappers.GEPNoWrapFlags 'Wrappers.MkGEPNoWrapFlags
        | typeName == ''Wrappers.RawLinkage -> wrapFunction ''Raw.Linkage 'Wrappers.unwrapLinkage
        | typeName == ''Wrappers.RawVisibility -> wrapFunction ''Raw.Visibility 'Wrappers.unwrapVisibility
        | typeName == ''Wrappers.RawDLLStorageClass -> wrapFunction ''Wrappers.DLLStorageClass 'Wrappers.unwrapDLLStorageClass
        | typeName == ''Wrappers.RawUnnamedAddr -> wrapFunction ''Wrappers.UnnamedAddr 'Wrappers.unwrapUnnamedAddr
        -- We have to do this horrible hack since llvm-ffi wraps its CUInt in a completely useless, but non-exported type synonym
        -- of the same name for some reason
        | TH.nameBase typeName == "CUInt" -> wrapFunction ''Int 'fromIntegral
        | TH.nameBase typeName == "CULLong" -> wrapFunction ''Word64 'fromIntegral
        | TH.nameBase typeName == "CDouble" -> wrapFunction ''Double 'doubleToCDouble
        | typeName == ''Word8 -> wrapIdentity typeName
        | typeName == ''Word64 -> wrapIdentity typeName
        | typeName == ''CSize -> wrapFunction ''Int 'fromIntegral
        -- llvm-ffi does not export the FunctionRef alias either...
        | TH.nameBase typeName == "FunctionRef" -> wrapNewtype ''Wrappers.Value 'Wrappers.MkValue
        | typeName == ''Raw.Bool -> wrapFunction ''Bool 'Raw.consBool
        | typeName == ''CString -> wrapWith ''Text 'Text.Foreign.withCString
        | otherwise -> fail $ "Unable to wrap unsupported type constructor " <> show typeName <> " in argument position"
    _ -> fail $ "Unable to wrap non-constructor parameter type " <> show rawType <> " in argument position"
  where
    wrapNewtype :: Name -> Name -> Q (Maybe TH.Type, [TH.ExpQ], TH.ExpQ -> TH.ExpQ)
    wrapNewtype wrappedTypeName constructorName = do
        nextVarName <- TH.newName "x"
        let bodyTransformer body = [|let $(TH.conP constructorName [TH.varP nextVarName]) = $(TH.varE varName) in $body|]
        pure (Just (TH.ConT wrappedTypeName), [TH.varE nextVarName], bodyTransformer)
    wrapWith :: Name -> Name -> Q (Maybe TH.Type, [TH.ExpQ], TH.ExpQ -> TH.ExpQ)
    wrapWith wrappedTypeName withFunctionName = do
        nextVarName <- TH.newName "x"
        let bodyTransformer body = [|$(TH.varE withFunctionName) $(TH.varE varName) \ $(TH.varP nextVarName) -> $body|]
        pure (Just (TH.ConT wrappedTypeName), [TH.varE nextVarName], bodyTransformer)
    wrapFunction :: Name -> Name -> Q (Maybe TH.Type, [TH.ExpQ], TH.ExpQ -> TH.ExpQ)
    wrapFunction wrappedTypeName wrappingFunctionName = do
        nextVarName <- TH.newName "x"
        let bodyTransformer body = [|let $(TH.varP nextVarName) = $(TH.varE wrappingFunctionName) $(TH.varE varName) in $body|]
        pure (Just (TH.ConT wrappedTypeName), [TH.varE nextVarName], bodyTransformer)
    wrapIdentity :: Name -> Q (Maybe TH.Type, [TH.ExpQ], TH.ExpQ -> TH.ExpQ)
    wrapIdentity typeName = pure (Just (TH.ConT typeName), [TH.varE varName], id)

wrapArray :: TH.Type -> Name -> Q (Maybe TH.Type, [TH.ExpQ], TH.ExpQ -> TH.ExpQ)
wrapArray elementType varName = case elementType of
    TH.ConT elementName
        | elementName == ''Raw.ValueRef -> withWrapper ''Wrappers.Value 'Wrappers.withValueArray
        | elementName == ''Raw.TypeRef -> withWrapper ''Wrappers.Type 'Wrappers.withTypeArray
        | elementName == ''Wrappers.OperandBundleRef -> withWrapper ''Wrappers.OperandBundle 'Wrappers.withOperandBundleArray
    _ -> fail $ "Unable to wrap array of unsupported elements type: " <> show elementType
  where
    withWrapper wrappedElementType function = do
        pointerName <- TH.newName "pointer"
        lengthName <- TH.newName "length"
        let bodyTransformer body = [|$(TH.varE function) $(TH.varE varName) \ $(TH.varP pointerName) $(TH.varP lengthName) -> $body|]
        vectorType <- [t|Storable.Vector $(TH.conT wrappedElementType)|]
        pure (Just vectorType, [TH.varE pointerName, [e|fromIntegral $(TH.varE lengthName)|]], bodyTransformer)

wrapCStringLenAsByteString :: Name -> Q (Maybe TH.Type, [TH.ExpQ], TH.ExpQ -> TH.ExpQ)
wrapCStringLenAsByteString varName = do
    pointerName <- TH.newName "pointer"
    lengthName <- TH.newName "length"
    let bodyTransformer body = [|ByteString.useAsCStringLen $(TH.varE varName) \($(TH.varP pointerName), $(TH.varP lengthName)) -> $body|]
    pure (Just (TH.ConT ''ByteString), [TH.varE pointerName, [e|fromIntegral $(TH.varE lengthName)|]], bodyTransformer)

wrapResult :: TH.Type -> Q (TH.Type, TH.ExpQ -> TH.ExpQ)
wrapResult rawType = case rawType of
    TH.AppT (TH.ConT monadName) (TH.TupleT 0)
        | monadName /= ''IO -> fail $ "Unable to wrap result in unsupported monad " <> show monadName
        | otherwise -> do
            let transformer body = [|pure $body|]
            pure (TH.ConT ''(), transformer)
    TH.AppT (TH.ConT monadName) (TH.ConT typeName)
        | monadName /= ''IO -> fail $ "Unable to wrap result in unsupported monad " <> show monadName
        | typeName == ''Raw.ValueRef -> wrapNewtype ''Wrappers.Value 'Wrappers.MkValue
        | typeName == ''Raw.TypeRef -> wrapNewtype ''Wrappers.Type 'Wrappers.MkType
        | typeName == ''Wrappers.FunctionTypeRef -> wrapNewtype ''Wrappers.FunctionType 'Wrappers.MkFunctionType
        | typeName == ''Wrappers.GlobalRef -> wrapNewtype ''Wrappers.Global 'Wrappers.MkGlobal
        | typeName == ''Raw.BasicBlockRef -> wrapNewtype ''Wrappers.BasicBlock 'Wrappers.MkBlock
        | typeName == ''Wrappers.MetaDataRef -> wrapNewtype ''Wrappers.MetaData 'Wrappers.MkMetaData
        | typeName == ''Wrappers.RawFastMathFlags -> wrapNewtype ''Wrappers.FastMathFlags 'Wrappers.MkFastMathFlags
        | typeName == ''Wrappers.RawGEPNoWrapFlags -> wrapNewtype ''Wrappers.GEPNoWrapFlags 'Wrappers.MkGEPNoWrapFlags
        | typeName == ''Wrappers.RawLinkage -> wrapFunction ''Raw.Linkage 'Wrappers.wrapLinkage
        | typeName == ''Wrappers.RawVisibility -> wrapFunction ''Raw.Visibility 'Wrappers.wrapVisibility
        | typeName == ''Wrappers.RawDLLStorageClass -> wrapFunction ''Wrappers.DLLStorageClass 'Wrappers.wrapDLLStorageClass
        | typeName == ''Wrappers.RawUnnamedAddr -> wrapFunction ''Wrappers.UnnamedAddr 'Wrappers.wrapUnnamedAddr
        | typeName == ''Wrappers.UnownedCString -> wrapMonadic ''Text 'Text.Foreign.peekCString
        | typeName == ''Raw.AttributeRef -> wrapNewtype ''Wrappers.Attribute 'Wrappers.MkAttribute
        | typeName == ''Raw.AttributeKind -> wrapIdentity typeName
        | typeName == ''CUInt -> wrapFunction ''Int 'fromIntegral
        | typeName == ''CInt -> wrapFunction ''Int 'fromIntegral
        | typeName == ''Word64 -> wrapIdentity typeName
        | TH.nameBase typeName == "CUInt" -> wrapFunction ''Int 'fromIntegral
        | TH.nameBase typeName == "CULLong" -> wrapFunction ''Word64 'fromIntegral
        | typeName == ''Raw.Bool -> wrapFunction ''Bool 'Raw.deconsBool
        | otherwise -> fail $ "Unable to wrap unsupported type constructor " <> show typeName <> " in return position"
    _ -> fail $ "Unable to wrap non-constructor parameter type " <> show rawType <> " in return position"
  where
    wrapNewtype wrappedTypeName constructorName = do
        let transformer body = [|pure ($(TH.conE constructorName) $body)|]
        pure (TH.ConT wrappedTypeName, transformer)
    wrapFunction wrappedTypeName functionName = do
        let transformer body = [|pure ($(TH.varE functionName) $body)|]
        pure (TH.ConT wrappedTypeName, transformer)
    wrapMonadic wrappedTypeName functionName = do
        let transformer body = [|($(TH.varE functionName) $body)|]
        pure (TH.ConT wrappedTypeName, transformer)
    wrapIdentity typeName = pure (TH.ConT typeName, \body -> [|pure $body|])

splitType :: TH.Type -> ([TH.Type], TH.Type)
splitType type_ = case type_ of
    TH.AppT (TH.AppT TH.ArrowT argument) result -> do
        let (rest, finalResult) = splitType result
        (argument : rest, finalResult)
    _ -> ([], type_)

doubleToCDouble :: Double -> CDouble
doubleToCDouble double = fromRational (toRational double)
