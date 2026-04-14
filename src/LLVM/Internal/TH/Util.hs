{-# LANGUAGE TemplateHaskell #-}

module LLVM.Internal.TH.Util where

import Foreign.C (CUInt (CUInt))
import GHC.ForeignPtr (ForeignPtr (ForeignPtr))
import GHC.Ptr (Ptr (Ptr))
import Language.Haskell.TH qualified as TH

cEnum :: String -> [String] -> TH.DecsQ
cEnum name constructors = cEnumWithValues name (zip [0 ..] constructors)

cEnumWithValues :: String -> [(Int, String)] -> TH.DecsQ
cEnumWithValues name constructors = do
    let rawTypeName = TH.mkName ("Raw" <> name)
    let typeName = TH.mkName name
    let wrapName = TH.mkName ("wrap" <> name)
    let unwrapName = TH.mkName ("unwrap" <> name)

    rawTypeDefinition <- TH.tySynD rawTypeName [] [t|CUInt|]

    typeDefinition <-
        TH.dataD
            (TH.cxt [])
            typeName
            []
            Nothing
            [TH.normalC (TH.mkName name) [] | (_, name) <- constructors]
            []

    wrapSignature <- TH.sigD wrapName [t|$(TH.conT rawTypeName) -> $(TH.conT typeName)|]
    unwrapSignature <- TH.sigD unwrapName [t|$(TH.conT typeName) -> $(TH.conT rawTypeName)|]

    wildcardName <- TH.newName "x"
    wrapDefinition <-
        TH.funD
            wrapName
            ( [ TH.clause [TH.litP (TH.integerL (fromIntegral index))] (TH.normalB (TH.conE (TH.mkName constructorName))) []
              | (index, constructorName) <- constructors
              ]
                <> [TH.clause [(TH.varP wildcardName)] (TH.normalB [e|error $ "Invalid value for enum: " <> show $(TH.varE wildcardName)|]) []]
            )

    unwrapDefinition <-
        TH.funD
            unwrapName
            ( [ TH.clause [TH.conP (TH.mkName constructorName) []] (TH.normalB (TH.litE (TH.integerL (fromIntegral index)))) []
              | (index, constructorName) <- constructors
              ]
            )

    pure $ [rawTypeDefinition, typeDefinition, wrapSignature, wrapDefinition, unwrapSignature, unwrapDefinition]

pointerWrapper :: String -> TH.DecsQ
pointerWrapper name = do
    let opaqueName = (TH.mkName ("Opaque" <> name))
    let refName = (TH.mkName (name <> "Ref"))

    opaqueDefinition <- TH.dataD (TH.cxt []) opaqueName [] Nothing [] []
    refDefinition <- TH.tySynD refName [] [t|Ptr $(TH.conT opaqueName)|]
    wrapperDefinition <-
        TH.newtypeD
            (TH.cxt [])
            (TH.mkName name)
            []
            Nothing
            (TH.normalC (TH.mkName ("Mk" <> name)) [TH.bangType (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness) (TH.conT refName)])
            []

    pure $ [opaqueDefinition, refDefinition, wrapperDefinition]

foreignPointerWrapper :: String -> TH.DecsQ
foreignPointerWrapper name = do
    let opaqueName = TH.mkName ("Opaque" <> name)
    let refName = TH.mkName (name <> "Ref")
    let withWrapperName = TH.mkName ("with" <> name)
    let dataConName = TH.mkName ("Mk" <> name)

    opaqueDefinition <- TH.dataD (TH.cxt []) opaqueName [] Nothing [] []
    refDefinition <- TH.tySynD refName [] [t|Ptr $(TH.conT opaqueName)|]
    wrapperDefinition <-
        TH.newtypeD
            (TH.cxt [])
            (TH.mkName name)
            []
            Nothing
            (TH.normalC dataConName [TH.bangType (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness) [t|ForeignPtr $(TH.conT opaqueName)|]])
            []
    withWrapperSignature <- TH.sigD withWrapperName [t|forall a. $(TH.conT (TH.mkName name)) -> ($(TH.conT refName) -> IO a) -> IO a|]

    foreignPtrName <- TH.newName "foreignPtr"
    contName <- TH.newName "cont"
    withWrapperDefinition <-
        TH.funD
            withWrapperName
            [ ( TH.clause
                    [TH.conP dataConName [TH.varP foreignPtrName], TH.varP contName]
                    (TH.normalB [|withForeignPtr $(TH.varE foreignPtrName) $(TH.varE contName)|])
                    []
              )
            ]

    pure $ [opaqueDefinition, refDefinition, wrapperDefinition, withWrapperSignature, withWrapperDefinition]
