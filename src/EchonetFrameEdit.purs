{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module EchonetFrameEdit
  ( Input
  , Output(..)
  , Query(..)
  , component
  ) where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import EchonetLite (ESV(..), EDatum, stringWithZeroPadding)
import EchonetLite as EchonetLite
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import URI.Host.IPv6Address (IPv6Address)
import Web.Clipboard (Clipboard)
import Web.Clipboard as Clipboard
import Web.HTML (window)
import Web.HTML.Window as Window

-- Icons
iconPlus ∷ forall w i. HH.HTML w i
iconPlus = HH.i [ HP.classes [ ClassName "bi", ClassName "bi-plus" ] ] []

iconTrash ∷ forall w i. HH.HTML w i
iconTrash = HH.i [ HP.classes [ ClassName "bi", ClassName "bi-trash" ] ] []

type Input
  = Maybe IPv6Address

data Output
  = SubmitToEchonetLiteMessage (Array UInt)
  | UpdateEchonetFrame ArrayBuffer

type State
  = { maybeClipboard :: Maybe Clipboard
    , frame :: EchonetLite.Frame
    , maybeIpv6Address :: Maybe IPv6Address
    }

data Query a
  = Handle a

data Action
  = Initialize
  | Finalize
  | HandleInput Input
  | OnValueChangeInput Int String
  | OnClickCopyToClipboard String
  | OnClickSubmit (Array UInt)
  | OnClickAddEDatum
  | OnClickRemoveEDatum Int

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Initialize
              , finalize = Just Finalize
              , receive = Just <<< HandleInput
              }
    }

initialState :: Input -> State
initialState maybeIpv6Address =
  { maybeClipboard: Nothing
  , maybeIpv6Address: maybeIpv6Address
  , frame:
      { ehd1: UInt.fromInt 0x10
      , ehd2: UInt.fromInt 0x81
      , tid: EchonetLite.TID (UInt.fromInt 0x1234)
      , seoj: EchonetLite.makeEOJ (UInt.fromInt 0x05) (UInt.fromInt 0xFF) (UInt.fromInt 0x01)
      , deoj: EchonetLite.makeEOJ (UInt.fromInt 0x02) (UInt.fromInt 0x88) (UInt.fromInt 0x01)
      , esv: Get
      , edata: NonEmptyArray.singleton initialEDatum
      }
  }

initialEDatum :: EchonetLite.EDatum
initialEDatum = { epc: UInt.fromInt 0xE7, edt: [] }

render :: forall input m. State -> H.ComponentHTML Action input m
render = formFrame

formFrame :: forall input m. State -> H.ComponentHTML Action input m
formFrame state =
  HH.div [ HP.classes [ HB.p2, HB.alert, HB.alertInfo ] ]
    [ HH.div [ HP.classes [ HB.p2 ] ] [ HH.text "ECHONET Lite フレーム" ]
    , HH.div [ HP.classes [ HB.p2 ] ]
        [ item "EHD1" (toHexStringCapitalized state.frame.ehd1) "固定値"
        , item "EHD2" (toHexStringCapitalized state.frame.ehd2) "固定値"
        , item "TID" (String.toUpper $ EchonetLite.toStringTID state.frame.tid) "このフォームでは編集できません"
        , item "SEOJ" (String.toUpper $ EchonetLite.toStringEOJ state.frame.seoj) "コントローラー"
        , item "DEOJ" (String.toUpper $ EchonetLite.toStringEOJ state.frame.deoj) "スマートメーター"
        , item "ESV" (toHexStringCapitalized $ EchonetLite.toUIntESV $ state.frame.esv) $ show state.frame.esv <> "要求"
        , item "OPC" (toHexStringCapitalized $ EchonetLite.getOPC state.frame) "EDATAの数"
        , HH.div [ HP.classes [ HB.p2 ] ]
            [ HH.div [ HP.classes [ HB.m1 ] ]
                [ HH.text "EDATA"
                , HH.span [ HP.classes [ HB.m1 ] ]
                    [ HH.button
                        [ HP.type_ HP.ButtonButton
                        , HP.classes [ HB.btn, HB.btnSm, HB.btnOutlineDark ]
                        , HE.onClick \_ -> OnClickAddEDatum
                        ]
                        [ iconPlus ]
                    ]
                , HH.div [ HP.classes [ HB.m1 ] ]
                    $ NonEmptyArray.toArray
                    $ NonEmptyArray.mapWithIndex formEData state.frame.edata
                ]
            ]
        ]
    ]
  where
  toHexStringCapitalized =
    String.toUpper <<< stringWithZeroPadding 2
      <<< Int.toStringAs Int.hexadecimal
      <<< UInt.toInt

  item label value trailer =
    HH.div [ HP.classes [ HB.row, HB.m1 ] ]
      [ HH.label [ HP.classes [ HB.colSm2 ] ] [ HH.text label ]
      , HH.span [ HP.classes [ HB.colSm5 ] ] [ HH.text value ]
      , HH.span [ HP.classes [ HB.colSm5 ] ] [ HH.text trailer ]
      ]

formEData :: forall input m. Int -> EDatum -> H.ComponentHTML Action input m
formEData index datum =
  HH.div [ HP.classes [ HB.p2, HB.border ] ]
    [ HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes [ HB.btn, HB.btnSm, HB.btnOutlineDark ]
        , HE.onClick \_ -> OnClickRemoveEDatum index
        ]
        [ iconTrash ]
    , epc { label: "EPC", trailer: "要求" }
    , item "PDC" (Int.toStringAs Int.hexadecimal $ UInt.toInt $ EchonetLite.getPDC datum) "この後に続くEDTのバイト数"
    , item "EDT" "" "Get要求にEDTはありません"
    ]
  where
  item label value trailer =
    HH.div [ HP.classes [ HB.row, HB.m1 ] ]
      [ HH.label [ HP.classes [ HB.colSm2 ] ] [ HH.text label ]
      , HH.span [ HP.classes [ HB.colSm5 ] ] [ HH.text value ]
      , HH.span [ HP.classes [ HB.colSm5 ] ] [ HH.text trailer ]
      ]

  epc { label, trailer } =
    HH.div [ HP.classes [ HB.row, HB.m1 ] ]
      [ HH.label [ HP.classes [ HB.colSm2, HB.colFormLabel ] ] [ HH.text label ]
      , HH.div [ HP.classes [ HB.colSm5 ] ]
          [ HH.select
              [ HP.class_ HB.formControl
              , HE.onValueChange (OnValueChangeInput index)
              ]
              [ option "80" "0x80:動作状態"
              , option "81" "0x81:設置場所"
              , option "88" "0x88:異常発生状態"
              , option "8A" "0x8A:メーカーコード"
              --
              , option "D3" "0xD3:係数"
              , option "D7" "0xD7:積算電力量有効桁数"
              , option "E0" "0xE0:積算電力量計測値"
              , option "E1" "0xE1:積算電力量単位"
              , option "E2" "0xE2:積算電力量計測値履歴1"
              , option "E7" "0xE7:瞬時電力計測値"
              , option "E8" "0xE8:瞬時電流計測値"
              , option "EA" "0xEA:正方向定時積算電力量計測値"
              , option "EB" "0xEB:逆方向定時積算電力量計測値"
              ]
          ]
      , HH.div [ HP.classes [ HB.colSm5 ] ]
          [ HH.text trailer ]
      ]
    where
    option v t =
      HH.option
        [ HP.value v
        , HP.selected (Int.fromStringAs Int.hexadecimal v == (Just $ UInt.toInt datum.epc))
        ]
        [ HH.text t ]

handleAction :: forall input m. MonadAff m => Action -> H.HalogenM State Action input Output m Unit
handleAction = case _ of
  Initialize -> do
    navigator <- H.liftEffect $ Window.navigator =<< window
    clipboard <- H.liftEffect $ Clipboard.clipboard navigator
    H.modify_ _ { maybeClipboard = clipboard }
    currentFrame <- H.gets _.frame
    arraybuffer <- H.liftEffect $ EchonetLite.serializeEchonetLiteFrame currentFrame
    H.raise $ UpdateEchonetFrame arraybuffer
  Finalize -> mempty
  HandleInput newInput -> do
    currentState <- H.get
    when (currentState.maybeIpv6Address /= newInput) do
      H.put $ currentState { maybeIpv6Address = newInput }
      arraybuffer <- H.liftEffect $ EchonetLite.serializeEchonetLiteFrame currentState.frame
      H.raise $ UpdateEchonetFrame arraybuffer
  OnValueChangeInput index text -> do
    frame <- H.gets _.frame
    let
      (newEpc :: Maybe UInt) = UInt.fromInt' =<< Int.fromStringAs Int.hexadecimal text

      (newEData :: Maybe (NonEmptyArray EchonetLite.EDatum)) = updateAtEData index frame.edata =<< newEpc
    case frame { edata = _ } <$> newEData of
      Just newFrame -> do
        H.modify_ _ { frame = newFrame }
        arraybuffer <- H.liftEffect $ EchonetLite.serializeEchonetLiteFrame newFrame
        H.raise $ UpdateEchonetFrame arraybuffer
      Nothing -> mempty
  OnClickCopyToClipboard s -> do
    maybeClipboard <- H.gets _.maybeClipboard
    case maybeClipboard of
      Nothing -> mempty
      Just clipboard -> void $ H.liftEffect $ Clipboard.writeText s clipboard
  OnClickSubmit arr -> H.raise $ SubmitToEchonetLiteMessage arr
  OnClickAddEDatum -> do
    frame <- H.gets _.frame
    let
      (newEData :: NonEmptyArray EchonetLite.EDatum) = addEData frame.edata initialEDatum

      (newFrame :: EchonetLite.Frame) = frame { edata = newEData }
    H.modify_ _ { frame = newFrame }
    arraybuffer <- H.liftEffect $ EchonetLite.serializeEchonetLiteFrame newFrame
    H.raise $ UpdateEchonetFrame arraybuffer
  OnClickRemoveEDatum index -> do
    frame <- H.gets _.frame
    let
      (newEData :: Maybe (NonEmptyArray EchonetLite.EDatum)) = removeEData index frame.edata
    case frame { edata = _ } <$> newEData of
      Just newFrame -> do
        H.modify_ _ { frame = newFrame }
        arraybuffer <- H.liftEffect $ EchonetLite.serializeEchonetLiteFrame newFrame
        H.raise $ UpdateEchonetFrame arraybuffer
      Nothing -> mempty
  where
  addEData :: NonEmptyArray EchonetLite.EDatum -> EchonetLite.EDatum -> NonEmptyArray EchonetLite.EDatum
  addEData edata datum = NonEmptyArray.snoc edata datum

  removeEData :: Int -> NonEmptyArray EchonetLite.EDatum -> Maybe (NonEmptyArray EchonetLite.EDatum)
  removeEData index edata = do
    newArray <- NonEmptyArray.deleteAt index edata
    NonEmptyArray.fromArray newArray

  updateAtEData :: Int -> NonEmptyArray EchonetLite.EDatum -> UInt -> Maybe (NonEmptyArray EchonetLite.EDatum)
  updateAtEData index edata epc = do
    item <- NonEmptyArray.index edata index
    let
      newItem = item { epc = epc }
    NonEmptyArray.updateAt index newItem edata

handleQuery :: forall input m a. MonadAff m => Query a -> H.HalogenM State Action input Output m (Maybe a)
handleQuery = case _ of
  Handle next -> do
    pure (Just next)
