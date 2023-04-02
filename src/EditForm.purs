{-
 https://github.com/ak1211/smartmeter-route-b-app
 Copyright (c) 2023 Akihiro Yamamoto.
 Licensed under the MIT License.
 See LICENSE file in the project root for full license information.
-}
module EditForm
  ( Input
  , Output(..)
  , Query(..)
  , component
  ) where

import Prelude
import Bp35a1 as Bp35a1
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Char (fromCharCode, toCharCode)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.Unsafe as StringUnsafe
import Data.UInt (UInt)
import Data.UInt as UInt
import EchonetLite (EDatum, ESV(..), stringWithZeroPadding)
import EchonetLite as EchonetLite
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
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
  = { maybeIpV6Address :: Maybe Bp35a1.IpV6Address }

data Output
  = SubmitToEchonetLiteMessage (Array UInt)

type State
  = { maybeClipboard :: Maybe Clipboard
    , frame :: EchonetLite.Frame
    , maybeIpV6Address :: Maybe Bp35a1.IpV6Address
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
initialState input =
  { maybeClipboard: Nothing
  , maybeIpV6Address: input.maybeIpV6Address
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
render state =
  HH.div_
    [ HH.h2_ [ HH.text "送信するECHONET Lite 電文を組み立てる" ]
    , HH.div [ HP.classes [ HB.alert, HB.alertLight ] ]
        [ HH.pre [ HP.classes [ HB.p2, HB.bgDark, HB.textWhite ] ] [ HH.text displayMessage ]
        , HH.div [ HP.class_ HB.row ]
            [ HH.div [ HP.class_ HB.col ] []
            , HH.button
                [ HP.classes [ HB.m1, HB.col3, HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
                , HP.type_ HP.ButtonButton
                , HE.onClick \_ -> OnClickCopyToClipboard displayMessage
                ]
                [ HH.text "copy to clipboard" ]
            , HH.button
                [ HP.classes [ HB.m1, HB.col3, HB.btn, HB.btnSm, HB.btnPrimary ]
                , HP.type_ HP.ButtonButton
                , HE.onClick \_ -> OnClickSubmit $ commandline.header <> [ whitespace ] <> commandline.payload
                ]
                [ HH.text "送信" ]
            ]
        ]
    , HH.p_ [ HH.text "送信電文はバイナリなのでそれを組み立てるフォームを用意しました" ]
    , formFrame state
    ]
  where
  whitespace :: UInt
  whitespace = UInt.fromInt 0x20

  commandline :: { header :: Array UInt, payload :: Array UInt }
  commandline = makeCommandLine state.maybeIpV6Address state.frame

  displayMessage :: String
  displayMessage =
    let
      h =
        String.joinWith ""
          $ map (String.singleton <<< String.codePointFromChar)
          $ Array.mapMaybe (fromCharCode <<< UInt.toInt) commandline.header

      pl =
        String.joinWith ""
          $ map enc
          $ Array.mapMaybe (fromCharCode <<< UInt.toInt) commandline.payload
    in
      String.joinWith " " [ h, pl ]
    where
    enc :: Char -> String
    enc char =
      "\\x"
        <> ( String.toUpper
              $ stringWithZeroPadding 2
              $ Int.toStringAs Int.hexadecimal
              $ toCharCode char
          )

makeCommandLine :: Maybe Bp35a1.IpV6Address -> EchonetLite.Frame -> { header :: Array UInt, payload :: Array UInt }
makeCommandLine ipv6 frame =
  let
    fr = serializeFrame frame

    partialCommand =
      String.joinWith " "
        [ "SKSENDTO"
        , "1"
        , maybe "[IpV6address]" Bp35a1.toStringIpV6Address ipv6
        , "0E1A"
        , "1"
        , String.toUpper $ stringWithZeroPadding 4 $ Int.toStringAs Int.hexadecimal $ Array.length fr
        ]
  in
    { header: toChars partialCommand, payload: fr }
  where
  toChars :: String -> Array UInt
  toChars str =
    map (UInt.fromInt <<< toCharCode <<< StringUnsafe.char <<< String.singleton)
      $ String.toCodePointArray str

serializeFrame ∷ EchonetLite.Frame -> Array UInt
serializeFrame frame =
  Array.concat
    [ [ frame.ehd1, frame.ehd2 ]
    , EchonetLite.toBigEndiannessTID frame.tid
    , EchonetLite.toBigEndiannessEOJ frame.seoj
    , EchonetLite.toBigEndiannessEOJ frame.deoj
    , [ EchonetLite.toUIntESV frame.esv ]
    , [ EchonetLite.getOPC frame ]
    , Array.concatMap serializeEData $ NonEmptyArray.toArray frame.edata
    ]
  where
  serializeEData :: EchonetLite.EDatum -> Array UInt
  serializeEData datum =
    Array.concat
      [ [ datum.epc, EchonetLite.getPDC datum ]
      , datum.edt
      ]

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
    H.modify_ _ { maybeClipboard = Just clipboard }
  Finalize -> mempty
  HandleInput input -> do
    current <- H.gets _.maybeIpV6Address
    when (current /= input.maybeIpV6Address)
      $ H.modify_ _ { maybeIpV6Address = input.maybeIpV6Address }
  OnValueChangeInput index text -> do
    let
      newEpc = UInt.fromInt' =<< Int.fromStringAs Int.hexadecimal text
    frame <- H.gets _.frame
    maybe mempty (\x -> H.modify_ _ { frame = frame { edata = x } }) $ updateAtEData index frame.edata =<< newEpc
  OnClickCopyToClipboard s -> do
    maybeClipboard <- H.gets _.maybeClipboard
    case maybeClipboard of
      Nothing -> mempty
      Just clipboard -> void $ H.liftEffect $ Clipboard.writeText s clipboard
  OnClickSubmit arr -> H.raise $ SubmitToEchonetLiteMessage arr
  OnClickAddEDatum -> do
    frame <- H.gets _.frame
    H.modify_ _ { frame = frame { edata = addEData frame.edata initialEDatum } }
  OnClickRemoveEDatum index -> do
    frame <- H.gets _.frame
    maybe mempty (\x -> H.modify_ _ { frame = frame { edata = x } }) $ removeEData index frame.edata
  where
  addEData edata datum = NonEmptyArray.snoc edata datum

  removeEData index edata = do
    newArray <- NonEmptyArray.deleteAt index edata
    NonEmptyArray.fromArray newArray

  updateAtEData index edata epc = do
    item <- NonEmptyArray.index edata index
    let
      newItem = item { epc = epc }
    NonEmptyArray.updateAt index newItem edata

handleQuery :: forall input m a. MonadAff m => Query a -> H.HalogenM State Action input Output m (Maybe a)
handleQuery = case _ of
  Handle next -> do
    pure (Just next)
