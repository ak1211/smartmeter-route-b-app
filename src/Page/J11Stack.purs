{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module Page.J11Stack
  ( Action(..)
  , Output(..)
  , component
  ) where

import Prelude
import Control.Alternative (guard)
import Data.Array as Array
import Data.ArrayBuffer.DataView as DV
import Data.Either (Either(..))
import Data.Either.Nested (either3)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Unfoldable (unfoldr1)
import EchonetFrameEdit as EchonetFrameEdit
import EchonetLite as EchonetLite
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Effect.Exception as Exc
import Effect.Now as Now
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import HexEdit as HexEdit
import Page.ToastMessage (ToastMessage(..))
import Partial.Unsafe (unsafePartial)
import ProtocolStack.J11StackUart.Command as C
import ProtocolStack.J11StackUart.Command.Types (J11Command(..), J11CommandFormat)
import ProtocolStack.J11StackUart.Command.Types as CT
import ProtocolStack.J11StackUart.Response.Types as R
import Type.Proxy (Proxy(..))
import URI.Host.IPv6Address (IPv6Address)
import URI.Host.IPv6Address as IPv6Address
import Utility as Utility
import Web.Clipboard (Clipboard)
import Web.Clipboard as Clipboard
import Web.HTML (window)
import Web.HTML.Window as Window

-- Icons
iconExclamationDiamondFill ∷ forall w i. HH.HTML w i
iconExclamationDiamondFill = HH.i [ HP.classes [ ClassName "bi", ClassName "bi-exclamation-diamond-fill" ] ] []

data Action
  = Initialize
  | Finalize
  | HandleEchonetFrameEditUpdate EchonetFrameEdit.Output
  | HandleHexEditUpdate HexEdit.Output
  | OnClickActivescanCommandButton
  | OnClickClosePortButton
  | OnClickCopyToClipboard String
  | OnClickOpenPortButton
  | OnClickSetJ11CommandFormatButton J11CommandFormat
  | OnClickSetPanaAuthCommandButton
  | OnClickSetJ11CommandButton J11Command
  | OnValueChangeBoudRateOption String
  | OnValueChangeScanDurationOption String
  | OnValueInputRouteBIdInputArea String
  | OnValueInputRouteBPasswordInputArea String

data Output
  = NewToastMessages (Array ToastMessage)

type State
  = { maybeClipboard :: Maybe Clipboard
    , serialportOpened :: Boolean
    , boudRate :: Int
    , maybeRouteBId :: Maybe String
    , maybeRouteBPassword :: Maybe String
    , scanDuration :: String -- アクティブスキャンの設定値
    , maybeActivescanAppendix :: Maybe R.ActivescanAppendix
    , maybeStartBRouteAppendix :: Maybe R.StartBRouteAppendix
    , maybeIpv6Address :: Maybe IPv6Address
    , commandTransmitData :: J11Command
    }

--
type ChildSlots
  = ( hexedit :: H.Slot HexEdit.Query HexEdit.Output Unit
    , echonetframeedit :: H.Slot EchonetFrameEdit.Query EchonetFrameEdit.Output Unit
    )

_hexedit = Proxy :: Proxy "hexedit"

_echonetframeedit = Proxy :: Proxy "echonetframeedit"

component :: forall query input m. MonadAff m => H.Component query input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , finalize = Just Finalize
              }
    }

initialState :: forall i. i -> State
initialState _ =
  { maybeClipboard: Nothing
  , serialportOpened: false
  , boudRate: 115200
  , maybeRouteBId: Nothing
  , maybeRouteBPassword: Nothing
  , scanDuration: "6"
  , maybeActivescanAppendix: Nothing
  , maybeStartBRouteAppendix: Nothing
  , maybeIpv6Address: Nothing
  , commandTransmitData: J11Command []
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.p [ HP.class_ HB.m1 ]
        [ HH.text "Wi-SUN Enhanced HAN plus B-Route Dual stackプロトコルスタックのモジュール"
        , HH.ul_
            [ HH.li_ [ HH.text "USB-シリアル変換器につないだ BP35C0-J11モジュール" ]
            , HH.li_ [ HH.text "USBドングル BP35C2-J11-T01" ]
            , HH.li_ [ HH.text "USBドングル RS-WSUHA-J11" ]
            ]
        , HH.text "などをUSBポートに挿入する。"
        ]
    , HH.p_ [ HH.text "準備ができたら、以下のボタンを押してシリアルポートを開きます。" ]
    , HH.div [ HP.class_ HB.m3 ]
        [ let
            strBoudRate = Int.toStringAs Int.decimal state.boudRate

            option s =
              HH.option [ HP.value s, HP.selected $ s == strBoudRate ]
                [ HH.text (s <> " bps") ]
          in
            HH.select
              [ HP.classes [ HB.formSelectSm ]
              , HE.onValueChange OnValueChangeBoudRateOption
              , HP.disabled state.serialportOpened
              ]
              [ option "9600"
              , option "19200"
              , option "38400"
              , option "57600"
              , option "115200"
              ]
        , HH.button
            [ HP.classes
                $ Array.concat
                    [ [ HB.m1, HB.btn ]
                    , if state.serialportOpened then
                        [ HB.disabled, HB.btnSecondary ]
                      else
                        [ HB.btnPrimary ]
                    ]
            , HE.onClick \_ -> OnClickOpenPortButton
            ]
            [ HH.text "シリアルポートを開く" ]
        , HH.button
            [ HP.classes
                $ Array.concat
                    [ [ HB.m1, HB.btn ]
                    , if state.serialportOpened then
                        [ HB.btnPrimary ]
                      else
                        [ HB.disabled, HB.btnSecondary ]
                    ]
            , HE.onClick \_ -> OnClickClosePortButton
            ]
            [ HH.text "シリアルポートを閉じる" ]
        ]
    --
    , HH.h2_ [ HH.text "BP35C0-J11と会話してみる" ]
    , HH.p_
        [ HH.text "コマンド仕様は\"BP35C0-J11 UART IF 仕様書\"で検索。"
        , HH.br_
        , HH.p_
            [ HH.text "まあ、"
            , HH.a
                [ HP.href "https://github.com/Interested-In-Spresense/BP35C0-J11/blob/eb826b025090e90b05dfe8a328253dd58ad2e378/documents/BP35C0-J11(Wi-SUN%20Enhanced%20HAN)/J11_UART_IF%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E4%BB%95%E6%A7%98%E6%9B%B8_%E7%AC%AC1.1%E7%89%88.pdf" ]
                [ HH.text "これ" ]
            , HH.text "なんですが。"
            ]
        ]
    , HH.h5_ [ HH.text "前提条件" ]
    , HH.p_
        [ HH.text "「2 UART IFコマンド」に書いてある内容。"
        , HH.ul_
            [ HH.li_ [ HH.text "全データバイナリ" ]
            , HH.li_ [ HH.text "全てビッグエンディアン" ]
            , HH.li_ [ HH.text "コマンド種別は「要求」「応答」「通知」の3種類" ]
            ]
        ]
    , HH.p_
        [ HH.text "「2.3 コマンドフォーマット」に書いてある内容。"
        , HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト)" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト)" ]
            , HH.li_ [ HH.text "メッセージ長(チェックサムとデータ部の長さを足した値, 2バイト)" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト)" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト)" ]
            , HH.li_ [ HH.text "データ(任意バイト)" ]
            ]
        ]
    , HH.p_
        [ HH.text "ユニークコードはこの中のどれか"
        , HH.ul_
            [ HH.li_ [ HH.text "要求コマンド: D0 EA 83 FC" ]
            , HH.li_ [ HH.text "応答コマンド: D0 F9 EE 5D" ]
            , HH.li_ [ HH.text "通知コマンド: D0 F9 EE 5D" ]
            ]
        ]
    , HH.p_ [ HH.text "コマンドパッドに「PORT OPENED」が表示されているのを確認したら、疎通確認のためにとりあえずファームウェアバージョン取得コマンドを送信する。" ]
    --
    , HH.h3_ [ HH.text "ファームウェアバージョン取得コマンド" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 6B" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 04" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 03 A8" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 00 00" ]
            ]
        , HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetJ11CommandFormatButton C.commandGetFirmwareVersion
            ]
            [ HH.text "ファームウェアバージョン取得コマンド" ]
        ]
    , HH.p [ HP.classes [ HB.p2, HB.bgDark, HB.textWhite ] ]
        [ HH.text "2025-03-03T13:11:24.369Z 送信 -> D0 EA 83 FC 00 6B 00 04 03 A8 00 00"
        , HH.br_
        , HH.text "2025-03-03T13:11:24.395Z 受信 <- D0 F9 EE 5D 20 6B 00 0D 03 AC 00 A8 01 04 00 01 03 00 00 15 8A"
        ]
    , HH.p_
        [ HH.text "こんな応答があれば通信ができている。"
        , HH.text "そうでなければ接続を見直してください。"
        ]
    --
    , HH.h2 [ HP.classes [ HB.my5 ] ] [ HH.text "スマートメーターと接続する" ]
    , HH.p_
        [ HH.text "送配電事業者からもらった認証IDとパスワードを用意して、"
        , HH.text "「7.2 Ｂルート接続」を参考にして"
        , HH.br_
        , HH.text "「7.2.3 Ｂルートペアリングシーケンス」と「7.2.4 Ｂルート通常接続シーケンス」に沿って接続を行う。"
        ]
    --
    , HH.h3_ [ HH.text "1. ハードウェアリセット" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 D9" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 04" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 00 00" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 00 00" ]
            ]
        , HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetJ11CommandFormatButton C.commandHardwareReset
            ]
            [ HH.text "ハードウェアリセットコマンド" ]
        ]
    --
    , HH.h3_ [ HH.text "2. 初期設定" ]
    , HH.p_ [ HH.text "初回は仮のチャネルを設定する。通信チャネルはスキャン後に再設定する" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 5F" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 08" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 00 00" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 00 00" ]
            , HH.li_
                [ HH.text "データ(4バイト): 05 00 04 00"
                , HH.br_
                , HH.text "05 Bルート&HAN"
                , HH.br_
                , HH.text "00 HAN Sleep無効"
                , HH.br_
                , HH.text "04 任意チャネル(スキャン後に再設定)"
                , HH.br_
                , HH.text "00 送信電力20mW"
                ]
            ]
        , HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetJ11CommandFormatButton $ C.commandInitialSetup $ UInt.fromInt 0x04
            ]
            [ HH.text "初期設定要求(仮のチャネルを設定)コマンド" ]
        ]
    --
    , HH.h3_ [ HH.text "3. Bルート PANA認証情報設定" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 54" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 30" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 03 bd" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 算出" ]
            , HH.li_
                [ HH.text "データ(42バイト)"
                , HH.br_
                , HH.text "Bルート認証ID(32バイト)"
                , HH.br_
                , HH.text "Bルートパスワード(12バイト)"
                ]
            ]
        , HH.div [ HP.classes [ HB.m3 ] ]
            [ HH.div_
                [ HH.label [ HP.classes [ HB.cardTitle ], HP.for "B-route-id" ] [ HH.text "Bルート認証ID" ]
                , HH.input
                    [ HP.classes [ HB.formControl ]
                    , HP.id "B-route-id"
                    , HP.type_ HP.InputText
                    , HP.placeholder "input id here..."
                    , HE.onValueInput (\ev -> OnValueInputRouteBIdInputArea ev)
                    , HP.value $ Maybe.fromMaybe "" state.maybeRouteBId
                    ]
                ]
            ]
        , HH.div [ HP.classes [ HB.m3 ] ]
            [ HH.div_
                [ HH.label [ HP.classes [ HB.cardTitle ], HP.for "B-route-pass" ] [ HH.text "Bルートパスワード" ]
                , HH.input
                    [ HP.classes [ HB.formControl ]
                    , HP.id "B-route-pass"
                    , HP.type_ HP.InputText
                    , HP.placeholder "input password here..."
                    , HE.onValueInput (\ev -> OnValueInputRouteBPasswordInputArea ev)
                    , HP.value $ Maybe.fromMaybe "" state.maybeRouteBPassword
                    ]
                ]
            ]
        , HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetPanaAuthCommandButton
            ]
            [ HH.text "PANA認証情報設定コマンド" ]
        , HH.div [ HP.classes [ HB.my2, HB.p3, HB.alertDanger, HB.fs5 ] ]
            [ HH.span [ HP.classes [ HB.me3, HB.fs3 ] ] [ iconExclamationDiamondFill ]
            , HH.text "ログにＩＤとパスワードが出力されているので、取り扱いには注意してください！"
            ]
        ]
    --
    , HH.h3_ [ HH.text "4. アクティブスキャン" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 51" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 12" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 03 9c" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 算出" ]
            , HH.li_
                [ HH.text "データ(14バイト)"
                , HH.br_
                , HH.text "スキャン時間(1バイト)"
                , HH.select
                    [ HP.classes [ HB.formSelectSm ]
                    , HE.onValueChange OnValueChangeScanDurationOption
                    ]
                    ( let
                        option s =
                          HH.option [ HP.value s, HP.selected $ s == state.scanDuration ]
                            [ HH.text s ]
                      in
                        [ option "5"
                        , option "6"
                        , option "7"
                        , option "8"
                        ]
                    )
                , HH.br_
                , HH.text "スキャンチャネル=チャネル4,5,6(4バイト)"
                , HH.br_
                , HH.text "ID設定=Paring IDあり(1バイト)"
                , HH.br_
                , HH.text "Paring ID=Bルート認証IDの最後の8文字(8バイト)"
                ]
            ]
        ]
    , HH.p_
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickActivescanCommandButton
            ]
            [ HH.text "アクティブスキャン実行コマンド" ]
        ]
    --
    , HH.h3_ [ HH.text "4.1 アクティブスキャン通知から必要な情報を取り出す。" ]
    , HH.p_
        [ HH.text "スマートメーターの応答があると通知コマンド0x4051が来る。"
        , HH.text "うまくいっていると、以下に表示される"
        ]
    , HH.div [ HP.classes [ HB.alert, HB.alertPrimary ] ]
        [ HH.table_
            let
              c = [ HP.classes [ HB.p2 ] ]
            in
              [ HH.thead_ []
              , HH.tbody_
                  [ HH.tr_ [ HH.td c [ HH.text "Channel" ], HH.td c [ HH.text $ Maybe.fromMaybe "不明" maybeChannel ] ]
                  , HH.tr_ [ HH.td c [ HH.text "Pan ID" ], HH.td c [ HH.text $ Maybe.fromMaybe "不明" maybePanId ] ]
                  , HH.tr_ [ HH.td c [ HH.text "MACアドレス" ], HH.td c [ HH.text $ Maybe.fromMaybe "不明" maybeMacAddress ] ]
                  , HH.tr_
                      [ HH.td c [ HH.text "RSSI" ]
                      , HH.td c [ HH.text $ Maybe.fromMaybe "不明" maybeRssi ]
                      , HH.td c [ HH.text "dBm" ]
                      ]
                  ]
              ]
        ]
    --
    , HH.h5_ [ HH.text "MACアドレスからIPv6リンクローカルアドレスへ変換する" ]
    , HH.p_
        [ HH.text "MACアドレスの最初の1バイト下位2bit目を反転して"
        , HH.br_
        , HH.text "0xFE80000000000000XXXXXXXXXXXXXXXXのXXをMACアドレスに置き換える"
        ]
    , HH.div [ HP.classes [ HB.alert, HB.alertPrimary ] ]
        [ HH.table_
            let
              c = [ HP.classes [ HB.p2 ] ]
            in
              [ HH.thead_ []
              , HH.tbody_
                  [ HH.tr_
                      [ HH.td c [ HH.text "IPV6アドレス" ]
                      , HH.td c [ HH.text $ Maybe.maybe "不明" IPv6Address.unsafeToString $ state.maybeIpv6Address ]
                      ]
                  ]
              ]
        ]
    --
    , HH.h3_ [ HH.text "5. 初期設定(2回目)" ]
    , HH.p_ [ HH.text "アクティブスキャンに応答してきたスマートメータのチャネルを設定する" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 5F" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 08" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 00 00" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 算出" ]
            , HH.li_
                [ HH.text
                    $ "データ(4バイト): 05 00 "
                    <> (Maybe.fromMaybe "不明" maybeChannel)
                    <> " 00"
                , HH.br_
                , HH.text "05 Bルート&HAN"
                , HH.br_
                , HH.text "00 HAN Sleep無効"
                , HH.br_
                , HH.text $ Maybe.fromMaybe "不明" maybeChannel <> " チャネル"
                , HH.br_
                , HH.text "00 送信電力20mW"
                ]
            ]
        , HH.button
            ( Array.concat
                [ [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ] ]
                , case (_.channel <$> state.maybeActivescanAppendix) of
                    Nothing -> [ HP.enabled false ]
                    Just channel -> [ HE.onClick \_ -> OnClickSetJ11CommandFormatButton $ C.commandInitialSetup channel ]
                ]
            )
            [ HH.text "初期設定要求(スマートメータのチャネルを設定)コマンド" ]
        ]
    --
    , HH.h3_ [ HH.text "6. Bルート動作開始" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 53" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 04" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 03 90" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 00 20" ]
            ]
        , HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetJ11CommandFormatButton C.commandBRouteStart
            ]
            [ HH.text "Bルート動作開始要求コマンド" ]
        ]
    --
    , HH.h3_ [ HH.text "7. UDPポートオープン" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 05" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 06" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 03 44" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 00 28" ]
            , HH.li_
                [ HH.text "データ(2バイト): 0E 1A"
                , HH.br_
                , HH.text "0E 1A = UDPポート番号3610"
                ]
            ]
        , HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetJ11CommandFormatButton C.commandUdpPortOpen
            ]
            [ HH.text "UDPポートオープン要求コマンド" ]
        ]
    --
    , HH.h3_ [ HH.text "8. BルートPANA開始" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 56" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 04" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 03 93" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 00 00" ]
            ]
        , HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetJ11CommandFormatButton C.commandBRouteStartPana
            ]
            [ HH.text "BルートPANA開始要求コマンド" ]
        ]
    --
    , HH.p_ [ HH.text "PANA認証成功通知が来ると接続完了。" ]
    --
    , HH.p [ HP.classes [ HB.m5, HB.p5 ] ]
        [ HH.strong_
            [ HH.text "ここまででスマートメーターとのPANAセッションが確立する。" ]
        ]
    --
    , HH.h2_ [ HH.text "接続完了時に送られてきたデータ受信通知を読んでみる" ]
    , HH.pre_
        [ HH.text "2025-03-08T13:46:00.513Z 受信 <- D0 F9 EE 5D 60 18 00 31 03 BD 0E 2E FE 80 00 00 00 00 00 00 8E DF 9D FF FE C1 0F 01 0E 1A 0E 1A D8 84 01 02 C4 00 12 10 81 90 5B 0E F0 01 0E F0 01 73 01 D5 04 01 02 88 01" ]
    , HH.p_
        [ HH.text "IPv6アドレスは配送用の情報, Echonet Liteのポートは 3610=0x0E1A 固定値"
        , HH.text "なので読んで意味がある情報はデータ部分。"
        , HH.br_
        , HH.text "それを取り出すと"
        , HH.br_
        , HH.text "10 81 90 5B 0E F0 01 0E F0 01 73 01 D5 04 01 02 88 01"
        , HH.br_
        , HH.text "Echonet Lite電文構成(フレームフォーマット)によると"
        , HH.ul_
            [ HH.li_ [ HH.text "EHD1 (1byte)" ]
            , HH.li_ [ HH.text "EHD2 (1byte)" ]
            , HH.li_ [ HH.text "TID (2bytes)" ]
            , HH.li_
                [ HH.text "ここからEDATA"
                , HH.p_
                    [ HH.text "SEOJ: 送信元Echonet Liteオブジェクト指定 (3bytes)"
                    , HH.br_
                    , HH.text "DEOJ: 送信先Echonet Liteオブジェクト指定 (3bytes)"
                    , HH.br_
                    , HH.text "ESV: Echonet Liteサービス (1byte)"
                    , HH.br_
                    , HH.text "OPC: 処理プロパティ数 (1byte)"
                    , HH.br_
                    , HH.text "EPC: Echonet Liteプロパティ (1byte)"
                    , HH.br_
                    , HH.text "PDC: EDTのバイト数 (1byte)"
                    , HH.br_
                    , HH.text "EDT: プロパティ値データ (PDCで指定bytes)"
                    ]
                ]
            ]
        , HH.br_
        , HH.text "なので、これに当てはめてみると"
        , HH.ul_
            [ HH.li_ [ HH.text "0x10 (EHD1)" ]
            , HH.li_ [ HH.text "0x81 (EHD2)" ]
            , HH.li_ [ HH.text "0x90 0x5B (TID)" ]
            , HH.li_
                [ HH.text "ここからEDATA"
                , HH.ul_
                    [ HH.li_ [ HH.text "0x0E 0xF0 0x01 (SEOJ) -> ノードプロファイルクラスの一般ノードインスタンス" ]
                    , HH.li_ [ HH.text "0x0E 0xF0 0x01 (DEOJ) -> ノードプロファイルクラスの一般ノードインスタンス" ]
                    , HH.li_ [ HH.text "0x73 (ESV) -> プロパティ値通知" ]
                    , HH.li_ [ HH.text "0x01 (OPC)" ]
                    , HH.li_ [ HH.text "0xD5 (EPC) -> インスタンスリスト通知" ]
                    , HH.li_ [ HH.text "0x04 (PDC)" ]
                    , HH.li_ [ HH.text "0x01 0x02 0x88 0x01 (EDT) -> 1つの028801(低圧スマート電力量メータクラスのインスタンス)が存在する" ]
                    ]
                ]
            , HH.text "ということで、これはEchonet Liteインスタンスリスト通知"
            , HH.br_
            ]
        ]
    --
    , HH.h2_ [ HH.text "スマートメーターと通信する" ]
    , HH.p_
        [ HH.text "下のフォームでスマートメーターに送信すると、返答がイベントでやってきます。"
        , HH.br_
        , HH.text "スマートメーターとのセッションが確立していると、とくに何もしなくても毎時0分,30分に定時積算電力量イベントがやってきます。"
        ]
    , HH.p_ [ HH.text "いろいろ試してみてください。" ]
    --
    , HH.h2_ [ HH.text "データ送信" ]
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 08" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 算出" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 算出" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 算出" ]
            , HH.li_
                [ HH.text "送信先IPv6アドレス(16バイト)"
                , HH.br_
                , HH.text "送信元ポート番号(2バイト): 0E 1A"
                , HH.br_
                , HH.text "送信先ポート番号(2バイト): 0E 1A"
                , HH.br_
                , HH.text "送信データサイズ(2バイト): 任意"
                , HH.br_
                , HH.text "送信データ(バイト): 任意"
                ]
            ]
        ]
    --
    , HH.h3_ [ HH.text "送信するECHONET Lite 電文を組み立てる" ]
    , HH.p_
        [ HH.pre [ HP.classes [ HB.p2, HB.bgDark, HB.textWhite ] ]
            [ HH.text $ CT.toStringJ11Command state.commandTransmitData ]
        , HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
            [ HH.button
                [ HP.classes [ HB.m1, HB.col3, HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
                , HP.type_ HP.ButtonButton
                , HE.onClick \_ -> OnClickCopyToClipboard $ CT.toStringJ11Command state.commandTransmitData
                ]
                [ HH.text "copy to clipboard" ]
            , HH.button
                [ HP.classes [ HB.m1, HB.col3, HB.btn, HB.btnSm, HB.btnPrimary ]
                , HP.type_ HP.ButtonButton
                , HE.onClick \_ -> OnClickSetJ11CommandButton state.commandTransmitData
                ]
                [ HH.text "コマンドラインへ送る" ]
            ]
        , HH.slot _echonetframeedit unit EchonetFrameEdit.component state.maybeIpv6Address HandleEchonetFrameEditUpdate
        ]
    --
    , HH.h2_ [ HH.text "おわり" ]
    --
    , HH.p_
        [ HH.ul_
            [ HH.li_ [ HH.text "ユニークコード(4バイト): D0 EA 83 FC" ]
            , HH.li_ [ HH.text "コマンドコード(2バイト): 00 57" ]
            , HH.li_ [ HH.text "メッセージ長(2バイト): 00 04" ]
            , HH.li_ [ HH.text "ヘッダ部チェックサム(2バイト): 03 94" ]
            , HH.li_ [ HH.text "データ部チェックサム(2バイト): 00 00" ]
            ]
        , HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetJ11CommandFormatButton C.commandBRouteTerminatePana
            ]
            [ HH.text "BルートPANA終了要求コマンド" ]
        , HH.br_
        , HH.text "BルートPANA終了要求コマンドを送信することでスマートメーターとのセッションを切ります。"
        ]
    --
    -- ここからコマンドライン
    --
    , HH.div
        [ HP.id "accordionExample"
        , HP.class_ HB.fixedBottom
        ]
        [ HH.div
            [ HP.classes [ HB.accordion, HB.fixedBottom ]
            ]
            [ HH.div [ HP.class_ HB.accordionItem ]
                [ HH.div [ HP.class_ HB.accordionHeader ]
                    [ HH.button
                        [ HP.type_ HP.ButtonButton
                        , HP.class_ HB.accordionButton
                        , HP.attr (AttrName "data-bs-toggle") "collapse"
                        , HP.attr (AttrName "data-bs-target") "#collapseOne"
                        , HP.attr (AttrName "aria-expanded") "true"
                        , HP.attr (AttrName "aria-controls") "collapseOne"
                        ]
                        [ HH.text "コマンドライン"
                        ]
                    ]
                , HH.div
                    [ HP.id "collapseOne"
                    , HP.classes
                        [ HB.accordionCollapse
                        , HB.collapse
                        , HB.show
                        ]
                    , HP.attr (AttrName "aria-labelledby") "headingOne"
                    , HP.attr (AttrName "data-bs-parent") "#accordionExample"
                    ]
                    [ HH.div [ HP.class_ HB.accordionBody ]
                        [ HH.slot _hexedit unit HexEdit.component unit HandleHexEditUpdate
                        ]
                    ]
                ]
            ]
        ]
    , footer
    ]
  where
  maybeBeacon :: Maybe R.Beacon
  maybeBeacon = _.beacon =<< state.maybeActivescanAppendix

  maybeChannel = Int.toStringAs Int.decimal <<< UInt.toInt <<< _.channel <$> state.maybeActivescanAppendix

  maybePanId = Utility.toStringHexAs Utility.word <<< _.panId <$> maybeBeacon

  maybeRssi = Int.toStringAs Int.decimal <<< _.rssi <$> maybeBeacon

  maybeMacAddress = String.joinWith ":" <<< map (Utility.toStringHexAs Utility.octet) <<< _.macAddress <$> maybeBeacon

  footer = HH.div [ HP.style "margin-bottom: 25vh;" ] []

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Initialize -> do
    navigator <- H.liftEffect $ Window.navigator =<< window
    clipboard <- H.liftEffect $ Clipboard.clipboard navigator
    maybeSerialPortOpened <- H.request _hexedit unit HexEdit.IsOpenedSerialPort
    H.modify_ _ { maybeClipboard = clipboard, serialportOpened = Maybe.fromMaybe false maybeSerialPortOpened }
  Finalize -> do
    H.tell _hexedit unit HexEdit.CloseSerialPort
  OnValueChangeBoudRateOption strBoudrate -> case Int.fromString strBoudrate of
    Nothing -> H.liftEffect $ logShow ("set option error: " <> strBoudrate)
    Just x -> do
      H.modify_ _ { boudRate = x }
  OnValueChangeScanDurationOption str -> do
    H.modify_ _ { scanDuration = str }
  OnClickOpenPortButton -> do
    boudrate <- H.gets _.boudRate
    H.tell _hexedit unit (HexEdit.OpenSerialPort boudrate)
  OnClickClosePortButton -> H.tell _hexedit unit HexEdit.CloseSerialPort
  OnValueInputRouteBIdInputArea inputText ->
    H.modify_
      _ { maybeRouteBId = if String.null inputText then Nothing else Just inputText }
  OnValueInputRouteBPasswordInputArea inputText ->
    H.modify_
      _ { maybeRouteBPassword = if String.null inputText then Nothing else Just inputText }
  OnClickSetJ11CommandFormatButton j11commandformat -> do
    j11command <- H.liftEffect $ CT.fromJ11CommandFormat j11commandformat
    H.tell _hexedit unit (HexEdit.SetJ11Command j11command)
  OnClickSetPanaAuthCommandButton -> do
    maybeRouteBId <- H.gets _.maybeRouteBId
    maybeRouteBPassword <- H.gets _.maybeRouteBPassword
    case (maybeRouteBId /\ maybeRouteBPassword) of
      (Just routeBId /\ Just routeBPassword) -> do
        j11commandformat <- H.liftEffect $ C.commandSetPanaAuthInfo routeBId routeBPassword
        j11command <- H.liftEffect $ CT.fromJ11CommandFormat j11commandformat
        H.tell _hexedit unit (HexEdit.SetJ11Command j11command)
      _ -> do
        now <- H.liftEffect $ Now.now
        let
          newToast = ToastMessageError { at: now, message: "IDまたはパスワードが入力されていません" }
        H.raise $ NewToastMessages [ newToast ]
  OnClickActivescanCommandButton ->
    H.gets _.maybeRouteBId
      >>= case _ of
          Just routeBId -> do
            scanDuration <- H.gets _.scanDuration
            let
              duration = unsafePartial $ Maybe.fromJust $ UInt.fromString scanDuration
            j11commandformat <- H.liftEffect $ C.commandActivescan duration routeBId
            j11command <- H.liftEffect $ CT.fromJ11CommandFormat j11commandformat
            H.tell _hexedit unit (HexEdit.SetJ11Command j11command)
          Nothing -> mempty
  HandleHexEditUpdate msg -> handleHexEditOutput msg
  HandleEchonetFrameEditUpdate (EchonetFrameEdit.SubmitToEchonetLiteMessage msg) -> H.tell _hexedit unit (HexEdit.SendCommand $ J11Command msg)
  HandleEchonetFrameEditUpdate (EchonetFrameEdit.UpdateEchonetFrame newFrame) -> do
    maybeIpv6Address <- H.gets _.maybeIpv6Address
    let
      default = IPv6Address.unsafeFromString "0000:0000:0000:0000:0000:0000:0000:0000"

      ipv6 = Maybe.fromMaybe default maybeIpv6Address
    j11command <- H.liftEffect $ CT.fromJ11CommandFormat <=< C.commandTransmitData ipv6 $ DV.whole newFrame
    H.modify_ _ { commandTransmitData = j11command }
  OnClickCopyToClipboard s -> do
    maybeClipboard <- H.gets _.maybeClipboard
    case maybeClipboard of
      Nothing -> mempty
      Just clipboard -> void $ H.liftEffect $ Clipboard.writeText s clipboard
  OnClickSetJ11CommandButton j11Command -> H.tell _hexedit unit (HexEdit.SetJ11Command j11Command)

handleHexEditOutput :: forall m. MonadAff m => HexEdit.Output -> H.HalogenM State Action ChildSlots Output m Unit
handleHexEditOutput = case _ of
  HexEdit.Message arrival -> do
    let
      newToast = ToastMessage arrival
    H.raise $ NewToastMessages (Array.singleton newToast)
  HexEdit.MessageError appendix -> do
    let
      newToast = ToastMessageError { at: appendix.at, message: Exc.message appendix.error }
    H.raise $ NewToastMessages (Array.singleton newToast)
  HexEdit.ArrivalResponse appendix -> do
    messages <- either3 selectResponseError selectCommandResponse selectNotify appendix.response
    H.raise $ NewToastMessages messages
  HexEdit.SerialportOpened -> H.modify_ _ { serialportOpened = true }
  HexEdit.SerialportClosed -> H.modify_ _ { serialportOpened = false }
  where
  --
  selectResponseError :: R.ResponseError -> H.HalogenM State Action ChildSlots Output m (Array ToastMessage)
  selectResponseError a = do
    now <- H.liftEffect $ Now.now
    pure [ ToastMessageError { at: now, message: show a } ]

  --
  selectCommandResponse :: R.CommandResponse -> H.HalogenM State Action ChildSlots Output m (Array ToastMessage)
  -- Bルート動作開始
  selectCommandResponse a@(R.CommandResponseStartBRoute response) = do
    H.modify_ _ { maybeStartBRouteAppendix = Just response }
    now <- H.liftEffect $ Now.now
    pure [ ToastMessageCommandResponse { at: now, message: R.toStringCommandResponse a } ]

  -- コマンド結果
  selectCommandResponse a = do
    now <- H.liftEffect $ Now.now
    pure [ ToastMessageCommandResponse { at: now, message: R.toStringCommandResponse a } ]

  --
  selectNotify :: R.Notify -> H.HalogenM State Action ChildSlots Output m (Array ToastMessage)
  -- 0x4051 アクティブスキャン結果通知
  selectNotify a@(R.NotifyActivescan response) = do
    let
      maybeIpv6Address = (fromMacAddressToIpv6LocalAddress <<< _.macAddress) =<< response.beacon
    H.modify_ _ { maybeIpv6Address = maybeIpv6Address, maybeActivescanAppendix = Just response }
    now <- H.liftEffect $ Now.now
    pure [ ToastMessageNotify { at: now, message: R.toStringNotify a } ]

  -- 0x6018 データ受信通知
  selectNotify a@(R.NotifyReceivedData response) = do
    let
      dataview = DV.whole <<< _.data $ unwrap response
    now <- H.liftEffect $ Now.now
    parsed <- H.liftEffect $ EchonetLite.parseEchonetLiteFrame dataview
    messages <- case parsed of
      Left _ -> pure [ ToastMessageError { at: now, message: show response } ]
      Right frame -> do
        let
          toStringProperty :: EchonetLite.Property -> Maybe String
          toStringProperty p = EchonetLite.toStringWhmProperty <$> EchonetLite.makeSmartWhmProperty p

          messages :: Array String
          messages = Array.mapMaybe toStringProperty frame.props
        pure $ map (\x -> ToastMessageEvent { at: now, message: x }) messages
    pure
      $ case messages of
          [] -> [ ToastMessageNotify { at: now, message: R.toStringNotify a } ]
          xs -> xs

  -- 0x6019 起動完了通知
  selectNotify a@R.NotifyBootCompleted = do
    -- 初期化されたので総て初期値にする
    H.modify_
      _
        { maybeActivescanAppendix = Nothing
        , maybeStartBRouteAppendix = Nothing
        }
    now <- H.liftEffect $ Now.now
    pure [ ToastMessageNotify { at: now, message: R.toStringNotify a } ]

  -- 0x6028 PANA認証結果通知
  selectNotify a@(R.NotifyPanaCert _) = do
    now <- H.liftEffect $ Now.now
    pure [ ToastMessageNotify { at: now, message: R.toStringNotify a } ]

  -- 通知
  selectNotify a = do
    now <- H.liftEffect $ Now.now
    pure [ ToastMessageNotify { at: now, message: R.toStringNotify a } ]

--
fromMacAddressToIpv6LocalAddress :: Array UInt -> (Maybe IPv6Address)
fromMacAddressToIpv6LocalAddress bytearray = do
  guard $ Array.length bytearray == 8
  wordsize <- toWordSize =<< bitFlipFirstOctet bytearray
  let
    strArray = map (Utility.toStringHexAs Utility.word) (fixed <> wordsize)
  pure $ IPv6Address.unsafeFromString $ String.joinWith ":" strArray
  where
  -- 最初の64ビットは FE80 0000 0000 0000 固定値
  fixed = [ UInt.fromInt 0xFE80, UInt.fromInt 0x0000, UInt.fromInt 0x00000, UInt.fromInt 0x0000 ]

  -- MACアドレスの最初の1バイト下位2bit目は反転する
  bitFlipFirstOctet :: Array UInt -> Maybe (Array UInt)
  bitFlipFirstOctet = Array.modifyAt 0 (\x -> UInt.xor x (UInt.fromInt 0x02))

  -- 16ビット毎にする
  toWordSize :: Array UInt -> Maybe (Array UInt)
  toWordSize = Utility.unwrapAllMaybes <<< unfoldr1 combine
    where
    calcWordNum :: UInt -> UInt -> UInt
    calcWordNum left right = left * (UInt.fromInt 256) + right

    combine :: Array UInt -> Tuple (Maybe UInt) (Maybe (Array UInt))
    combine xs = case Array.splitAt 2 xs of
      { before: [ a, b ], after: [] } -> Tuple (Just $ calcWordNum a b) Nothing
      { before: [ a, b ], after: after } -> Tuple (Just $ calcWordNum a b) $ Just after
      _ -> Tuple Nothing Nothing -- 奇数の時
