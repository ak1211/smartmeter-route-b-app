{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module Page.SkStack
  ( Action(..)
  , Output(..)
  , component
  ) where

import Prelude
import Data.Array as Array
import Data.ArrayBuffer.Builder (putUint8)
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.Cast as Cast
import Data.ArrayBuffer.DataView as DataView
import Data.ArrayBuffer.Typed as ArrayBufferTyped
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Char as Char
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Number.Format (toString)
import Data.String as String
import Data.String.Unsafe as StringUnsafe
import Data.UInt (UInt)
import Data.UInt as UInt
import EchonetFrameEdit as EchonetFrameEdit
import EchonetLite as EchonetLite
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Effect.Exception as Exc
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Page.ToastMessage (ToastMessage(..))
import ProtocolStack.SkStackUart (Erxudp(..))
import ProtocolStack.SkStackUart as SkStackUart
import Terminal as Terminal
import Type.Proxy (Proxy(..))
import URI.Host.IPv6Address (IPv6Address)
import URI.Host.IPv6Address as IPv6Address
import Web.Clipboard (Clipboard)
import Web.Clipboard as Clipboard
import Web.HTML (window)
import Web.HTML.Window as Window

data Action
  = Initialize
  | Finalize
  | OnValueChangeBoudRateOption String
  | OnClickOpenPortButton
  | OnClickClosePortButton
  | OnClickSetCommandLineButton String
  | OnClickCopyToClipboard String
  | OnClickSubmit String
  | HandleTerminalUpdate Terminal.Output
  | HandleEchonetFrameEditUpdate EchonetFrameEdit.Output

data Output
  = NewToastMessages (Array ToastMessage)

type State
  = { maybeClipboard :: Maybe Clipboard
    , serialportOpened :: Boolean
    , boudRate :: Int
    , maybeEpandesc :: Maybe SkStackUart.Epandesc
    , maybeIpv6Address :: Maybe IPv6Address
    , sksendtoCommand :: String
    }

--
type ChildSlots
  = ( terminal :: H.Slot Terminal.Query Terminal.Output Unit
    , echonetframeedit :: H.Slot EchonetFrameEdit.Query EchonetFrameEdit.Output Unit
    )

_terminal = Proxy :: Proxy "terminal"

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
  , maybeEpandesc: Nothing
  , maybeIpv6Address: Nothing
  , sksendtoCommand: ""
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.p [ HP.class_ HB.m1 ]
        [ HH.text "SKSTACKプロトコルスタックのモジュール"
        , HH.ul_
            [ HH.li_ [ HH.text "USB-シリアル変換器につないだ BP35A1モジュール" ]
            , HH.li_ [ HH.text "USBドングル WSR35A1-00" ]
            , HH.li_ [ HH.text "USBドングル RL7023 Stick-D/IPS" ]
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
    , HH.h2_ [ HH.text "BP35A1と会話してみる" ]
    , HH.p_ [ HH.text "ターミナルにPort openedが表示されているのを確認したら、疎通確認のためにとりあえずSKVERと入力して送信する。" ]
    , HH.button
        [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
        , HE.onClick \_ -> OnClickSetCommandLineButton "SKVER"
        ]
        [ HH.text "SKVER" ]
    , HH.p_
        [ HH.text "SKVERと入力するか、またはこのボタンを押すとSKVERが入力されるので、送信を押すことでBP35A1またはRL7023にSKVERを送信する"
        , HH.br_
        , HH.text "この後"
        , HH.br_
        , HH.text "EVER 1.2.8"
        , HH.br_
        , HH.text "OK"
        , HH.br_
        , HH.text "などの応答があればBP35A1との通信ができている。"
        , HH.text "そうでなければ接続を見直してください。"
        ]
    , HH.h5_ [ HH.text "表示形式を見てみる" ]
    , HH.button
        [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
        , HE.onClick \_ -> OnClickSetCommandLineButton "ROPT"
        ]
        [ HH.text "ROPT" ]
    , HH.p_
        [ HH.text "ROPTはデータ部の表示形式を読みだす。"
        , HH.text "OK 00またはOK 01"
        , HH.br_
        , HH.text "この意味は"
        , HH.br_
        , HH.text "0: バイナリ表示"
        , HH.br_
        , HH.text "1: 16進ASCII表示"
        , HH.br_
        , HH.text "RL7023 Stick-D/IPSでは16進ASCIIのみサポートなのでER04になる。"
        , HH.br_
        , HH.text "このプログラムは16進ASCII表示であることが前提です。"
        , HH.br_
        , HH.text "バイナリ表示ならWOPT 01を送信して16進ASCII表示に切り替えてください"
        , HH.br_
        ]
    , HH.h5_ [ HH.text "他のレジスタの内容も見てみる" ]
    , HH.button
        [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
        , HE.onClick \_ -> OnClickSetCommandLineButton "SKSREG SFE"
        ]
        [ HH.text "SKSREG SFE" ]
    , HH.p_
        [ HH.text "SKSREG SFEはエコーバックフラグを読みだす。"
        , HH.br_
        , HH.text "ESREG 1"
        , HH.br_
        , HH.text "OK"
        , HH.br_
        , HH.text "この意味は"
        , HH.br_
        , HH.text "0: コマンド入力のエコーバックをしない"
        , HH.br_
        , HH.text "1: エコーバックあり"
        , HH.br_
        , HH.text "です。"
        ]
    --
    , HH.h2_ [ HH.text "スマートメーターと接続する" ]
    , HH.p_ [ HH.text "送配電事業者からもらった認証IDとパスワードを用意する。" ]
    --
    , HH.h5_ [ HH.text "パスワードを登録する。" ]
    , HH.p_
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetCommandLineButton "SKSETPWD C "
            ]
            [ HH.text "SKSETPWD C " ]
        , HH.br_
        , HH.text "SKSETPWD C につづけてパスワードを入力して送信する。"
        ]
    --
    , HH.h5_ [ HH.text "認証IDを登録する。" ]
    , HH.p_
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetCommandLineButton "SKSETRBID "
            ]
            [ HH.text "SKSETRBID " ]
        , HH.br_
        , HH.text "SKSETRBID につづけて認証IDを入力して送信する。"
        ]
    --
    , HH.h2_ [ HH.text "アクティブスキャンを実行する。" ]
    , HH.p_
        [ HH.text "SKSCAN 2 を送信した後はEVENT 0x22 (アクティブスキャンの完了)の応答を待つこと。"
        , HH.br_
        , HH.text "EPANDESCのイベントがこなければ続けてSKSCAN 2を送信する(durationを大きくして)"
        , HH.br_
        , HH.text "EVENT 0x20 (Beaconを受信した)の後にEPANDESCがやってくるまでdurationを大きくしながら繰り返す。"
        , HH.br_
        , HH.text "ボタンをおいておくので、試してください。"
        ]
    --
    , HH.h5_ [ HH.text "duration: 4 でアクティブスキャンを実行する。" ]
    , HH.p_
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetCommandLineButton "SKSCAN 2 FFFFFFFF 4"
            ]
            [ HH.text "SKSCAN 2 FFFFFFFF 4" ]
        ]
    --
    , HH.h5_ [ HH.text "duration: 5 でアクティブスキャンを実行する。" ]
    , HH.p_
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetCommandLineButton "SKSCAN 2 FFFFFFFF 5"
            ]
            [ HH.text "SKSCAN 2 FFFFFFFF 5" ]
        ]
    --
    , HH.h5_ [ HH.text "duration: 6 でアクティブスキャンを実行する。" ]
    , HH.p_
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetCommandLineButton "SKSCAN 2 FFFFFFFF 6"
            ]
            [ HH.text "SKSCAN 2 FFFFFFFF 6" ]
        ]
    --
    , HH.h5_ [ HH.text "duration: 7 でアクティブスキャンを実行する。" ]
    , HH.p_
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetCommandLineButton "SKSCAN 2 FFFFFFFF 7"
            ]
            [ HH.text "SKSCAN 2 FFFFFFFF 7" ]
        ]
    --
    , HH.h5_ [ HH.text "duration: 8 でアクティブスキャンを実行する。" ]
    , HH.p_
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetCommandLineButton "SKSCAN 2 FFFFFFFF 8"
            ]
            [ HH.text "SKSCAN 2 FFFFFFFF 8" ]
        ]
    --
    , HH.h2_ [ HH.text "EPANDESCイベントから必要な情報を取り出す。" ]
    , HH.p_
        [ HH.text "こんな応答が来るはず。(EPANDESCイベント。一部隠しています)"
        , HH.br_
        , HH.pre_
            [ HH.text "EVENT 20 0000:0000:0000:0000:0000:0000:0000:0000\r\n"
            , HH.text "EPANDESC\r\n"
            , HH.text "  Channel:3B\r\n"
            , HH.text "  Channel Page:09\r\n"
            , HH.text "  Pan ID:0000\r\n"
            , HH.text "  Addr:0000000000000000\r\n"
            , HH.text "  LQI:35\r\n"
            , HH.text "  PairID:00000000\r\n"
            ]
        ]
    , HH.p_ [ HH.text "うまくいっていると、以下に表示される" ]
    , HH.div [ HP.classes [ HB.alert, HB.alertPrimary ] ]
        [ HH.table_
            let
              c = [ HP.classes [ HB.p2 ] ]
            in
              [ HH.thead_ []
              , HH.tbody_
                  [ HH.tr_ [ HH.td c [ HH.text "Channel" ], HH.td c [ HH.text $ fromMaybe "不明" channel ] ]
                  , HH.tr_ [ HH.td c [ HH.text "Channel Page" ], HH.td c [ HH.text $ fromMaybe "不明" channelPage ] ]
                  , HH.tr_ [ HH.td c [ HH.text "Pan ID" ], HH.td c [ HH.text $ fromMaybe "不明" panId ] ]
                  , HH.tr_ [ HH.td c [ HH.text "Addr" ], HH.td c [ HH.text $ fromMaybe "不明" (show <$> addr) ] ]
                  , HH.tr_ [ HH.td c [ HH.text "LQI(10進値)" ], HH.td c [ HH.text $ maybe "不明" UInt.toString lqi ] ]
                  , HH.tr_
                      [ HH.td c [ HH.text "LQI(RSSI)" ]
                      , HH.td c [ HH.text $ maybe "不明" (toString <<< unwrap) rssi ]
                      , HH.td c [ HH.text "dBm" ]
                      ]
                  , HH.tr_ [ HH.td c [ HH.text "PairID:" ], HH.td c [ HH.text $ fromMaybe "不明" pairId ] ]
                  ]
              ]
        ]
    --
    , HH.h5_ [ HH.text "MACアドレスからIPv6リンクローカルアドレスへ変換する" ]
    , HH.p_
        let
          text = "SKLL64 " <> maybe "" unwrap addr
        in
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
              , HE.onClick \_ -> OnClickSetCommandLineButton text
              ]
              [ HH.text text ]
          , HH.br_
          , HH.text "SKLL64 につづけてEPANDESCで指定されているAddrを入力して送信する。"
          , HH.br_
          , HH.text "FE80:0000:0000:0000:0000:0000:0000:0000"
          , HH.br_
          , HH.text "こんなかんじの応答がある。これがIPv6リンクローカルアドレス。"
          ]
    , HH.p_ [ HH.text "うまくいっていると、以下に表示される" ]
    , HH.div [ HP.classes [ HB.alert, HB.alertPrimary ] ]
        [ HH.table_
            let
              c = [ HP.classes [ HB.p2 ] ]
            in
              [ HH.thead_ []
              , HH.tbody_
                  [ HH.tr_
                      [ HH.td c [ HH.text "IPV6アドレス" ]
                      , HH.td c [ HH.text $ maybe "不明" IPv6Address.unsafeToString $ state.maybeIpv6Address ]
                      ]
                  ]
              ]
        ]
    --
    , HH.h5_ [ HH.text "自端末の論理チャンネル番号を設定する" ]
    , HH.p_
        let
          text = "SKSREG S2 " <> fromMaybe "" channel
        in
          [ HH.text "自端末が使用する周波数の論理チャンネル番号を設定する"
          , HH.br_
          , HH.button
              [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
              , HE.onClick \_ -> OnClickSetCommandLineButton text
              ]
              [ HH.text text ]
          , HH.br_
          , HH.text "SKSREG S2 につづけてEPANDESCで指定されているChannelを入力して送信する。"
          ]
    --
    , HH.h5_ [ HH.text "自端末のPAN IDを設定する" ]
    , HH.p_
        let
          text = "SKSREG S3 " <> fromMaybe "" panId
        in
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
              , HE.onClick \_ -> OnClickSetCommandLineButton text
              ]
              [ HH.text text ]
          , HH.br_
          , HH.text "SKSREG S3 につづけてEPANDESCで指定されているPan IDを入力して送信する。"
          ]
    --
    , HH.h5_ [ HH.text "PANA認証" ]
    , HH.p_
        let
          maybeStrIpv6 =
            state.maybeIpv6Address
              <#> IPv6Address.unsafeToString
              >>= (String.stripPrefix (String.Pattern "[") >=> String.stripSuffix (String.Pattern "]"))

          text = "SKJOIN " <> fromMaybe "" maybeStrIpv6
        in
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
              , HE.onClick \_ -> OnClickSetCommandLineButton text
              ]
              [ HH.text text ]
          , HH.br_
          , HH.text "SKJOIN につづけてIPv6リンクローカルアドレスを入力して送信する。"
          ]
    --
    , HH.p_
        [ HH.text "成功するとイベントがたくさん送られてくる。"
        , HH.br_
        , HH.text "その中にEVENT25があると接続完了。"
        ]
    --
    , HH.p [ HP.classes [ HB.m5, HB.p5 ] ]
        [ HH.strong_
            [ HH.text "EVENT 25受信時点でスマートメーターとのセッションが確立する。" ]
        ]
    --
    , HH.h2_ [ HH.text "ERXUDPイベントを読んでみる" ]
    , HH.p_
        [ HH.text "ERXUDPイベントの構造は"
        , HH.ul_
            [ HH.li_ [ HH.text "ERXUDP" ]
            , HH.li_ [ HH.text "送信元IPv6アドレス" ]
            , HH.li_ [ HH.text "送信先IPv6アドレス" ]
            , HH.li_ [ HH.text "送信元ポート番号" ]
            , HH.li_ [ HH.text "送信先ポート番号" ]
            , HH.li_ [ HH.text "送信元のMAC層アドレス" ]
            , HH.li_ [ HH.text "1" ]
            , HH.li_ [ HH.text "受信したデータの長さ(16進数)" ]
            , HH.li_ [ HH.text "受信データ" ]
            ]
        ]
    , HH.p_ [ HH.text "接続完了時に送られてきたERXUDPイベントを読んでみる" ]
    , HH.pre_
        [ HH.text "ERXUDP FE80:0000:0000:0000:0000:0000:0000:0000 FF02:0000:0000:0000:0000:0000:0000:0000 0E1A 0E1A 0000000000000000 1 0012 10812EDA0EF0010EF0017301D50401028801"
        ]
    , HH.p_
        [ HH.text "IPv6アドレスは配送用の情報, Echonet Liteのポートは 0E1A 固定値"
        , HH.text "なので読んで意味がある情報は最後のペイロード部分。"
        , HH.br_
        , HH.text "それを取り出すと"
        , HH.br_
        , HH.text "0012 10812EDA0EF0010EF0017301D50401028801"
        , HH.br_
        , HH.text "0012 (16進数) = 18バイト, 1バイトは16進数2文字だから2文字*18=データは16進数で36文字"
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
            , HH.li_ [ HH.text "0x2E 0xDA (TID)" ]
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
    , HH.h3_ [ HH.text "送信するECHONET Lite 電文を組み立てる" ]
    , HH.p_
        [ HH.pre [ HP.classes [ HB.p2, HB.bgDark, HB.textWhite ] ] [ HH.text state.sksendtoCommand ]
        , HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
            [ HH.button
                [ HP.classes [ HB.m1, HB.col3, HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
                , HP.type_ HP.ButtonButton
                , HE.onClick \_ -> OnClickCopyToClipboard state.sksendtoCommand
                ]
                [ HH.text "copy to clipboard" ]
            , HH.button
                [ HP.classes [ HB.m1, HB.col3, HB.btn, HB.btnSm, HB.btnPrimary ]
                , HP.type_ HP.ButtonButton
                , HE.onClick \_ -> OnClickSetCommandLineButton state.sksendtoCommand
                ]
                [ HH.text "コマンドラインへ送る" ]
            ]
        , HH.slot _echonetframeedit unit EchonetFrameEdit.component state.maybeIpv6Address HandleEchonetFrameEditUpdate
        ]
    --
    , HH.h2_ [ HH.text "おわり" ]
    , HH.p_
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
            , HE.onClick \_ -> OnClickSetCommandLineButton "SKTERM"
            ]
            [ HH.text "SKTERM" ]
        , HH.br_
        , HH.text "最後はSKTERMを送信することでスマートメーターとのセッションを切ります。"
        ]
    --
    -- ここからターミナル
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
                        [ HH.text "ターミナル"
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
                        [ HH.slot _terminal unit Terminal.component unit HandleTerminalUpdate
                        ]
                    ]
                ]
            ]
        ]
    , footer
    ]
  where
  channel = _.channel <<< unwrap <$> state.maybeEpandesc

  channelPage = _.channelPage <<< unwrap <$> state.maybeEpandesc

  panId = _.panId <<< unwrap <$> state.maybeEpandesc

  addr = _.addr <<< unwrap <$> state.maybeEpandesc

  lqi = _.lqi <<< unwrap <$> state.maybeEpandesc

  rssi = SkStackUart.makeRssifromLqi <$> lqi

  pairId = _.pairId <<< unwrap <$> state.maybeEpandesc

  footer = HH.div [ HP.style "margin-bottom: 25vh;" ] []

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Initialize -> do
    navigator <- H.liftEffect $ Window.navigator =<< window
    clipboard <- H.liftEffect $ Clipboard.clipboard navigator
    maybeSerialPortOpened <- H.request _terminal unit Terminal.IsOpenedSerialPort
    H.modify_ _ { maybeClipboard = clipboard, serialportOpened = Maybe.fromMaybe false maybeSerialPortOpened }
  Finalize -> do
    H.tell _terminal unit Terminal.CloseSerialPort
  OnValueChangeBoudRateOption strBoudrate -> case Int.fromString strBoudrate of
    Nothing -> H.liftEffect $ logShow ("set option error: " <> strBoudrate)
    Just x -> do
      H.modify_ _ { boudRate = x }
  OnClickOpenPortButton -> do
    boudrate <- H.gets _.boudRate
    H.tell _terminal unit (Terminal.OpenSerialPort boudrate)
  OnClickClosePortButton -> H.tell _terminal unit Terminal.CloseSerialPort
  OnClickSetCommandLineButton s -> H.tell _terminal unit (Terminal.SetCommandLineString s)
  OnClickCopyToClipboard s -> do
    maybeClipboard <- H.gets _.maybeClipboard
    case maybeClipboard of
      Nothing -> mempty
      Just clipboard -> void $ H.liftEffect $ Clipboard.writeText s clipboard
  OnClickSubmit strCommand -> do
    let
      codes =
        map (UInt.fromInt <<< Char.toCharCode <<< StringUnsafe.char <<< String.singleton)
          $ String.toCodePointArray strCommand
    H.tell _terminal unit (Terminal.SendCommand codes)
  HandleTerminalUpdate msg -> handleTerminalOutput msg
  HandleEchonetFrameEditUpdate (EchonetFrameEdit.SubmitToEchonetLiteMessage msg) -> H.tell _terminal unit (Terminal.SendCommand msg)
  HandleEchonetFrameEditUpdate (EchonetFrameEdit.UpdateEchonetFrame newFrame) -> do
    maybeIpv6Address <- H.gets _.maybeIpv6Address
    strCommandLine <- H.liftEffect $ makeCommandLine maybeIpv6Address newFrame
    H.modify_ _ { sksendtoCommand = displayMessage strCommandLine }
  where
  displayMessage :: { header :: Array UInt, payload :: Array UInt } -> String
  displayMessage commandline =
    let
      h =
        String.joinWith ""
          $ map (String.singleton <<< String.codePointFromChar)
          $ Array.mapMaybe (Char.fromCharCode <<< UInt.toInt) commandline.header

      pl =
        String.joinWith ""
          $ map enc
          $ Array.mapMaybe (Char.fromCharCode <<< UInt.toInt) commandline.payload
    in
      String.joinWith " " [ h, pl ]
    where
    enc :: Char -> String
    enc char =
      "\\x"
        <> ( String.toUpper
              $ EchonetLite.stringWithZeroPadding 2
              $ Int.toStringAs Int.hexadecimal
              $ Char.toCharCode char
          )

{-}

  whitespace :: UInt
  whitespace = UInt.fromInt 0x20

  commandline :: { header :: Array UInt, payload :: Array UInt }
  commandline = makeCommandLine state.maybeIpv6Address state.frame

  displayMessage :: String
  displayMessage =
    let
      h =
        String.joinWith ""
          $ map (String.singleton <<< String.codePointFromChar)
          $ Array.mapMaybe (Char.fromCharCode <<< UInt.toInt) commandline.header

      pl =
        String.joinWith ""
          $ map enc
          $ Array.mapMaybe (Char.fromCharCode <<< UInt.toInt) commandline.payload
    in
      String.joinWith " " [ h, pl ]
    where
    enc :: Char -> String
    enc char =
      "\\x"
        <> ( String.toUpper
              $ EchonetLite.stringWithZeroPadding 2
              $ Int.toStringAs Int.hexadecimal
              $ Char.toCharCode char
          )
          -}
handleTerminalOutput :: forall m. MonadAff m => Terminal.Output -> H.HalogenM State Action ChildSlots Output m Unit
handleTerminalOutput = case _ of
  Terminal.Message arrival -> do
    let
      newToast = ToastMessage arrival
    H.raise $ NewToastMessages (Array.singleton newToast)
  Terminal.MessageError { at: at, error: error } -> do
    let
      newToast = ToastMessageError { at: at, message: Exc.message error }
    H.raise $ NewToastMessages (Array.singleton newToast)
  Terminal.ArraivalResponse { at: at, response: response } -> do
    newToasts <- case response of
      SkStackUart.ResEVENT a ->
        pure
          [ ToastMessageEvent
              { at: at
              , message: (SkStackUart.toStringEvent a).message
              }
          ]
      SkStackUart.ResEPANDESC a -> do
        H.modify_ _ { maybeEpandesc = Just a }
        pure [ ToastMessageEvent { at: at, message: show a } ]
      SkStackUart.ResERXUDP (Erxudp erxudp) ->
        do
          echonetliteframe <-
            H.liftEffect
              $ Builder.execPut do
                  foldM (\_ x -> putUint8 x) mempty erxudp.payload
          EchonetLite.parseEchonetLiteFrame (DataView.whole echonetliteframe)
          >>= case _ of
              Left _ -> pure [ ToastMessageEvent { at: at, message: show (Erxudp erxudp) } ]
              Right parsed -> do
                let
                  toStringProperty :: EchonetLite.Property -> Maybe String
                  toStringProperty p = EchonetLite.toStringWhmProperty <$> EchonetLite.makeSmartWhmProperty p

                  messages :: Array String
                  messages = Array.mapMaybe toStringProperty parsed.props
                pure $ map (\x -> ToastMessageEvent { at: at, message: x }) messages
      SkStackUart.ResIpv6Address a -> do
        H.modify_ _ { maybeIpv6Address = Just a }
        pure [ ToastMessageEvent { at: at, message: IPv6Address.unsafeToString a } ]
      SkStackUart.ResFAIL er ->
        pure
          [ ToastMessageError { at: at, message: show er } ]
      SkStackUart.ResOK ->
        pure
          [ ToastMessageEvent { at: at, message: "OK" } ]
      SkStackUart.ResLocalEcho a ->
        pure
          [ ToastMessageLocalEcho { at: at, message: a } ]
      SkStackUart.ResMessage a ->
        pure
          [ ToastMessageEvent { at: at, message: a } ]
    H.raise $ NewToastMessages newToasts
  Terminal.SerialportOpened -> do
    H.liftEffect $ logShow "Terminal.SerialportOpened"
    H.modify_ _ { serialportOpened = true }
  Terminal.SerialportClosed -> do
    H.liftEffect $ logShow "Terminal.SerialportClosed"
    H.modify_ _ { serialportOpened = false }

makeCommandLine :: Maybe IPv6Address -> ArrayBuffer -> Effect { header :: Array UInt, payload :: Array UInt }
makeCommandLine maybeIpv6 frame = do
  let
    maybeStrIpv6 =
      maybeIpv6
        <#> IPv6Address.unsafeToString
        >>= (String.stripPrefix (String.Pattern "[") >=> String.stripSuffix (String.Pattern "]"))

    partialCommand =
      String.joinWith " "
        [ "SKSENDTO"
        , "1"
        , Maybe.fromMaybe "IPV6ADDRESS" maybeStrIpv6
        , "0E1A"
        , "1"
        , String.toUpper
            $ EchonetLite.stringWithZeroPadding 4
            $ Int.toStringAs Int.hexadecimal (DataView.byteLength $ DataView.whole frame)
        ]
  (payload :: Array UInt) <- ArrayBufferTyped.toArray =<< Cast.toUint8Array (DataView.whole frame)
  pure $ { header: toChars partialCommand, payload: _ } payload
  where
  toChars :: String -> Array UInt
  toChars str =
    map (UInt.fromInt <<< Char.toCharCode <<< StringUnsafe.char <<< String.singleton)
      $ String.toCodePointArray str
