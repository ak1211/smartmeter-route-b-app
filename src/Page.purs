{-
 https://github.com/ak1211/smartmeter-route-b-app
 copyright (c) 2023 akihiro yamamoto.
 licensed under the mit license.
 see license file in the project root for full license information.
-}
module Page
  ( component
  ) where

import Prelude
import Bp35a1 (ERXUDP(..))
import Bp35a1 as Bp35a1
import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.ArrayBuffer.Builder (putUint8)
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.DataView as DataView
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Number.Format (toString)
import Data.Time.Duration (Milliseconds(..), Minutes)
import Data.Tuple (Tuple(..))
import Data.UInt as UInt
import EchonetLite as EchonetLite
import EditForm as EditForm
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Effect.Exception as Exc
import Effect.Now as Now
import Effect.Unsafe (unsafePerformEffect)
import Halogen (SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.Themes.Bootstrap5 as HB
import Terminal as Terminal
import Type.Proxy (Proxy(..))
import WebSerialApi as WebSerialApi

--
foreign import bootstrapToastShow :: Effect Unit

-- Icons
iconCheckCircleFill ∷ forall w i. HH.HTML w i
iconCheckCircleFill = HH.i [ HP.classes [ ClassName "bi", ClassName "bi-check-circle-fill" ] ] []

iconExclamationTriangleFill ∷ forall w i. HH.HTML w i
iconExclamationTriangleFill = HH.i [ HP.classes [ ClassName "bi", ClassName "bi-exclamation-triangle-fill" ] ] []

iconExclamationCircleFill ∷ forall w i. HH.HTML w i
iconExclamationCircleFill = HH.i [ HP.classes [ ClassName "bi", ClassName "bi-exclamation-circle-fill" ] ] []

type ChildSlots
  = ( terminal :: H.Slot Terminal.Query Terminal.Output Unit
    , editform :: H.Slot EditForm.Query EditForm.Output Unit
    )

_terminal = Proxy :: Proxy "terminal"

_editform = Proxy :: Proxy "editform"

data ToastMessage
  = ToastMessage { at :: Instant, message :: String }
  | ToastMessageError { at :: Instant, message :: String }
  | ToastMessageEvent { at :: Instant, message :: String }
  | ToastMessageLocalEcho { at :: Instant, message :: String }

derive instance eqToastMessage :: Eq ToastMessage

type State
  = { maybeTimer :: Maybe SubscriptionId
    , instant :: Instant
    , availableWebSerialApi :: Boolean
    , serialportOpened :: Boolean
    , toastMessages :: Array (Tuple Boolean ToastMessage)
    , maybeEpandesc :: Maybe Bp35a1.EPANDESC
    , maybeIpV6Address :: Maybe Bp35a1.IpV6Address
    }

data Action
  = Initialize
  | Finalize
  | Tick
  | OnClickOpenPortButton
  | OnClickClosePortButton
  | OnClickSetCommandLineButton String
  | OnClickToastHideButton ToastMessage
  | HandleTerminalUpdate Terminal.Output
  | HandleEditFormUpdate EditForm.Output

component :: forall query input output m. MonadAff m => H.Component query input output m
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
  { maybeTimer: Nothing
  , instant: unsafePerformEffect $ Now.now
  , availableWebSerialApi: false
  , serialportOpened: false
  , toastMessages: []
  , maybeEpandesc: Nothing
  , maybeIpV6Address: Nothing
  }

navigationBar ∷ forall w i. HH.HTML w i
navigationBar =
  HH.nav [ HP.classes [ HB.navbar, HB.navbarExpandLg, HB.bgPrimary, HB.textWhite ] ]
    [ HH.div [ HP.class_ HB.containerFluid ]
        [ HH.div [ HP.classes [ HB.navbarBrand ] ] [ HH.text "スマートメーター ルート B テストアプリケーション" ] ]
    ]

toastDialog ∷ forall w. State -> Array (HH.HTML w Action)
toastDialog state = map go $ state.toastMessages
  where
  go (Tuple active toastmessage) = case toastmessage of
    ToastMessage a -> toastMessage a
    ToastMessageError a -> toastMessageError a
    ToastMessageEvent a -> toastMessageEvent a
    ToastMessageLocalEcho a -> toastMessageLocalEcho a
    where
    toastMessage = toast [ HB.textWhite, HB.bgDark ] [ iconCheckCircleFill, HH.text "メッセージ" ]

    toastMessageError = toast [ HB.textWarning, HB.bgDark ] [ iconExclamationCircleFill, HH.text "失敗しました" ]

    toastMessageEvent = toast [ HB.textDark, HB.bgInfo ] [ iconCheckCircleFill, HH.text "イベント" ]

    toastMessageLocalEcho = toast [ HB.textDark, HB.bgInfo ] [ iconCheckCircleFill, HH.text "エコーバック" ]

    closeButton =
      HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes [ HB.btnClose, HB.bgLight ]
        , HP.attr (AttrName "aria-label") "Close"
        , HE.onClick \_ -> OnClickToastHideButton toastmessage
        ]
        []

    toast attachclasses header body =
      let
        diff = Int.floor $ unwrap (Instant.diff state.instant body.at :: Minutes)

        min = Int.toStringAs Int.decimal diff
      in
        HH.div
          [ HP.classes [ HB.toast, if active then ClassName "show" else ClassName "hide" ]
          , HP.attr (AttrName "role") "alert"
          , HP.attr (AttrName "aria-live") "assertive"
          , HP.attr (AttrName "aria-atomic") "true"
          ]
          [ HH.div [ HP.classes $ Array.concat [ [ HB.toastHeader ], attachclasses ] ]
              [ HH.strong [ HP.class_ HB.meAuto ] header
              , HH.small_
                  $ if diff < 1 then
                      [ HH.text "just now" ]
                    else
                      [ HH.text $ min <> " mins ago" ]
              , closeButton
              ]
          , HH.div [ HP.class_ HB.toastBody ] [ HH.text body.message ]
          ]

successAleatDialog ∷ forall w i. String -> HH.HTML w i
successAleatDialog s =
  HH.div [ HP.classes [ HB.m2, HB.alert, HB.alertSuccess, HB.fs5 ] ]
    [ iconCheckCircleFill, HH.text s ]

dangerAleatDialog ∷ forall w i. String -> HH.HTML w i
dangerAleatDialog s =
  HH.div [ HP.classes [ HB.m2, HB.alert, HB.alertDanger, HB.fs5 ] ]
    [ iconExclamationTriangleFill, HH.text s ]

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ navigationBar
    , HH.main [ HP.classes [ HB.container, HB.pb5 ] {-, HP.style "margin-top: 80px;"-} ]
        [ HH.div
            [ HP.style "z-index: 1000"
            , HP.classes [ HB.toastContainer, HB.positionFixed, HB.top0, HB.end0, HB.p3 ]
            ]
            $ toastDialog state
        , HH.p [ HP.classes [ HB.m1 ] ]
            [ HH.a [ HP.href "https://github.com/ak1211/smartmeter-route-b-app" ] [ HH.text "スマートメーター ルート B テストアプリケーション" ]
            , HH.text "© 2023 Akihiro Yamamoto."
            ]
        , HH.p [ HP.classes [ HB.m1 ] ]
            [ HH.text "このWebアプリケーションは"
            , HH.a [ HP.href "https://developer.mozilla.org/ja/docs/Web/API/Web_Serial_API" ] [ HH.text "Web Serial API" ]
            , HH.text "を使用しています。"
            ]
        , case state.availableWebSerialApi of
            true -> successAleatDialog "Web Serial APIが使用できます。"
            false -> dangerAleatDialog "Web Serial APIに対応したブラウザで開いてください。"
        , HH.h2_ [ HH.text "ハードウエアの準備" ]
        , HH.p_
            [ HH.ul_
                [ HH.li_ [ HH.text "USB-シリアル変換器につないだBP35A1" ]
                , HH.li_ [ HH.text "USBドングル WSR35A1-00" ]
                , HH.li_ [ HH.text "USBドングル RL7023 Stick-D/IPS" ]
                ]
            , HH.text "などをUSBポートに挿入する。"
            , HH.br_
            , HH.text "またはコンピューターのシリアルポートに直接BP35A1を接続してください。"
            ]
        , HH.p_
            [ HH.text "コンピューターとBP35A1がシリアルポートを介して会話できるように接続出来たら、"
            , HH.text "このボタンを押してシリアルポートを開きます。"
            ]
        , HH.div [ HP.class_ HB.m3 ]
            [ HH.button
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
            , HH.text "などの応答かあるとBP35A1との通信ができている。"
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
                      [ HH.tr_ [ HH.td c [ HH.text "IPV6アドレス" ], HH.td c [ HH.text $ fromMaybe "不明" ipv6address ] ]
                      ]
                  ]
            ]
        --
        , HH.h5_ [ HH.text "MACアドレスからIPv6リンクローカルアドレスへ変換する" ]
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
              text = "SKJOIN " <> fromMaybe "" ipv6address
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
        , HH.slot _editform unit EditForm.component { maybeIpV6Address: state.maybeIpV6Address } HandleEditFormUpdate
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
        ]
    , footer
    ]
  where
  channel = _.channel <<< unwrap <$> state.maybeEpandesc

  channelPage = _.channelPage <<< unwrap <$> state.maybeEpandesc

  panId = _.panId <<< unwrap <$> state.maybeEpandesc

  addr = _.addr <<< unwrap <$> state.maybeEpandesc

  lqi = _.lqi <<< unwrap <$> state.maybeEpandesc

  rssi = Bp35a1.makeRSSIfromLQI <$> lqi

  pairId = _.pairId <<< unwrap <$> state.maybeEpandesc

  ipv6address = unwrap <$> state.maybeIpV6Address

  footer = HH.div [ HP.style "margin-bottom: 25vh;" ] []

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action ChildSlots output m Unit
handleAction = case _ of
  Initialize -> do
    api <- H.liftEffect $ WebSerialApi.available
    maybeSerialPortOpened <- H.request _terminal unit Terminal.IsOpenedSerialPort
    -- timer
    sid <- H.subscribe =<< timer Tick
    H.modify_
      _
        { maybeTimer = Just sid
        , availableWebSerialApi = api
        , serialportOpened = Maybe.fromMaybe false maybeSerialPortOpened
        }
    H.liftEffect $ bootstrapToastShow
  Finalize -> mempty
  Tick -> do
    instant <- H.liftEffect Now.now
    H.modify_ _ { instant = instant }
  OnClickOpenPortButton -> H.tell _terminal unit Terminal.OpenSerialPort
  OnClickClosePortButton -> H.tell _terminal unit Terminal.CloseSerialPort
  OnClickSetCommandLineButton s -> H.tell _terminal unit (Terminal.SetCommandLineString s)
  OnClickToastHideButton item ->
    let
      unsetflag :: forall a. Eq a => a -> Array (Tuple Boolean a) -> Maybe (Array (Tuple Boolean a))
      unsetflag value xs = do
        index <- Array.findIndex (\(Tuple _ y) -> y == value) xs
        Array.modifyAt index (\(Tuple _ y) -> Tuple false y) xs
    in
      do
        toastMessages <- H.gets _.toastMessages
        case unsetflag item toastMessages of
          Nothing -> mempty
          Just arr -> H.modify_ _ { toastMessages = arr }
  HandleTerminalUpdate msg -> handleTerminalOutput msg
  HandleEditFormUpdate (EditForm.SubmitToEchonetLiteMessage msg) -> H.tell _terminal unit (Terminal.SendCommand msg)

handleTerminalOutput :: forall output m. MonadAff m => Terminal.Output -> H.HalogenM State Action ChildSlots output m Unit
handleTerminalOutput = case _ of
  Terminal.Message a -> do
    let
      newToast = ToastMessage a
    toastMessages <- H.gets _.toastMessages
    H.modify_ _ { toastMessages = Array.snoc toastMessages (Tuple true newToast) }
  Terminal.MessageError { at: at, error: error } -> do
    let
      newToast = ToastMessageError { at: at, message: Exc.message error }
    toastMessages <- H.gets _.toastMessages
    H.modify_ _ { toastMessages = Array.snoc toastMessages (Tuple true newToast) }
  Terminal.ArraivalResponce { at: at, responce: responce } -> do
    toastMessages <- H.gets _.toastMessages
    toasts <- case responce of
      Bp35a1.ResEVENT a ->
        pure
          [ ToastMessageEvent
              { at: at
              , message: (Bp35a1.toStringEvent a).message
              }
          ]
      Bp35a1.ResEPANDESC a -> do
        H.modify_ _ { maybeEpandesc = Just a }
        pure [ ToastMessageEvent { at: at, message: show a } ]
      Bp35a1.ResERXUDP (ERXUDP erxudp) ->
        do
          echonetliteframe <-
            H.liftEffect
              $ Builder.execPut do
                  foldM (\_ x -> putUint8 x) mempty erxudp.payload
          EchonetLite.parseEchonetLiteFrame (DataView.whole echonetliteframe)
          >>= case _ of
              Left _ -> pure [ ToastMessageEvent { at: at, message: show (ERXUDP erxudp) } ]
              Right parsed -> do
                let
                  toStringProperty :: EchonetLite.Property -> Maybe String
                  toStringProperty p = EchonetLite.toStringWhmProperty <$> EchonetLite.makeSmartWhmProperty p

                  messages :: Array String
                  messages = Array.mapMaybe toStringProperty parsed.props
                pure $ map (\x -> ToastMessageEvent { at: at, message: x }) messages
      Bp35a1.ResIpV6Address a -> do
        H.modify_ _ { maybeIpV6Address = Just a }
        pure [ ToastMessageEvent { at: at, message: show a } ]
      Bp35a1.ResFAIL er ->
        pure
          [ ToastMessageError { at: at, message: show er } ]
      Bp35a1.ResOK ->
        pure
          [ ToastMessageEvent { at: at, message: "OK" } ]
      Bp35a1.ResLocalEcho a ->
        pure
          [ ToastMessageLocalEcho { at: at, message: a } ]
      Bp35a1.ResMessage a ->
        pure
          [ ToastMessageEvent { at: at, message: a } ]
    let
      messages = map (\a -> Tuple true a) toasts
    H.modify_ _ { toastMessages = toastMessages <> messages }
  Terminal.SerialportOpened -> do
    H.liftEffect $ logShow "Opened"
    H.modify_ _ { serialportOpened = true }
  Terminal.SerialportClosed -> do
    H.liftEffect $ logShow "Page.purs:587 Closed"
    H.modify_ _ { serialportOpened = false }

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <-
    H.liftAff $ Aff.forkAff
      $ forever do
          Aff.delay $ Milliseconds 60.0
          H.liftEffect $ HS.notify listener val
  pure emitter
