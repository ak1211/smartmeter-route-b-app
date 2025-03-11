{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module Page
  ( component
  ) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Const (Const)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), Minutes)
import Data.Tuple (Tuple(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
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
import Page.SkStack as PageSkStack
import Page.ToastMessage (ToastMessage(..), bootstrapToastShow)
import Page.J11Stack as PageJ11Stack
import Type.Proxy (Proxy(..))
import WebSerialApi as WebSerialApi

data Action
  = Initialize
  | Finalize
  | Tick
  | OnClickToastHideButton ToastMessage
  | OnClickChooseSkStackButton
  | OnClickChooseJ11StackButton
  | HandlePageSkStackUpdate PageSkStack.Output
  | HandlePageJ11StackUpdate PageJ11Stack.Output

-- Icons
iconCheckCircleFill ∷ forall w i. HH.HTML w i
iconCheckCircleFill = HH.i [ HP.classes [ ClassName "bi", ClassName "bi-check-circle-fill" ] ] []

iconExclamationTriangleFill ∷ forall w i. HH.HTML w i
iconExclamationTriangleFill = HH.i [ HP.classes [ ClassName "bi", ClassName "bi-exclamation-triangle-fill" ] ] []

iconExclamationCircleFill ∷ forall w i. HH.HTML w i
iconExclamationCircleFill = HH.i [ HP.classes [ ClassName "bi", ClassName "bi-exclamation-circle-fill" ] ] []

type ChildSlots
  = ( pageSkStack :: H.Slot (Const Void) PageSkStack.Output Unit
    , pageJ11Stack :: H.Slot (Const Void) PageJ11Stack.Output Unit
    )

_pageSkStack = Proxy :: Proxy "pageSkStack"

_pageJ11Stack = Proxy :: Proxy "pageJ11Stack"

data SelectedProtocolStack
  = Skstack
  | J11stack

type State
  = { selectedProtocolStack :: SelectedProtocolStack
    , maybeTimer :: Maybe SubscriptionId
    , instant :: Instant
    , availableWebSerialApi :: Boolean
    , toastMessages :: Array (Tuple Boolean ToastMessage)
    }

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
  { selectedProtocolStack: Skstack
  , maybeTimer: Nothing
  , instant: unsafePerformEffect $ Now.now
  , availableWebSerialApi: false
  , toastMessages: []
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
    ToastMessageNotify a -> toastMessageNotify a
    ToastMessageCommandResponse a -> toastMessageCommandResponse a
    where
    toastMessage = toast [ HB.textWhite, HB.bgDark ] [ iconCheckCircleFill, HH.text "メッセージ" ]

    toastMessageError = toast [ HB.textWarning, HB.bgDark ] [ iconExclamationCircleFill, HH.text "失敗しました" ]

    toastMessageEvent = toast [ HB.textDark, HB.bgInfo ] [ iconCheckCircleFill, HH.text "イベント" ]

    toastMessageLocalEcho = toast [ HB.textDark, HB.bgInfo ] [ iconCheckCircleFill, HH.text "エコーバック" ]

    toastMessageNotify = toast [ HB.textDark, HB.bgInfo ] [ iconCheckCircleFill, HH.text "通知" ]

    toastMessageCommandResponse = toast [ HB.textDark, HB.bgInfo ] [ iconCheckCircleFill, HH.text "コマンド応答" ]

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
        , HH.h2_ [ HH.text "通信プロトコルスタックを選択してください" ]
        , HH.p_
            [ HH.ul_
                [ HH.li_
                    [ HH.text "SKSTACK IP (SKプロトコルスタック)"
                    , HH.br_
                    , HH.text "BP35A1, RL7023 Stickなど通信仕様がSKSTACKプロトコルスタックのモジュールはこちらを選択する"
                    ]
                , HH.li_
                    [ HH.text "Wi-SUN Enhanced HAN plus B-Route Dual stack (J11プロトコルスタック)"
                    , HH.br_
                    , HH.text "BP35C0-J11-T01, BP35C2-J11-T01など通信仕様がJ11プロトコルスタックのモジュールはこちらを選択する"
                    ]
                ]
            ]
        , HH.ul
            [ HP.classes [ HB.mb3, HB.nav, HB.navTabs, HB.justifyContentCenter ] ]
            [ HH.li [ HP.class_ HB.navItem ]
                [ HH.button
                    [ case state.selectedProtocolStack of
                        Skstack -> HP.classes [ HB.btnLink, HB.navLink, HB.active ]
                        J11stack -> HP.classes [ HB.btnLink, HB.navLink ]
                    , HE.onClick \_ -> OnClickChooseSkStackButton
                    ]
                    [ HH.text "SKプロトコルスタック" ]
                ]
            , HH.li [ HP.classes [ HB.navItem ] ]
                [ HH.button
                    [ case state.selectedProtocolStack of
                        Skstack -> HP.classes [ HB.btnLink, HB.navLink ]
                        J11stack -> HP.classes [ HB.btnLink, HB.navLink, HB.active ]
                    , HE.onClick \_ -> OnClickChooseJ11StackButton
                    ]
                    [ HH.text "J11プロトコルスタック" ]
                ]
            ]
        , case state.selectedProtocolStack of
            Skstack -> HH.slot _pageSkStack unit PageSkStack.component unit HandlePageSkStackUpdate
            J11stack -> HH.slot _pageJ11Stack unit PageJ11Stack.component unit HandlePageJ11StackUpdate
        ]
    , footer
    ]
  where
  footer = HH.div [ HP.style "margin-bottom: 25vh;" ] []

handleAction :: forall input output m. MonadAff m => Action -> H.HalogenM State Action input output m Unit
handleAction = case _ of
  Initialize -> do
    api <- H.liftEffect $ WebSerialApi.available
    -- timer
    sid <- H.subscribe =<< timer Tick
    H.modify_
      _
        { maybeTimer = Just sid
        , availableWebSerialApi = api
        }
    H.liftEffect $ bootstrapToastShow
  Finalize -> mempty
  Tick -> do
    instant <- H.liftEffect Now.now
    H.modify_ _ { instant = instant }
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
  OnClickChooseSkStackButton -> H.modify_ _ { selectedProtocolStack = Skstack }
  OnClickChooseJ11StackButton -> H.modify_ _ { selectedProtocolStack = J11stack }
  HandlePageSkStackUpdate
    (PageSkStack.NewToastMessages newToasts) -> do
    let
      messages = map (\a -> Tuple true a) newToasts
    toastMessages <- H.gets _.toastMessages
    H.modify_ _ { toastMessages = toastMessages <> messages }
  HandlePageJ11StackUpdate (PageJ11Stack.NewToastMessages newToasts) -> do
    let
      messages = map (\a -> Tuple true a) newToasts
    toastMessages <- H.gets _.toastMessages
    H.modify_ _ { toastMessages = toastMessages <> messages }

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <-
    H.liftAff $ Aff.forkAff
      $ forever do
          Aff.delay $ Milliseconds 60.0
          H.liftEffect $ HS.notify listener val
  pure emitter
