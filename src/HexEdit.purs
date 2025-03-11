{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module HexEdit
  ( Output(..)
  , Query(..)
  , RxDataLog
  , TxDataLog
  , TxRxDataLog(..)
  , component
  , initialState
  ) where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.Builder (putArrayBuffer)
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.Cast as Cast
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as ArrayBufferTyped
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Char as Char
import Data.CodePoint.Unicode as Unicode
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.RFC3339String as RFC3339
import Data.String (codePointFromChar)
import Data.String as String
import Data.Traversable (traverse_)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (logShow)
import Effect.Exception (Error)
import Effect.Exception as Exc
import Effect.Now as Now
import Effect.Ref as EffectRef
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (SubscriptionId)
import Halogen.Subscription as HS
import Halogen.Themes.Bootstrap5 as HB
import ProtocolStack.J11StackUart.Response as J11StackUart
import ProtocolStack.J11StackUart.Command.Types (J11Command(..))
import ProtocolStack.J11StackUart.Command.Types as CT
import Utility as Utility
import Web.HTML as HTML
import Web.HTML.Window (RequestIdleCallbackId)
import Web.HTML.Window as Window
import WebSerialApi as WebSerialApi

data Output
  = Message { at :: Instant, message :: String }
  | MessageError { at :: Instant, error :: Error }
  | ArrivalResponse { at :: Instant, response :: J11StackUart.Response }
  | SerialportOpened
  | SerialportClosed

type TxDataLog
  = { at :: Instant, sendCommand :: Array UInt }

type RxDataLog
  = { at :: Instant, receiveCommand :: Array UInt }

data TxRxDataLog
  = TxDataLog TxDataLog
  | RxDataLog RxDataLog

type State
  = { maybeIdleCallbackId :: Maybe SubscriptionId
    , maybeSerialport :: Maybe WebSerialApi.SerialPort
    , maybeReadFiber :: Maybe (Fiber Unit)
    , rxdataChunk :: ArrayBuffer
    , txrxDataHistories :: Array TxRxDataLog
    , hexEditLine :: Array UInt
    }

data Query a
  = OpenSerialPort Int a
  | CloseSerialPort a
  | SetJ11Command J11Command a
  | SendCommand J11Command a
  | IsOpenedSerialPort (Boolean -> a)

data Action
  = Initialize
  | Finalize
  | HandleSerialPortOpen H.SubscriptionId (Either Error WebSerialApi.SerialPort)
  | HandleSerialPortClose H.SubscriptionId (Either Error Unit)
  | OnClickFormSendButton
  | HandleSerialPortArrivalData H.SubscriptionId WebSerialApi.ReadResult
  | HandleIdleCallback

component :: forall input m. MonadAff m => H.Component Query input Output m
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
              }
    }

initialState :: forall input. input -> State
initialState _ =
  { maybeIdleCallbackId: Nothing
  , maybeSerialport: Nothing
  , maybeReadFiber: Nothing
  , rxdataChunk: Utility.zeroSizeArrayBuffer
  , txrxDataHistories: []
  , hexEditLine: []
  }

render :: forall input m. State -> H.ComponentHTML Action input m
render state =
  HH.div []
    [ HH.div [ HP.class_ HB.mb1 ] statusline
    , HH.div [ HP.classes [ HB.containerFluid ] ]
        [ HH.div [ HP.classes [ HB.row, HB.mb1, HB.bgDark, HB.textWhite ] ]
            [ HH.pre [ HP.classes [ HB.col ], HP.style "max-height: 30vh" ]
                $ Array.concatMap
                    ( case _ of
                        TxDataLog txd -> txDataLog txd
                        RxDataLog rxd -> rxDataLog rxd
                    )
                    state.txrxDataHistories
            ]
        , hexEditer state.hexEditLine
        ]
    ]
  where
  portStatus = case Maybe.isJust state.maybeSerialport of
    true -> { classes: [ HB.bgSuccess ], text: "PORT OPENED" }
    false -> { classes: [ HB.bgDark, HB.textWarning ], text: "PORT CLOSED" }

  statusline =
    [ HH.div
        [ HP.classes $ [ HB.p2, HB.border, HB.me1, HB.badge ] <> portStatus.classes ]
        [ HH.text portStatus.text ]
    ]

  txDataLog txd =
    [ HH.text $ unwrap $ RFC3339.fromDateTime $ Instant.toDateTime txd.at
    , HH.text " 送信 -> "
    , HH.text <<< String.joinWith " " $ map (String.toUpper <<< Utility.toStringHexAs Utility.octet) txd.sendCommand
    , HH.br_
    ]

  rxDataLog rxd =
    [ HH.text $ unwrap $ RFC3339.fromDateTime $ Instant.toDateTime rxd.at
    , HH.text " 受信 <- "
    , HH.text <<< String.joinWith " " $ map (String.toUpper <<< Utility.toStringHexAs Utility.octet) rxd.receiveCommand
    , HH.br_
    ]

hexEditer :: forall input m. Array UInt -> H.ComponentHTML Action input m
hexEditer editline =
  HH.div [ HP.classes [ HB.row ] ]
    [ HH.div [ HP.classes [ HB.col ] ]
        $ map (\x -> displayColumns x.columns)
        $ at16bytes elemWithIndex
    , HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes [ HB.col1, HB.btn, HB.btnPrimary ]
        , HE.onClick $ const OnClickFormSendButton
        , HP.enabled $ not (Array.null editline)
        ]
        [ HH.text "送信" ]
    ]
  where
  elemWithIndex :: Array { index :: Int, elem :: UInt }
  elemWithIndex = Utility.elemWithIndex editline

  at16bytes :: Array { index :: Int, elem :: UInt } -> Array { columns :: Array { index :: Int, elem :: UInt } }
  at16bytes = map { columns: _ } <<< Utility.toArrayArray 16

  displayColumns :: forall w i. Array { index :: Int, elem :: UInt } -> HH.HTML w i
  displayColumns array =
    let
      address = maybe 0 _.index $ Array.head array

      addressText = [ HH.text $ String.toUpper $ Utility.toStringHexAs Utility.dword $ UInt.fromInt address ]

      splitted = Array.splitAt 8 array
    in
      HH.div [ HP.classes [ HB.row, HB.rowCols3 ] ]
        [ HH.span [ HP.classes [ HB.col2, HB.fontMonospace, HB.textPrimary ] ] addressText
        , HH.span [ HP.classes [ HB.col7, HB.fontMonospace ] ]
            $ Array.concat
                [ map displaySingleByte splitted.before
                , [ HH.span [ HP.classes [ HB.mx1 ] ] [ HH.text "" ] ]
                , map displaySingleByte splitted.after
                ]
        , HH.span [ HP.classes [ HB.col3, HB.fontMonospace ] ] $ map displaySingleChar array
        ]

  displaySingleByte :: forall w i. { index :: Int, elem :: UInt } -> HH.HTML w i
  displaySingleByte byte =
    HH.span
      [ HP.id $ "index#" <> Int.toStringAs Int.decimal byte.index
      , HP.classes [ HB.m1, HB.fontMonospace ]
      , contenteditable "false"
      ]
      [ HH.text $ String.toUpper $ Utility.toStringHexAs Utility.octet byte.elem ]

  displaySingleChar :: forall w i. { index :: Int, elem :: UInt } -> HH.HTML w i
  displaySingleChar byte =
    HH.span [ HP.classes [ HB.m0, HB.fontMonospace, HB.textPrimary ] ]
      [ HH.text $ printChar byte.elem ]

  printChar :: UInt -> String
  printChar x = case codePointFromChar <$> (Char.fromCharCode $ UInt.toInt x) of
    Just codepoint
      | Unicode.isPrint codepoint -> String.singleton codepoint
    _ -> "."

handleAction :: forall input m. MonadAff m => Action -> H.HalogenM State Action input Output m Unit
handleAction = case _ of
  Initialize -> do
    sid <- H.subscribe $ emitRequestIdleCallback HandleIdleCallback
    H.modify_ _ { maybeIdleCallbackId = Just sid }
  Finalize -> do
    (st :: State) <- H.get
    maybe mempty H.unsubscribe st.maybeIdleCallbackId
    maybe mempty (H.liftAff <<< Aff.killFiber (Exc.error "kill to read loop fiber")) st.maybeReadFiber
    maybe mempty (H.liftAff <<< WebSerialApi.forgetPort) st.maybeSerialport
  HandleSerialPortOpen sid (Left err) -> do
    H.liftEffect $ logShow ("port open error:" <> show err)
    now <- H.liftEffect $ Now.now
    H.raise $ MessageError { at: now, error: err }
    H.raise SerialportClosed
    H.unsubscribe sid
  HandleSerialPortOpen sid (Right serialport) -> do
    H.modify_ _ { maybeSerialport = Just serialport }
    now <- H.liftEffect $ Now.now
    H.raise $ Message { at: now, message: "シリアルポート接続成功" }
    H.raise SerialportOpened
    fiber <- runReadSerialPortThread serialport
    H.modify_ _ { maybeReadFiber = Just fiber }
    H.liftEffect $ logShow "start serial port reader thread"
    H.unsubscribe sid
  HandleSerialPortClose sid (Left err) -> do
    H.liftEffect $ logShow $ "HandleSerialPortClose" <> show err
    H.modify_ _ { maybeSerialport = Nothing }
    now <- H.liftEffect $ Now.now
    H.raise $ MessageError { at: now, error: err }
    H.raise SerialportClosed
    H.unsubscribe sid
  HandleSerialPortClose sid (Right _) -> do
    H.modify_ _ { maybeSerialport = Nothing }
    now <- H.liftEffect $ Now.now
    H.raise $ Message { at: now, message: "シリアルポートを閉じました" }
    H.raise SerialportClosed
    H.unsubscribe sid
  OnClickFormSendButton -> do
    now <- H.liftEffect $ Now.now
    (st :: State) <- H.get
    case st.maybeSerialport of
      Nothing -> H.raise $ MessageError { at: now, error: Exc.error "シリアルポートが閉じています。" }
      Just serialport -> do
        (sendJ11Command :: J11Command) <- H.liftEffect $ (CT.makeJ11Command <=< ArrayBufferTyped.fromArray) st.hexEditLine
        H.liftEffect $ writeJ11CommandToSerialPort serialport sendJ11Command
        let
          txDataLog = TxDataLog { at: now, sendCommand: st.hexEditLine }

          newTxrxDataHistories = Array.snoc st.txrxDataHistories txDataLog
        H.modify_ _ { hexEditLine = [], txrxDataHistories = newTxrxDataHistories }
  HandleSerialPortArrivalData sid WebSerialApi.Closed -> do
    now <- H.liftEffect $ Now.now
    H.raise $ MessageError { at: now, error: Exc.error "シリアルポートが閉じています。" }
    H.unsubscribe sid
    maybeReadFiber <- H.gets _.maybeReadFiber
    maybe mempty (H.liftAff <<< Aff.killFiber (Exc.error "kill to read loop fiber")) maybeReadFiber
    H.modify_ _ { maybeReadFiber = Nothing }
  HandleSerialPortArrivalData _ (WebSerialApi.Chunk chunk) -> do
    thisRxd <- H.gets _.rxdataChunk
    newRxd <-
      H.liftEffect
        $ Builder.execPut do
            putArrayBuffer thisRxd
            putArrayBuffer $ ArrayBufferTyped.buffer chunk
    H.modify_ _ { rxdataChunk = newRxd }
  HandleIdleCallback -> handleIdleCallback

handleIdleCallback :: forall input m. MonadEffect m => H.HalogenM State Action input Output m Unit
handleIdleCallback = do
  rxdChunk <- H.gets _.rxdataChunk
  -- 受信バイナリをパースする
  rxdResult <- H.liftEffect $ J11StackUart.parseReceiveData (DV.whole rxdChunk)
  case rxdResult of
    Left _ -> mempty
    Right result -> do
      -- パーサーで消費した分
      parsed <- H.liftEffect $ DV.part rxdChunk 0 result.positionIndex
      let
        -- パーサーで消費した残り
        newChunk = ArrayBuffer.slice result.positionIndex (ArrayBuffer.byteLength rxdChunk) rxdChunk
      -- パーサーで消費した分を取り除いた断片を次回の処理に回す
      H.modify_ _ { rxdataChunk = newChunk }
      response <- H.liftEffect $ J11StackUart.fromJ11ResponseCommandFormat result.responseCommandFormat
      now <- H.liftEffect $ Now.now
      H.raise $ ArrivalResponse { at: now, response: response }
      --
      receiveCommand <- H.liftEffect (ArrayBufferTyped.toArray =<< Cast.toUint8Array parsed)
      let
        rxDataLog = RxDataLog { at: now, receiveCommand: receiveCommand }
      (st :: State) <- H.get
      H.modify_ _ { txrxDataHistories = Array.snoc st.txrxDataHistories rxDataLog }

handleQuery :: forall input m a. MonadAff m => Query a -> H.HalogenM State Action input Output m (Maybe a)
handleQuery = case _ of
  OpenSerialPort boudrate next -> do
    H.liftEffect $ logShow $ "boudrate: " <> Int.toStringAs Int.decimal boudrate
    openSerialPort boudrate
    pure (Just next)
  CloseSerialPort next -> do
    closeSerialPort
    pure (Just next)
  SetJ11Command j11command next -> do
    currentHexEditLine <- H.gets _.hexEditLine
    let
      newHexEditLine = unwrap j11command
    when (currentHexEditLine /= newHexEditLine)
      $ H.modify_ _ { hexEditLine = newHexEditLine }
    pure (Just next)
  SendCommand command next ->
    let
      fail = do
        now <- H.liftEffect $ Now.now
        H.raise $ MessageError { at: now, error: Exc.error "シリアルポートが閉じています。" }

      go serialport = H.liftEffect $ writeJ11CommandToSerialPort serialport command
    in
      do
        maybe fail go =<< H.gets _.maybeSerialport
        pure (Just next)
  IsOpenedSerialPort k -> do
    (opened :: Boolean) <- Maybe.isJust <$> H.gets _.maybeSerialport
    pure (Just (k opened))

openSerialPort :: forall input output m. MonadEffect m => MonadAff m => Int -> H.HalogenM State Action input output m Unit
openSerialPort boudrate = do
  { emitter, listener } <- H.liftEffect HS.create
  sid <- H.subscribe emitter
  H.liftEffect $ Aff.runAff_ (callback listener sid) open
  where
  open :: Aff WebSerialApi.SerialPort
  open =
    let
      option = WebSerialApi.defaultOpenPortOption { baudRate = boudrate }
    in
      do
        port <- WebSerialApi.requestPort
        WebSerialApi.openPort port option
        pure port

  callback listener sid x = H.liftEffect $ HS.notify listener (HandleSerialPortOpen sid x)

closeSerialPort :: forall input output m. MonadEffect m => MonadAff m => H.HalogenM State Action input output m Unit
closeSerialPort = do
  (st :: State) <- H.get
  maybe mempty (H.liftAff <<< Aff.killFiber (Exc.error "kill to read loop fiber")) st.maybeReadFiber
  maybe mempty go st.maybeSerialport
  H.modify_ _ { maybeReadFiber = Nothing, maybeSerialport = Nothing }
  where
  go serialport = do
    { emitter, listener } <- H.liftEffect HS.create
    sid <- H.subscribe emitter
    fiber <- H.liftEffect $ Aff.runAff (callback listener sid) $ WebSerialApi.closePort serialport
    liftAff $ Aff.joinFiber fiber

  callback listener sid x = H.liftEffect $ HS.notify listener (HandleSerialPortClose sid x)

runReadSerialPortThread :: forall input output m. MonadEffect m => MonadAff m => WebSerialApi.SerialPort -> H.HalogenM State Action input output m (Fiber Unit)
runReadSerialPortThread serialport = do
  reader <- H.liftEffect $ WebSerialApi.getReader serialport
  { emitter, listener } <- H.liftEffect HS.create
  sid <- H.subscribe emitter
  go reader listener sid
  where
  go reader listener sid = do
    H.liftAff $ Aff.forkAff $ Aff.finally release $ tailRecM loop unit
    where
    release = H.liftEffect $ WebSerialApi.releaseLockReader reader

    loop _ = do
      result <- WebSerialApi.read reader
      H.liftEffect $ HS.notify listener (HandleSerialPortArrivalData sid result)
      case result of
        WebSerialApi.Closed -> pure (Done unit)
        WebSerialApi.Chunk _ -> pure (Loop unit)

writeJ11CommandToSerialPort :: WebSerialApi.SerialPort -> J11Command -> Effect Unit
writeJ11CommandToSerialPort serialport (J11Command command) = do
  arraybuffer <-
    Builder.execPut do
      traverse_ Builder.putUint8 command
  u8array <- Cast.toUint8Array $ DV.whole arraybuffer
  writeUint8ArrayToSerialPort serialport u8array

writeUint8ArrayToSerialPort :: WebSerialApi.SerialPort -> Uint8Array -> Effect Unit
writeUint8ArrayToSerialPort serialport array = do
  writer <- WebSerialApi.getWriter serialport
  let
    toRelease :: Aff Unit
    toRelease = liftEffect $ WebSerialApi.releaseLockWriter writer
  Aff.launchAff_ $ Aff.finally toRelease $ WebSerialApi.write writer array

emitRequestIdleCallback :: Action -> HS.Emitter Action
emitRequestIdleCallback val = HS.makeEmitter go
  where
  go emitter =
    H.liftEffect do
      ref <- EffectRef.new (Nothing :: Maybe RequestIdleCallbackId)
      let
        loop = do
          emitter val
          window <- HTML.window
          callbackId <- Window.requestIdleCallback { timeout: 300 } loop window
          EffectRef.write (Just callbackId) ref
      loop
      pure (traverse_ cancel =<< EffectRef.read ref)

  cancel callbackId = do
    window <- HTML.window
    Window.cancelIdleCallback callbackId window

contenteditable :: forall r i. String -> HP.IProp r i
contenteditable = HP.attr (AttrName "contenteditable")
