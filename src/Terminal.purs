{-
 https://github.com/ak1211/smartmeter-route-b-app
 Copyright (c) 2023 Akihiro Yamamoto.
 Licensed under the MIT License.
 See LICENSE file in the project root for full license information.
-}
module Terminal
  ( Output(..)
  , Query(..)
  , component
  ) where

import Prelude
import Bp35a1 as Bp35a1
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.ArrayBuffer.Builder (putArrayBuffer, putUint8)
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.Typed as ArrayBufferTyped
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Char as Char
import Data.CodePoint.Unicode as Unicode
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Enum as Enum
import Data.Foldable (foldM)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe as Maybe
import Data.MediaType (MediaType(..))
import Data.String (CodePoint, joinWith)
import Data.String as CodePoint
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse_)
import Data.UInt (UInt)
import Data.UInt as UInt
import EchonetLite (stringWithZeroPadding)
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (Error)
import Effect.Exception as Exc
import Effect.Now as Now
import Effect.Ref as EffectRef
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (SubscriptionId)
import Halogen.Subscription as HS
import Halogen.Themes.Bootstrap5 as HB
import Parsing as P
import Parsing.String as PString
import Parsing.Token as PToken
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel
import Web.File.Blob as FB
import Web.File.Url as FU
import Web.HTML as HTML
import Web.HTML.Window (RequestIdleCallbackId)
import Web.HTML.Window as Window
import WebSerialApi as WebSerialApi

data Output
  = Message { at :: Instant, message :: String }
  | MessageError { at :: Instant, error :: Error }
  | ArraivalResponce { at :: Instant, responce :: Bp35a1.Responce }
  | SerialportOpened
  | SerialportClosed

type State
  = { maybeIdleCallbackId :: Maybe SubscriptionId
    , maybeSerialport :: Maybe WebSerialApi.SerialPort
    , maybeReadFiber :: Maybe (Fiber Unit)
    , txHistories :: List { at :: Instant, sendText :: String }
    , rxDataArraybuffer :: Maybe ArrayBuffer
    , commandlineText :: String
    , terminalText :: String
    , saveObjectUrl :: String
    }

data Query a
  = OpenSerialPort a
  | CloseSerialPort a
  | SetCommandLineString String a
  | SendCommand (Array UInt) a
  | IsOpenedSerialPort (Boolean -> a)

data Action
  = Initialize
  | Finalize
  | HandleSerialPortOpen H.SubscriptionId (Either Error WebSerialApi.SerialPort)
  | HandleSerialPortClose H.SubscriptionId (Either Error Unit)
  | OnValueInputFormInputArea String
  | OnClickFormSendButton
  | HandleInput (Maybe String)
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
  , txHistories: List.Nil
  , rxDataArraybuffer: Nothing
  , commandlineText: ""
  , terminalText: ""
  , saveObjectUrl: ""
  }

render :: forall input m. State -> H.ComponentHTML Action input m
render state =
  HH.div []
    [ HH.div [ HP.class_ HB.mb1 ] statusline
    , HH.div [ HP.classes [ HB.containerFluid ] ]
        [ HH.div [ HP.classes [ HB.row, HB.mb1, HB.bgDark, HB.textWhite ] ] terminaltext
        , commandline
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
    , HH.a
        [ HP.classes
            $ Array.concat
                [ [ HB.btn
                  , HB.btnSm
                  , HB.btnOutlineDark
                  ]
                , if state.terminalText == "" then [ HB.disabled ] else []
                ]
        , HP.href state.saveObjectUrl
        , HP.target "_blank"
        , HP.download "terminal.log"
        ]
        [ HH.text "保存" ]
    ]

  terminaltext =
    [ HH.pre
        [ HP.classes [ HB.col ]
        , HP.style "max-height: 30vh"
        ]
        [ HH.text $ if state.terminalText == "" then " " else state.terminalText ]
    ]

  commandline =
    HH.div [ HP.classes [ HB.row ] ]
      [ HH.input
          [ HP.classes [ HB.col, HB.me1, HB.formControl ]
          , HP.type_ HP.InputText
          , HP.placeholder "input here..."
          , HE.onValueInput (\ev -> OnValueInputFormInputArea ev)
          , HP.value state.commandlineText
          ]
      , HH.button
          [ HP.type_ HP.ButtonButton
          , HP.classes [ HB.col1, HB.btn, HB.btnPrimary ]
          , HE.onClick $ const OnClickFormSendButton
          ]
          [ HH.text "送信" ]
      ]

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
    H.liftEffect $ log ("port open error:" <> show err)
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
    H.liftEffect $ log "start serial port reader thread"
    H.unsubscribe sid
  HandleSerialPortClose sid (Left err) -> do
    H.liftEffect $ logShow (err)
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
  OnValueInputFormInputArea inputText -> H.modify_ _ { commandlineText = inputText }
  OnClickFormSendButton -> do
    now <- H.liftEffect $ Now.now
    (st :: State) <- H.get
    case st.maybeSerialport of
      Nothing -> H.raise $ MessageError { at: now, error: Exc.error "シリアルポートが閉じています。" }
      Just serialport -> case encodeToUIntArrayFromAsciiCodes (st.commandlineText <> "\r\n") of
        Left err -> do
          H.liftEffect $ log ("PARSE ERROR " <> P.parseErrorMessage err)
          H.raise $ MessageError { at: now, error: Exc.error ("不正な文字があります: " <> P.parseErrorMessage err) }
        Right encodedcommand -> do
          let
            text = decodeToStringFromArrayUInt encodedcommand
          H.liftEffect $ log ("SEND: " <> Maybe.fromMaybe "" text)
          H.liftEffect $ writeBinaryToSerialPort serialport encodedcommand
          let
            newHistories = { at: now, sendText: st.commandlineText } : st.txHistories
          H.modify_ _ { commandlineText = "", txHistories = newHistories }
  HandleInput Nothing -> mempty
  HandleInput (Just newText) -> do
    currentText <- H.gets _.commandlineText
    when (currentText /= newText)
      $ H.modify_ _ { commandlineText = newText }
  HandleSerialPortArrivalData sid WebSerialApi.Closed -> do
    now <- H.liftEffect $ Now.now
    H.raise $ MessageError { at: now, error: Exc.error "シリアルポートが閉じています。" }
    H.unsubscribe sid
    maybeReadFiber <- H.gets _.maybeReadFiber
    maybe mempty (H.liftAff <<< Aff.killFiber (Exc.error "kill to read loop fiber")) maybeReadFiber
    H.modify_ _ { maybeReadFiber = Nothing }
  HandleSerialPortArrivalData _ (WebSerialApi.Chunk chunk) -> do
    rxDataArraybuffer <- H.gets _.rxDataArraybuffer
    newBuf <-
      H.liftEffect
        $ Builder.execPut do
            maybe mempty putArrayBuffer rxDataArraybuffer
            putArrayBuffer (ArrayBufferTyped.buffer chunk)
    H.modify_ _ { rxDataArraybuffer = Just newBuf }
  HandleIdleCallback -> handleIdleCallback

handleIdleCallback :: forall input m. MonadEffect m => H.HalogenM State Action input Output m Unit
handleIdleCallback = do
  rxDataArraybuffer <- H.gets _.rxDataArraybuffer
  case rxDataArraybuffer of
    Nothing -> mempty
    Just arraybuf -> do
      txHistories <- H.gets _.txHistories
      let
        latestSendText = _.sendText <$> List.head txHistories
      --
      buf <- H.liftEffect $ ArrayBufferTyped.whole arraybuf
      resp <- Bp35a1.parseResponce latestSendText buf
      case resp of
        Right a -> do
          now <- H.liftEffect $ Now.now
          H.raise $ ArraivalResponce { at: now, responce: a.responce }
        Left _ -> mempty
      --
      maybeLine <- H.liftEffect $ splitSingleLine buf
      case maybeLine of
        Nothing -> mempty
        Just { line: line, remains: remains } -> do
          H.modify_ _ { rxDataArraybuffer = Just $ ArrayBufferTyped.buffer remains }
          --
          decoder <- H.liftEffect $ TextDecoder.new UtfLabel.utf8
          string <- H.liftEffect $ TextDecoder.decode line decoder
          terminalText <- H.gets _.terminalText
          H.modify_ _ { terminalText = terminalText <> string }
          H.liftEffect $ logShow string
      --
      terminalText <- H.gets _.terminalText
      let
        blob = FB.fromString terminalText (MediaType "text/plain")
      saveObjectUrl <- H.liftEffect $ FU.createObjectURL blob
      H.modify_ _ { saveObjectUrl = saveObjectUrl }

splitSingleLine :: Uint8Array -> Effect (Maybe { line :: Uint8Array, remains :: Uint8Array })
splitSingleLine buf =
  ArrayBufferTyped.findIndex (\c _ -> c == linefeed) buf
    >>= case _ of
        Nothing -> pure Nothing
        Just pos -> do
          line <- ArrayBufferTyped.slice 0 (pos + 1) buf
          remains <- ArrayBufferTyped.slice (pos + 1) (ArrayBufferTyped.byteLength buf) buf
          pure $ Just { line: line, remains: remains }
  where
  linefeed = UInt.fromInt 0x0a

handleQuery :: forall input m a. MonadAff m => Query a -> H.HalogenM State Action input Output m (Maybe a)
handleQuery = case _ of
  OpenSerialPort next -> do
    openSerialPort
    pure (Just next)
  CloseSerialPort next -> do
    closeSerialPort
    pure (Just next)
  SetCommandLineString newText next -> do
    currentText <- H.gets _.commandlineText
    when (currentText /= newText)
      $ H.modify_ _ { commandlineText = newText }
    pure (Just next)
  SendCommand array next ->
    let
      fail = do
        now <- H.liftEffect $ Now.now
        H.raise $ MessageError { at: now, error: Exc.error "シリアルポートが閉じています。" }

      go serialport = H.liftEffect $ writeBinaryToSerialPort serialport array

      text = show array

      text' = Maybe.fromMaybe "" $ decodeToStringFromArrayUInt array
    in
      do
        H.liftEffect $ log ("SENDCOMMAND: " <> text)
        H.liftEffect $ log ("SENDCOMMAND: " <> text')
        maybe fail go =<< H.gets _.maybeSerialport
        pure (Just next)
  IsOpenedSerialPort k -> do
    (opened :: Boolean) <- Maybe.isJust <$> H.gets _.maybeSerialport
    pure (Just (k opened))

openSerialPort :: forall input output m. MonadEffect m => MonadAff m => H.HalogenM State Action input output m Unit
openSerialPort = do
  { emitter, listener } <- H.liftEffect HS.create
  sid <- H.subscribe emitter
  H.liftEffect $ Aff.runAff_ (callback listener sid) open
  where
  open :: Aff WebSerialApi.SerialPort
  open = do
    port <- WebSerialApi.requestPort
    WebSerialApi.openPort port WebSerialApi.defaultOpenPortOption
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

decodeToStringFromArrayUInt :: Array UInt -> Maybe String
decodeToStringFromArrayUInt input = do
  cs <- toCodePoints input
  Just (joinWith "" $ map mapping cs)
  where
  toCodePoints :: Array UInt -> Maybe (Array CodePoint)
  toCodePoints xs =
    let
      maybeChars :: Array (Maybe Char)
      maybeChars = map (Char.fromCharCode <<< UInt.toInt) xs
    in
      if Array.all Maybe.isJust maybeChars then
        Just (map String.codePointFromChar $ Array.catMaybes maybeChars)
      else
        Nothing

  mapping :: CodePoint -> String
  mapping c
    | c == CodePoint.codePointFromChar '\r' = "\\r"
    | c == CodePoint.codePointFromChar '\n' = "\\n"
    | Unicode.isAscii c && Unicode.isPrint c = String.singleton c
    | otherwise =
      let
        n = Enum.fromEnum c
      in
        "\\x" <> stringWithZeroPadding 2 (Int.toStringAs Int.hexadecimal n)

encodeToUIntArrayFromAsciiCodes :: String -> Either P.ParseError (Array UInt)
encodeToUIntArrayFromAsciiCodes input =
  P.runParser input do
    as <- Array.many (hexedCode <|> anyCharacter)
    pure as
  where
  anyCharacter :: forall m. P.ParserT String m UInt
  anyCharacter = do
    c <- PString.anyChar
    pure (UInt.fromInt $ Char.toCharCode c)

  hexedCode :: forall m. P.ParserT String m UInt
  hexedCode = do
    hexed <- (PString.string "\\x" <|> PString.string "\\\\x") *> Array.many PToken.hexDigit
    let
      cs :: String
      cs = CodeUnits.fromCharArray hexed

      maybeUI :: Maybe UInt
      maybeUI = UInt.fromInt <$> Int.fromStringAs Int.hexadecimal cs
    maybe (P.fail "code conversion failed.") pure maybeUI

writeBinaryToSerialPort :: WebSerialApi.SerialPort -> Array UInt -> Effect Unit
writeBinaryToSerialPort serialport array =
  toUint8Array array
    >>= writeUint8ArrayToSerialPort serialport
  where
  toUint8Array :: Array UInt -> Effect Uint8Array
  toUint8Array = ArrayBufferTyped.whole <=< createArrayBuffer

writeUint8ArrayToSerialPort :: WebSerialApi.SerialPort -> Uint8Array -> Effect Unit
writeUint8ArrayToSerialPort serialport array = do
  writer <- WebSerialApi.getWriter serialport
  let
    toRelease :: Aff Unit
    toRelease = liftEffect $ WebSerialApi.releaseLockWriter writer
  Aff.launchAff_ $ Aff.finally toRelease $ WebSerialApi.write writer array

createArrayBuffer :: Array UInt -> Effect ArrayBuffer
createArrayBuffer array =
  Builder.execPut do
    foldM (\_ x -> putUint8 x) mempty array

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
