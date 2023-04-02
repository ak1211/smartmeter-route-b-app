{-
 https://github.com/ak1211/smartmeter-route-b-app
 Copyright (c) 2023 Akihiro Yamamoto.
 Licensed under the MIT License.
 See LICENSE file in the project root for full license information.
-}
module WebSerialApi.SerialPort
  ( SerialPort
  , ReadResult(..)
  , Reader
  , Writer
  , DataBits(..)
  , FlowControl(..)
  , OpenPortOption(..)
  , Parity(..)
  , StopBits(..)
  , defaultOpenPortOption
  , cancelReader
  , getReader
  , getWriter
  , read
  , write
  , openPort
  , closePort
  , forgetPort
  , requestPort
  , releaseLockReader
  , releaseLockWriter
  ) where

import Prelude
import Control.Promise (Promise, toAffE)
import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import data SerialPort :: Type

foreign import data Reader :: Type

foreign import cancelReaderImpl :: EffectFn1 Reader (Promise Unit)

cancelReader :: Reader -> Aff Unit
cancelReader reader = runEffectFn1 cancelReaderImpl reader # toAffE

foreign import releaseLockReaderImpl :: EffectFn1 Reader Unit

releaseLockReader :: Reader -> Effect Unit
releaseLockReader = runEffectFn1 releaseLockReaderImpl

foreign import getReaderImpl :: EffectFn1 SerialPort Reader

getReader :: SerialPort -> Effect Reader
getReader = runEffectFn1 getReaderImpl

foreign import readImpl :: EffectFn1 Reader (Promise { done :: Boolean, value :: Uint8Array })

data ReadResult
  = Chunk Uint8Array
  | Closed

read :: Reader -> Aff ReadResult
read reader = do
  { done: done, value: value } <- runEffectFn1 readImpl reader # toAffE
  pure $ if done then Closed else Chunk value

foreign import data Writer :: Type

foreign import releaseLockWriterImpl :: EffectFn1 Writer Unit

releaseLockWriter :: Writer -> Effect Unit
releaseLockWriter = runEffectFn1 releaseLockWriterImpl

foreign import getWriterImpl :: EffectFn1 SerialPort Writer

getWriter :: SerialPort -> Effect Writer
getWriter = runEffectFn1 getWriterImpl

foreign import writeImpl :: EffectFn2 Writer Uint8Array (Promise Unit)

write :: Writer -> Uint8Array -> Aff Unit
write writer chunk = runEffectFn2 writeImpl writer chunk # toAffE

foreign import requestPortImpl :: Effect (Promise SerialPort)

requestPort :: Aff SerialPort
requestPort = requestPortImpl # toAffE

data DataBits
  = DataBits7
  | DataBits8

data FlowControl
  = FlowControlNone
  | FlowControlHardware

data Parity
  = ParityNone
  | ParityEven
  | ParityOdd

data StopBits
  = StopBits1
  | StopBits2

type OpenPortOption
  = { baudRate :: Int
    , bufferSize :: Int
    , dataBits :: DataBits
    , flowControl :: FlowControl
    , parity :: Parity
    , stopBits :: StopBits
    }

type ForeignOpenPortOption
  = { baudRate :: Int
    , bufferSize :: Int
    , dataBits :: Int
    , flowControl :: String
    , parity :: String
    , stopBits :: Int
    }

fromOpenPortOption :: OpenPortOption -> ForeignOpenPortOption
fromOpenPortOption option =
  { baudRate: option.baudRate
  , bufferSize: option.bufferSize
  , dataBits:
      case option.dataBits of
        DataBits7 -> 7
        DataBits8 -> 8
  , flowControl:
      case option.flowControl of
        FlowControlNone -> "none"
        FlowControlHardware -> "hardware"
  , parity:
      case option.parity of
        ParityNone -> "none"
        ParityEven -> "even"
        ParityOdd -> "Odd"
  , stopBits:
      case option.stopBits of
        StopBits1 -> 1
        StopBits2 -> 2
  }

foreign import openPortImpl :: EffectFn2 SerialPort ForeignOpenPortOption (Promise Unit)

openPort :: SerialPort -> OpenPortOption -> Aff Unit
openPort serialport openportoption =
  runEffectFn2 openPortImpl serialport (fromOpenPortOption openportoption)
    # toAffE

defaultOpenPortOption :: OpenPortOption
defaultOpenPortOption =
  { baudRate: 115200
  , bufferSize: 255
  , dataBits: DataBits8
  , flowControl: FlowControlNone
  , parity: ParityNone
  , stopBits: StopBits1
  }

foreign import closePortImpl :: EffectFn1 SerialPort (Promise Unit)

closePort :: SerialPort -> Aff Unit
closePort serialport = runEffectFn1 closePortImpl serialport # toAffE

foreign import forgetPortImpl :: EffectFn1 SerialPort (Promise Unit)

forgetPort :: SerialPort -> Aff Unit
forgetPort serialport = runEffectFn1 forgetPortImpl serialport # toAffE
