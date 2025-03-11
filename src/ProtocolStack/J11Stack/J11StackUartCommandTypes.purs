{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module ProtocolStack.J11StackUart.Command.Types
  ( CommandCode
  , J11Command(..)
  , J11CommandFormat
  , J11CommandHeader
  , J11RequestCommandFormat(..)
  , UniqueCode
  , fromJ11CommandFormat
  , makeJ11Command
  , printJ11CommandFormat
  , toStringJ11Command
  ) where

import Prelude
import Control.Monad.Loops (unfoldrM)
import Data.Array as Array
import Data.ArrayBuffer.Cast as Cast
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as ArrayBufferTyped
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt, (.&.))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Utility as Utility

-- | J11 UART コマンド
newtype J11Command
  = J11Command (Array UInt)

derive instance newtypeJ11Command :: Newtype J11Command _

instance showJ11Command :: Show J11Command where
  show command = "J11Command [" <> toStringJ11Command command <> "]"

makeJ11Command :: Uint8Array -> Effect J11Command
makeJ11Command arr = J11Command <$> ArrayBufferTyped.toArray arr

fromJ11CommandFormat :: J11CommandFormat -> Effect J11Command
fromJ11CommandFormat x = do
  dataBody <- ArrayBufferTyped.toArray <=< Cast.toUint8Array $ DV.whole (x.dataBody)
  pure
    $ J11Command
    $ Array.concat
        [ [ {- byte# 0 -} x.uniqueCode `UInt.shr` (UInt.fromInt 24) .&. UInt.fromInt 0xff
          , {- byte# 1 -} x.uniqueCode `UInt.shr` (UInt.fromInt 16) .&. UInt.fromInt 0xff
          , {- byte# 2 -} x.uniqueCode `UInt.shr` (UInt.fromInt 8) .&. UInt.fromInt 0xff
          , {- byte# 3 -} x.uniqueCode .&. UInt.fromInt 0xff
          , {- byte# 4 -} x.commandCode `UInt.shr` (UInt.fromInt 8) .&. UInt.fromInt 0xff
          , {- byte# 5 -} x.commandCode .&. UInt.fromInt 0xff
          , {- byte# 6 -} x.messageLen `UInt.shr` (UInt.fromInt 8) .&. UInt.fromInt 0xff
          , {- byte# 7 -} x.messageLen .&. UInt.fromInt 0xff
          , {- byte# 8 -} x.headerChecksum `UInt.shr` (UInt.fromInt 8) .&. UInt.fromInt 0xff
          , {- byte# 9 -} x.headerChecksum .&. UInt.fromInt 0xff
          , {- byte#10 -} x.dataChecksum `UInt.shr` (UInt.fromInt 8) .&. UInt.fromInt 0xff
          , {- byte#11 -} x.dataChecksum .&. UInt.fromInt 0xff
          ]
        , dataBody
        ]

toStringJ11Command :: J11Command -> String
toStringJ11Command (J11Command command) = String.joinWith " " $ map (Utility.toStringHexAs Utility.octet) command

type UniqueCode
  = UInt

type CommandCode
  = UInt

type J11CommandHeader
  = { uniqueCode :: UniqueCode
    , commandCode :: CommandCode
    , messageLen :: UInt
    , headerChecksum :: UInt
    , dataChecksum :: UInt
    }

type J11CommandFormat
  = { uniqueCode :: UniqueCode
    , commandCode :: CommandCode
    , messageLen :: UInt
    , headerChecksum :: UInt
    , dataChecksum :: UInt
    , dataBody :: ArrayBuffer
    }

printJ11CommandFormat :: J11CommandFormat -> Effect String
printJ11CommandFormat format = do
  dataBody <- getDataBody
  pure $ "{"
    <> ("uniqueCode: " <> Utility.toStringHexAs Utility.dword format.uniqueCode)
    <> (",commandCode: " <> Utility.toStringHexAs Utility.word format.commandCode)
    <> (",messageLen: " <> Utility.toStringHexAs Utility.word format.messageLen)
    <> (",headerChecksum: " <> Utility.toStringHexAs Utility.word format.headerChecksum)
    <> (",dataChecksum: " <> Utility.toStringHexAs Utility.word format.dataChecksum)
    <> (",dataBody: " <> joinWith " " dataBody)
    <> "}"
  where
  getDataBody :: Effect (Array String)
  getDataBody =
    unfoldrM
      ( \byteOffset -> do
          maybeUint <- DV.getUint8 (DV.whole format.dataBody) byteOffset
          pure
            $ case maybeUint of
                Just n -> Just $ Utility.toStringHexAs Utility.octet n /\ (byteOffset + 1)
                Nothing -> Nothing
      )
      0

newtype J11RequestCommandFormat
  = J11RequestCommandFormat J11CommandFormat

derive instance newtypeJ11RequestCommandFormat :: Newtype J11RequestCommandFormat _

instance showJ11RequestCommandFormat :: Show J11RequestCommandFormat where
  show (J11RequestCommandFormat a) = "J11RequestCommandFormat " <> (unsafePerformEffect $ printJ11CommandFormat a)
