{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module ProtocolStack.J11StackUart.Command
  ( calculateChecksum
  , commandActivescan
  , commandBRouteStart
  , commandBRouteStartPana
  , commandBRouteTerminatePana
  , commandCodeGetFirmwareVersion
  , commandGetFirmwareVersion
  , commandHardwareReset
  , commandInitialSetup
  , commandSetPanaAuthInfo
  , commandTransmitData
  , commandUdpPortOpen
  , uniqueCodeRequestCommand
  , uniqueCodeResponseCommand
  ) where

import Prelude
import Data.Array as Array
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.Cast as Cast
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as ArrayBufferTyped
import Data.ArrayBuffer.Types (DataView)
import Data.Enum (fromEnum)
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodePoints as CodePoints
import Data.Traversable (traverse_)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import ProtocolStack.J11StackUart.Command.Types (CommandCode, J11CommandFormat, UniqueCode)
import URI.Host.IPv6Address (IPv6Address)
import URI.Host.IPv6Address as IPv6Address
import Utility as Utility

-- ユニークコード(要求コマンド)
uniqueCodeRequestCommand ∷ UniqueCode
uniqueCodeRequestCommand = UInt.shl (UInt.fromInt 0xd0ea) (UInt.fromInt 16) + UInt.fromInt 0x83fc

-- ユニークコード(応答/通知コマンド)
uniqueCodeResponseCommand ∷ UniqueCode
uniqueCodeResponseCommand = UInt.shl (UInt.fromInt 0xd0f9) (UInt.fromInt 16) + UInt.fromInt 0xee5d

-- チェックサム算出
calculateChecksum :: DataView -> Effect UInt
calculateChecksum dataview = do
  uint8array <- Cast.toUint8Array dataview
  ArrayBufferTyped.foldl sum (UInt.fromInt 0) uint8array
  where
  -- 0xffffを超えた値を無視して各々バイトを足し合わせる
  sum acc n = (acc + n) `UInt.and` (UInt.fromInt 0xffff)

-- コマンドコード(ファームウェアバージョン取得)
commandCodeGetFirmwareVersion ∷ CommandCode
commandCodeGetFirmwareVersion = UInt.fromInt 0x006b

-- ファームウェアバージョン取得コマンド
commandGetFirmwareVersion :: J11CommandFormat
commandGetFirmwareVersion =
  { uniqueCode: uniqueCodeRequestCommand
  , commandCode: UInt.fromInt 0x006b
  , messageLen: UInt.fromInt 0x0004
  , headerChecksum: UInt.fromInt 0x03a8
  , dataChecksum: UInt.fromInt 0x0000
  , dataBody: Utility.zeroSizeArrayBuffer
  }

-- ハードウェアリセットコマンド
commandHardwareReset :: J11CommandFormat
commandHardwareReset =
  { uniqueCode: uniqueCodeRequestCommand
  , commandCode: UInt.fromInt 0x00d9
  , messageLen: UInt.fromInt 0x0004
  , headerChecksum: UInt.fromInt 0x0000
  , dataChecksum: UInt.fromInt 0x0000
  , dataBody: Utility.zeroSizeArrayBuffer
  }

-- 初期設定要求コマンド
commandInitialSetup :: UInt -> J11CommandFormat
commandInitialSetup channel =
  { uniqueCode: uniqueCodeRequestCommand
  , commandCode: UInt.fromInt 0x005f
  , messageLen: UInt.fromInt 0x0008
  , headerChecksum: UInt.fromInt 0x0000
  , dataChecksum: UInt.fromInt 0x0000
  , dataBody:
      unsafePerformEffect
        $ Builder.execPut do
            Builder.putUint8 $ UInt.fromInt 0x05
            Builder.putUint8 $ UInt.fromInt 0x00
            Builder.putUint8 channel
            Builder.putUint8 $ UInt.fromInt 0x00
  }

-- PANA認証情報設定コマンド
commandSetPanaAuthInfo :: String -> String -> Effect J11CommandFormat
commandSetPanaAuthInfo routeBId routeBPassword = do
  dataBody <-
    Builder.execPut do
      traverse_ Builder.putUint8 $ Array.take 32 asciiCode.id -- 認証ID(32バイト)
      traverse_ Builder.putUint8 $ Array.take 12 asciiCode.password -- 認証パスワード(12バイト)
  dataChecksum <- calculateChecksum (DV.whole dataBody)
  pure
    { uniqueCode: uniqueCodeRequestCommand
    , commandCode: UInt.fromInt 0x0054
    , messageLen: UInt.fromInt 0x0030
    , headerChecksum: UInt.fromInt 0x03bd
    , dataChecksum: dataChecksum
    , dataBody: dataBody
    }
  where
  asciiCode :: { id :: Array UInt, password :: Array UInt }
  asciiCode =
    { id: map (UInt.fromInt <<< fromEnum) $ CodePoints.toCodePointArray routeBId
    , password: map (UInt.fromInt <<< fromEnum) $ CodePoints.toCodePointArray routeBPassword
    }

-- Bルート動作開始要求コマンド
commandBRouteStart :: J11CommandFormat
commandBRouteStart =
  { uniqueCode: uniqueCodeRequestCommand
  , commandCode: UInt.fromInt 0x0053
  , messageLen: UInt.fromInt 0x0004
  , headerChecksum: UInt.fromInt 0x0390
  , dataChecksum: UInt.fromInt 0x0000
  , dataBody: Utility.zeroSizeArrayBuffer
  }

-- アクティブスキャン実行要求コマンド
commandActivescan :: UInt -> String -> Effect J11CommandFormat
commandActivescan scanDuration routebId = do
  dataBody <-
    Builder.execPut do
      Builder.putUint8 scanDuration -- スキャン時間(1バイト)
      Builder.putUint32be (UInt.fromInt 0x0003fff0) -- スキャンチャネル4,5,6指定(4バイト)
      Builder.putUint8 $ UInt.fromInt 0x01 -- ID設定(1バイト)
      traverse_ Builder.putUint8 $ Array.takeEnd 8 routebIdCode -- Ｂルート認証IDの最後8文字(8バイト)
  dataChecksum <- calculateChecksum (DV.whole dataBody)
  pure
    { uniqueCode: uniqueCodeRequestCommand
    , commandCode: UInt.fromInt 0x0051
    , messageLen: UInt.fromInt 0x0012
    , headerChecksum: UInt.fromInt 0x039c
    , dataChecksum: dataChecksum
    , dataBody: dataBody
    }
  where
  routebIdCode :: Array UInt
  routebIdCode = map (UInt.fromInt <<< fromEnum) $ CodePoints.toCodePointArray routebId

-- UDPポートオープン要求コマンド
commandUdpPortOpen :: J11CommandFormat
commandUdpPortOpen =
  { uniqueCode: uniqueCodeRequestCommand
  , commandCode: UInt.fromInt 0x0005
  , messageLen: UInt.fromInt 0x0006
  , headerChecksum: UInt.fromInt 0x0344
  , dataChecksum: UInt.fromInt 0x0028
  , dataBody:
      unsafePerformEffect
        $ Builder.execPut do
            Builder.putInt16be 0x0E1A
  }

-- BルートPANA開始要求コマンド
commandBRouteStartPana :: J11CommandFormat
commandBRouteStartPana =
  { uniqueCode: uniqueCodeRequestCommand
  , commandCode: UInt.fromInt 0x0056
  , messageLen: UInt.fromInt 0x0004
  , headerChecksum: UInt.fromInt 0x0393
  , dataChecksum: UInt.fromInt 0x0000
  , dataBody: Utility.zeroSizeArrayBuffer
  }

-- BルートPANA終了要求コマンド
commandBRouteTerminatePana :: J11CommandFormat
commandBRouteTerminatePana =
  { uniqueCode: uniqueCodeRequestCommand
  , commandCode: UInt.fromInt 0x0057
  , messageLen: UInt.fromInt 0x0004
  , headerChecksum: UInt.fromInt 0x0394
  , dataChecksum: UInt.fromInt 0x0000
  , dataBody: Utility.zeroSizeArrayBuffer
  }

-- データ送信要求コマンド
commandTransmitData :: IPv6Address -> DataView -> Effect J11CommandFormat
commandTransmitData destination payload = do
  let
    -- IPv6アドレスをUInt型に変える
    destinationAddress = Maybe.fromMaybe [] (maybeUIntAddress =<< maybeStrAddress destination)
  -- データ部
  dataBody <-
    Builder.execPut do
      traverse_ Builder.putUint16be destinationAddress -- 送信元IPv6アドレス(16バイト)
      Builder.putUint16be $ UInt.fromInt 0x0e1a -- 送信元ポート番号(2バイト)
      Builder.putUint16be $ UInt.fromInt 0x0e1a -- 送信先ポート番号(2バイト)
      Builder.putUint16be (UInt.fromInt $ DV.byteLength payload) -- 送信データ長(2バイト)
      Builder.putDataView payload -- 送信データ(任意バイト)
  let
    -- データ長
    dataLength = ArrayBuffer.byteLength dataBody

    -- メッセージ長
    messageLen = 4 + dataLength
  -- ヘッダ部チェックサム算出
  headerChecksum <-
    calculateChecksum <<< DV.whole
      =<< ( Builder.execPut do
            Builder.putUint32be uniqueCode
            Builder.putUint16be commandCode
            Builder.putInt16be messageLen
        )
  -- データ部チェックサム算出
  dataChecksum <- calculateChecksum (DV.whole dataBody)
  --
  pure
    { uniqueCode: uniqueCode
    , commandCode: commandCode
    , messageLen: UInt.fromInt messageLen
    , headerChecksum: headerChecksum
    , dataChecksum: dataChecksum
    , dataBody: dataBody
    }
  where
  uniqueCode = uniqueCodeRequestCommand

  commandCode = UInt.fromInt 0x0008

  maybeStrAddress :: IPv6Address -> Maybe (Array String)
  maybeStrAddress ipv6 =
    ipv6 # IPv6Address.unsafeToString
      # (String.stripPrefix (String.Pattern "[") >=> String.stripSuffix (String.Pattern "]"))
      <#> String.split (String.Pattern ":")

  maybeUIntAddress :: Array String -> Maybe (Array UInt)
  maybeUIntAddress strAddress =
    Utility.unwrapAllMaybes
      $ map (\x -> UInt.fromInt <$> Int.fromStringAs Int.hexadecimal x) strAddress
