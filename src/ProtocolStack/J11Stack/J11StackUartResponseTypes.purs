{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module ProtocolStack.J11StackUart.Response.Types
  ( ActivescanAppendix
  , Beacon
  , CommandResponse(..)
  , FirmwareVersion(..)
  , J11ResponseCommandFormat(..)
  , Notify(..)
  , PanaCert
  , PanaResult(..)
  , ReceivedData(..)
  , ResponseError(..)
  , StartBRouteAppendix
  , toStringBeacon
  , toStringCommandResponse
  , toStringFirmwareId
  , toStringFirmwareVersion
  , toStringNotify
  , toStringReceivedData
  , toStringStartBRouteAppendix
  ) where

import Prelude
import Data.ArrayBuffer.Cast as Cast
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as ArrayBufferTyped
import Data.ArrayBuffer.Types (ArrayBuffer, DataView)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import ProtocolStack.J11StackUart.Command.Types (J11CommandFormat, printJ11CommandFormat)
import URI.Host.IPv6Address (IPv6Address)
import URI.Host.IPv6Address as IPv6Address
import Utility as Utility

newtype J11ResponseCommandFormat
  = J11ResponseCommandFormat J11CommandFormat

derive instance newtypeJ11ResponseCommandFormat :: Newtype J11ResponseCommandFormat _

instance showJ11ResponseCommandFormat :: Show J11ResponseCommandFormat where
  show (J11ResponseCommandFormat a) = "J11ResponseCommandFormat " <> (unsafePerformEffect $ printJ11CommandFormat a)

newtype FirmwareVersion
  = FirmwareVersion { firmwareId :: UInt, versionMajor :: UInt, versionMinor :: UInt, revision :: UInt }

derive instance newtypeFirmwareVersion :: Newtype FirmwareVersion _

instance showFirmwareVersion :: Show FirmwareVersion where
  show a = "FirmwareVersion " <> toStringFirmwareVersion a

toStringFirmwareId :: FirmwareVersion -> String
toStringFirmwareId (FirmwareVersion a) = case (UInt.toInt a.firmwareId) of
  0x0400 -> "Wi-SUN Enhanced HAN Plus B-route Dual stack"
  _ -> "Unknown firmware id:" <> Utility.toStringHexAs Utility.dword a.firmwareId

toStringFirmwareVersion :: FirmwareVersion -> String
toStringFirmwareVersion (FirmwareVersion a) =
  UInt.toString a.versionMajor
    <> "."
    <> UInt.toString a.versionMinor
    <> ".Rev "
    <> Utility.toStringHexAs Utility.dword a.revision

type StartBRouteAppendix
  = { channel :: UInt, panId :: UInt, macAddress :: Array UInt, rssi :: Int }

toStringStartBRouteAppendix :: StartBRouteAppendix -> String
toStringStartBRouteAppendix a =
  "チャンネル番号:"
    <> UInt.toString a.channel
    <> ",PAN ID:"
    <> UInt.toString a.panId
    <> ",MACアドレス:["
    <> (String.joinWith ":" $ map (Utility.toStringHexAs Utility.octet) a.macAddress)
    <> "],RSSI:"
    <> Int.toStringAs Int.decimal a.rssi

type Beacon
  = { scanCount :: UInt, macAddress :: Array UInt, panId :: UInt, rssi :: Int }

toStringBeacon :: Beacon -> String
toStringBeacon beacon =
  "スキャン数:"
    <> UInt.toString beacon.scanCount
    <> ",MACアドレス:["
    <> (String.joinWith ":" $ map (Utility.toStringHexAs Utility.octet) beacon.macAddress)
    <> "],PAN ID:"
    <> UInt.toString beacon.panId
    <> ",RSSI:"
    <> Int.toStringAs Int.decimal beacon.rssi

data CommandResponse
  = CommandResponseUnknownCommandCode UInt
  | CommandResponseSuccess
  | CommandResponseFail UInt
  | CommandResponseFirmwareVersion FirmwareVersion
  | CommandResponseStartBRoute StartBRouteAppendix

derive instance genericCommandResponse :: Generic CommandResponse _

instance showCommandResponse :: Show CommandResponse where
  show = genericShow

toStringCommandResponse :: CommandResponse -> String
toStringCommandResponse = case _ of
  CommandResponseUnknownCommandCode n -> "コマンドコード不明(0x" <> Utility.toStringHexAs Utility.word n <> ")"
  CommandResponseSuccess -> "成功"
  CommandResponseFail n -> failure n
  CommandResponseFirmwareVersion v -> "ファームウェアバージョン: " <> toStringFirmwareVersion v
  CommandResponseStartBRoute a -> "Bルート動作開始: " <> toStringStartBRouteAppendix a
  where
  failure n
    | n == UInt.fromInt 0x01 = "成功"
    | n == UInt.fromInt 0x02 = "指定したアドレスがデバイスリストに存在しない"
    | n == UInt.fromInt 0x03 = "コマンドコード不正"
    | n == UInt.fromInt 0x04 = "パラメータ値不正"
    -- 0x05は無い
    | n == UInt.fromInt 0x06 = "宛先不正による送信エラー"
    | n == UInt.fromInt 0x0e = "MAC 接続失敗"
    | n == UInt.fromInt 0x13 = "コマンド受信エラー。データ受信タイムアウト（1 秒）"
    | n == UInt.fromInt 0xf0 = "コマンド受信エラー。ヘッダチェックサムエラー"
    | n == UInt.fromInt 0xf1 = "コマンド受信エラー。データチェックサムエラー"
    | n == UInt.fromInt 0xf2 = "コマンド受信エラー。ヘッダで指定されたメッセージ長が短い"
    | n == UInt.fromInt 0xf3 = "コマンド受信エラー。ヘッダで指定されたメッセージ長が最大長オーバー"
    | otherwise = "失敗 0x" <> Utility.toStringHexAs Utility.octet n

type ActivescanAppendix
  = { channel :: UInt, beacon :: Maybe Beacon }

newtype ReceivedData
  = ReceivedData
  { senderIpv6 :: IPv6Address
  , senderPort :: UInt
  , destinationPort :: UInt
  , senderPanId :: UInt
  , senderAddressType :: UInt
  , encrypted :: UInt
  , rssi :: UInt
  , dataLength :: UInt
  , data :: ArrayBuffer
  }

derive instance newtypeReceivedData :: Newtype ReceivedData _

instance showReceivedData :: Show ReceivedData where
  show = unsafePerformEffect <<< toStringReceivedData

toStringReceivedData :: ReceivedData -> Effect String
toStringReceivedData (ReceivedData x) = do
  u8array <- toUint8Array (DV.whole x.data)
  pure $ "{senderIpv6:"
    <> IPv6Address.unsafeToString x.senderIpv6
    <> ",senderPort:"
    <> (Int.toStringAs Int.decimal $ UInt.toInt x.senderPort)
    <> ",destinationPort:"
    <> (Int.toStringAs Int.decimal $ UInt.toInt x.destinationPort)
    <> ",senderPanId:"
    <> (Int.toStringAs Int.decimal $ UInt.toInt x.senderPanId)
    <> ",senderAddressType:"
    <> (Int.toStringAs Int.decimal $ UInt.toInt x.senderAddressType)
    <> ",encrypted:"
    <> (Int.toStringAs Int.decimal $ UInt.toInt x.encrypted)
    <> ",rssi:"
    <> (Int.toStringAs Int.decimal $ UInt.toInt x.rssi)
    <> ",dataLength:"
    <> (Int.toStringAs Int.decimal $ UInt.toInt x.dataLength)
    <> ",data:["
    <> (String.joinWith "," $ map (Utility.toStringHexAs Utility.octet) u8array)
    <> "]"
  where
  toUint8Array :: DataView -> Effect (Array UInt)
  toUint8Array dv = ArrayBufferTyped.toArray =<< Cast.toUint8Array dv

data PanaResult
  = PanaSuccess
  | PanaFailure
  | PanaNoResponse

derive instance genericPanaResult :: Generic PanaResult _

instance showPanaResult :: Show PanaResult where
  show PanaSuccess = "PANA認証: 成功"
  show PanaFailure = "PANA認証: 失敗"
  show PanaNoResponse = "PANA認証: 応答なし"

type PanaCert
  = { result :: PanaResult, macAddress :: Array UInt }

data Notify
  = NotifyUnknownCommandCode Int
  | NotifyBootCompleted
  | NotifyActivescan ActivescanAppendix
  | NotifyPanaCert PanaCert
  | NotifyReceivedData ReceivedData

derive instance genericNotify :: Generic Notify _

instance showNotify :: Show Notify where
  show = genericShow

toStringNotify :: Notify -> String
toStringNotify = case _ of
  NotifyUnknownCommandCode n -> "通知コード不明(0x" <> Utility.toStringHexAs Utility.octet (UInt.fromInt n) <> ")"
  NotifyBootCompleted -> "起動完了"
  NotifyActivescan a -> case a.beacon of
    Nothing -> "アクティブスキャン応答無し。" <> "チャネル番号" <> Int.toStringAs Int.decimal (UInt.toInt a.channel)
    Just beacon ->
      "アクティブスキャン応答あり。"
        <> "チャネル番号"
        <> Int.toStringAs Int.decimal (UInt.toInt a.channel)
        <> ","
        <> toStringBeacon beacon
  NotifyPanaCert cert ->
    show cert.result
      <> ", MACアドレス:["
      <> (String.joinWith ":" $ map (Utility.toStringHexAs Utility.octet) cert.macAddress)
      <> "]"
  NotifyReceivedData arrival -> show arrival

data ResponseError
  = ChecksumError
  | IlligalCommandError

instance showResponseError :: Show ResponseError where
  show ChecksumError = "エラー: チェックサム不一致"
  show IlligalCommandError = "エラー: 不正なコマンドコード"
