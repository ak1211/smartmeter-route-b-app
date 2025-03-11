{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module ProtocolStack.J11StackUart.Response
  ( ParseReceiveDataResult
  , Response
  , fromJ11ResponseCommandFormat
  , parseReceiveData
  , toStringResponse
  ) where

import Prelude
import Control.Alternative (guard)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (DataView)
import Data.Either (Either)
import Data.Either.Nested (Either3)
import Data.Either.Nested as Either
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Parsing as P
import Parsing.Combinators ((<?>))
import Parsing.DataView as PDV
import ProtocolStack.J11StackUart.Command (calculateChecksum, uniqueCodeResponseCommand, uniqueCodeRequestCommand)
import ProtocolStack.J11StackUart.Command.Types (J11CommandHeader)
import ProtocolStack.J11StackUart.Response.Types as R
import URI.Host.IPv6Address as IPv6Address
import Utility as Utility

type ParseReceiveDataResult
  = { positionIndex :: Int -- この位置までパーサーが消費した
    , responseCommandFormat :: R.J11ResponseCommandFormat -- 応答
    }

parseReceiveData :: forall m. MonadRec m => MonadEffect m => DataView -> m (Either P.ParseError ParseReceiveDataResult)
parseReceiveData dataview =
  P.runParserT dataview do
    -- anyTillで先頭のごみデータを無視しながらヘッダが読み取れるまで探す
    _ /\ header <- PDV.anyTill takeHeader <?> "header"
    -- データ部読み取り
    dataBody <- PDV.takeN (UInt.toInt header.messageLen - 4) <?> "data body"
    -- データ部チェックサムの検査
    dataChecksum <- lift <<< liftEffect $ calculateChecksum dataBody
    guard (header.dataChecksum == dataChecksum) <?> "data checksum mismatch"
    --
    -- ここで読み取り完了
    --
    -- 現在位置を取得する
    (P.Position pos) <- P.position
    let
      start = DV.byteOffset dataBody

      end = start + DV.byteLength dataBody

      responseCommandFormat =
        R.J11ResponseCommandFormat
          { uniqueCode: header.uniqueCode
          , commandCode: header.commandCode
          , messageLen: header.messageLen
          , headerChecksum: header.headerChecksum
          , dataChecksum: header.dataChecksum
          , dataBody: ArrayBuffer.slice start end (DV.buffer dataBody)
          }
    pure { positionIndex: pos.index, responseCommandFormat: responseCommandFormat }
  where
  -- ヘッダを読み込む
  takeHeader :: P.ParserT DataView m J11CommandHeader
  takeHeader = do
    -- ユニークコード読み取り(4 バイト)
    uniqueCode <- PDV.satisfyUint32be (\x -> x == uniqueCodeRequestCommand || x == uniqueCodeResponseCommand)
    -- 応答コード読み取り(2 バイト)
    commandCode <- PDV.anyUint16be <?> "response command code"
    -- メッセージ長読み取り(2 バイト)
    messageLen <- PDV.anyUint16be <?> "message length"
    -- ヘッダ部チェックサム読み取り(2 バイト)
    headerChecksum <- PDV.anyUint16be <?> "header checksum"
    -- データ部チェックサム読み取り(2 バイト)
    dataChecksum <- PDV.anyUint16be <?> "data checksum"
    -- ここまで12バイト
    buffer <-
      lift <<< liftEffect
        $ Builder.execPut do
            Builder.putUint32be uniqueCode
            Builder.putUint16be commandCode
            Builder.putUint16be messageLen
    -- ヘッダ部チェックサムの検査
    checksum <- lift <<< liftEffect $ calculateChecksum (DV.whole buffer)
    guard (checksum == headerChecksum) <?> "header checksum mismatch"
    pure
      { uniqueCode: uniqueCode
      , commandCode: commandCode
      , messageLen: messageLen
      , headerChecksum: headerChecksum
      , dataChecksum: dataChecksum
      }

type Response
  = Either3 R.ResponseError R.CommandResponse R.Notify

toStringResponse :: Response -> String
toStringResponse = Either.either3 show R.toStringCommandResponse R.toStringNotify

fromJ11ResponseCommandFormat :: R.J11ResponseCommandFormat -> Effect Response
fromJ11ResponseCommandFormat = go
  where
  isCommandResponse code = UInt.fromInt 0x2000 <= code && code <= UInt.fromInt 0x2fff

  go commandFormat@(R.J11ResponseCommandFormat response)
    | response.commandCode == (UInt.fromInt 0x2fff) = pure $ Either.in1 R.ChecksumError -- ヘッダ部チェックサム不正
    | response.commandCode == (UInt.fromInt 0xffff) = pure $ Either.in1 R.IlligalCommandError -- 不正なコマンドコード
    | isCommandResponse response.commandCode = getCommandResponse commandFormat
    | otherwise = getNotify commandFormat

getCommandResponse :: R.J11ResponseCommandFormat -> Effect Response
getCommandResponse (R.J11ResponseCommandFormat response) = do
  -- 応答結果を確認する
  maybeResultCode <- liftEffect $ DV.getUint8 dataview 0
  case maybeResultCode of
    Nothing -> pure $ Either.in1 R.ChecksumError -- 応答結果が無いのはチェックサム不一致で検出すべき
    Just rc
      | rc == (UInt.fromInt 0x01) -> go (UInt.toInt response.commandCode) -- 成功
      | otherwise -> pure $ Either.in2 $ R.CommandResponseFail rc -- 失敗
  where
  dataview = DV.whole response.dataBody

  --
  go :: Int -> Effect Response
  -- 0x2005 UDPポートオープン応答
  go 0x2005 = pure $ Either.in2 R.CommandResponseSuccess

  -- 0x2008 データ送信応答
  go 0x2008 = pure $ Either.in2 R.CommandResponseSuccess

  -- 0x2051 アクティブスキャン応答
  go 0x2051 = pure $ Either.in2 R.CommandResponseSuccess

  -- 0x2053 Ｂルート動作開始応答
  go 0x2053 = do
    -- データ部の#1バイトがチャネル
    maybeChannel <- liftEffect $ DV.getUint8 dataview 1
    -- データ部の#2,#3バイトがPAN ID
    maybePanId <- liftEffect $ DV.getUint16be dataview 2
    -- データ部の#4,#5,#6,#7,#8,#9,#10,#11バイトがMACアドレス
    maybeMacAddress <- liftEffect $ traverse (DV.getUint8 dataview) $ Array.range 4 11
    -- データ部の#12がRSSI
    maybeRssi <- liftEffect $ DV.getInt8 dataview 12
    let
      (maybeAppendix :: Maybe R.StartBRouteAppendix) =
        { channel: _, panId: _, macAddress: _, rssi: _ }
          <$> maybeChannel
          <*> maybePanId
          <*> Utility.unwrapAllMaybes maybeMacAddress
          <*> maybeRssi
    -- データ数不足はChecksumErrorを返却する
    pure $ maybe (Either.in1 R.ChecksumError) (Either.in2 <<< R.CommandResponseStartBRoute) maybeAppendix

  -- 0x2054 ＢルートPANA認証情報設定応答
  go 0x2054 = pure $ Either.in2 R.CommandResponseSuccess

  -- 0x2056 ＢルートPANA開始要求応答
  go 0x2056 = pure $ Either.in2 R.CommandResponseSuccess

  -- 0x2057 ＢルートPANA終了要求応答
  go 0x2057 = pure $ Either.in2 R.CommandResponseSuccess

  -- 0x205f 初期設定応答
  go 0x205f = pure $ Either.in2 R.CommandResponseSuccess

  -- 0x206b ファームウェアバージョン応答
  go 0x206b = do
    -- データ部の#1,#2バイトがファームウェアID
    maybeId <- liftEffect $ DV.getUint16be dataview 1
    -- データ部の#3バイトがメジャーバージョン
    maybeVersionMajor <- liftEffect $ DV.getUint8 dataview 3
    -- データ部の#4バイトがマイナーバージョン
    maybeVersionMinor <- liftEffect $ DV.getUint8 dataview 4
    -- データ部の#5,#6,#7,#8バイトがリビジョン
    maybeRevision <- liftEffect $ DV.getUint32be dataview 5
    --
    let
      (maybeFirmwareVersion :: Maybe R.FirmwareVersion) =
        R.FirmwareVersion
          <$> ( { firmwareId: _, versionMajor: _, versionMinor: _, revision: _ }
                <$> maybeId
                <*> maybeVersionMajor
                <*> maybeVersionMinor
                <*> maybeRevision
            )
    -- データ数不足はChecksumErrorを返却する
    pure $ maybe (Either.in1 R.ChecksumError) (Either.in2 <<< R.CommandResponseFirmwareVersion) maybeFirmwareVersion

  -- よくわからない応答コード
  go n = pure $ Either.in2 $ R.CommandResponseUnknownCommandCode (UInt.fromInt n)

getNotify :: R.J11ResponseCommandFormat -> Effect Response
getNotify (R.J11ResponseCommandFormat response) = go (UInt.toInt response.commandCode)
  where
  dataview = DV.whole response.dataBody

  go :: Int -> Effect Response
  -- 0x4051 アクティブスキャン結果通知
  go 0x4051 = do
    -- データ部の#0バイトがスキャン結果
    -- データ部の#1バイトがチャネル
    rc <- liftEffect $ traverse (DV.getUint8 dataview) $ Array.range 0 1
    case (Utility.unwrapAllMaybes rc) of
      (Just [ result, channel ])
        | result == UInt.fromInt 0x01 -> do -- Beacon応答なし
          pure $ Either.in3
            $ R.NotifyActivescan { channel: channel, beacon: Nothing }
        | result == UInt.fromInt 0x00 -> do -- Beacon応答あり
          -- データ部の#2バイトがスキャン数
          maybeScanCount <- liftEffect $ DV.getUint8 dataview 2
          -- データ部の#3,#4,#5,#6,#7,#8,#9,#10バイトがMACアドレス
          maybeMacAddress <- liftEffect $ traverse (DV.getUint8 dataview) $ Array.range 3 10
          -- データ部の#11,#12バイトがPAN ID
          maybePanId <- liftEffect $ DV.getUint16be dataview 11
          -- データ部の#13バイトがRSSI
          maybeRssi <- liftEffect $ DV.getInt8 dataview 13
          --
          let
            (maybeBeacon :: Maybe R.Beacon) =
              { scanCount: _, macAddress: _, panId: _, rssi: _ }
                <$> maybeScanCount
                <*> Utility.unwrapAllMaybes maybeMacAddress
                <*> maybePanId
                <*> maybeRssi
          pure $ Either.in3
            $ R.NotifyActivescan
            $ { channel: channel, beacon: _ } maybeBeacon
        | otherwise -> pure $ Either.in1 R.ChecksumError -- 0または1のみなのでこの分岐は無いはず。ChecksumErrorを返却する
      _ -> pure $ Either.in1 R.ChecksumError -- データ数不足はChecksumErrorを返却する

  -- 0x6018 データ受信通知
  go 0x6018 = do
    -- データ部の#0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15バイトが送信元IPV6アドレス
    maybeSenderIpv6Address <- liftEffect $ traverse (\index -> DV.getUint16be dataview (2 * index)) $ Array.range 0 7
    -- データ部の#16,#17バイトが送信元ポート番号
    maybeSenderPort <- liftEffect $ DV.getUint16be dataview 16
    -- データ部の#18,#19バイトが送信先ポート番号
    maybeDestinationPort <- liftEffect $ DV.getUint16be dataview 18
    -- データ部の#20,#21バイトが送信元PAN ID
    maybeSenderPanId <- liftEffect $ DV.getUint16be dataview 18
    -- データ部の#22バイトが送信先アドレス種別
    maybeSenderAddressType <- liftEffect $ DV.getUint8 dataview 22
    -- データ部の#23バイトが暗号化
    maybeEncrypted <- liftEffect $ DV.getUint8 dataview 23
    -- データ部の#24バイトが暗号化
    maybeRssi <- liftEffect $ DV.getUint8 dataview 24
    -- データ部の#25,#26バイトがデーター長
    maybeDataLength <- liftEffect $ DV.getUint16be dataview 25
    -- データ部の#27バイト以降がデーター
    remainder <- liftEffect $ DV.remainder (DV.buffer dataview) 27
    payload <- liftEffect $ Builder.execPut (Builder.putDataView remainder)
    --
    let
      maybeIpv6string =
        String.joinWith ":" <<< map (Utility.toStringHexAs Utility.word)
          <$> Utility.unwrapAllMaybes maybeSenderIpv6Address

      (maybeReceivedData :: Maybe R.ReceivedData) =
        R.ReceivedData
          <$> ( { senderIpv6: _
              , senderPort: _
              , destinationPort: _
              , senderPanId: _
              , senderAddressType: _
              , encrypted: _
              , rssi: _
              , dataLength: _
              , data: payload
              }
                <$> (IPv6Address.unsafeFromString <$> maybeIpv6string)
                <*> maybeSenderPort
                <*> maybeDestinationPort
                <*> maybeSenderPanId
                <*> maybeSenderAddressType
                <*> maybeEncrypted
                <*> maybeRssi
                <*> maybeDataLength
            )
    -- データ数不足はChecksumErrorを返却する
    pure $ maybe (Either.in1 R.ChecksumError) (Either.in3 <<< R.NotifyReceivedData) maybeReceivedData

  -- 0x6019 起動完了通知
  go 0x6019 = pure $ Either.in3 R.NotifyBootCompleted

  -- 0x6028 PANA認証結果通知
  go 0x6028 = do
    -- データ部の#0バイトがPANA結果
    maybeResult <- liftEffect $ DV.getUint8 dataview 0
    -- データ部の#1,#2,#3,#4,#5,#6,#7,#8バイトがMACアドレス
    maybeMacAddress <- liftEffect $ traverse (DV.getUint8 dataview) $ Array.range 1 8
    --
    let
      (maybePanaResult :: Maybe R.PanaResult) =
        maybeResult
          >>= case _ of
              n
                | n == (UInt.fromInt 0x01) -> Just R.PanaSuccess
                | n == (UInt.fromInt 0x02) -> Just R.PanaFailure
                | n == (UInt.fromInt 0x03) -> Just R.PanaNoResponse
                | otherwise -> Nothing

      (maybeCert :: Maybe R.PanaCert) =
        { result: _, macAddress: _ }
          <$> maybePanaResult
          <*> Utility.unwrapAllMaybes maybeMacAddress
    -- データ数不足はChecksumErrorを返却する
    pure $ maybe (Either.in1 R.ChecksumError) (Either.in3 <<< R.NotifyPanaCert) maybeCert

  --
  go n = pure $ Either.in3 $ R.NotifyUnknownCommandCode n
