{-
 https://github.com/ak1211/smartmeter-route-b-app
 Copyright (c) 2023 Akihiro Yamamoto.
 Licensed under the MIT License.
 See LICENSE file in the project root for full license information.
-}
module Bp35a1
  ( Address64(..)
  , EPANDESC(..)
  , ER(..)
  , ERXUDP(..)
  , EVENT(..)
  , IpV6Address(..)
  , RSSI(..)
  , Responce(..)
  , toStringEvent
  , parseResponce
  , parseResponce'
  , makeRSSIfromLQI
  , toStringIpV6Address
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Class (class MonadEffect, liftEffect)
import Parsing as P
import Parsing.Combinators as PC
import Parsing.String as PS
import Parsing.Token as PT
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.TextEncoder as TextEncoder
import Web.Encoding.UtfLabel as UtfLabel

newtype ER
  = ER Int

derive instance newtypeER :: Newtype ER _

derive instance eqER :: Eq ER

instance showER :: Show ER where
  show (ER 0x01) = "ER01: reserved"
  show (ER 0x02) = "ER02: reserved"
  show (ER 0x03) = "ER03: reserved"
  show (ER 0x04) = "ER04: 指定されたコマンドがサポートされていない"
  show (ER 0x05) = "ER05: 指定されたコマンドの引数の数が正しくない"
  show (ER 0x06) = "ER06: 指定されたコマンドの引数形式や値域が正しくない"
  show (ER 0x07) = "ER07: reserved"
  show (ER 0x08) = "ER08: reserved"
  show (ER 0x09) = "ER09: UART入力エラーが発生した"
  show (ER 0x10) = "ER10: 指定されたコマンドは受け付けたが、実行結果が失敗した"
  show (ER c) = "ER" <> Int.toStringAs Int.hexadecimal c

newtype IpV6Address
  = IpV6Address String

derive instance newtypeIpV6Address :: Newtype IpV6Address _

derive instance eqIpV6Address :: Eq IpV6Address

instance showIpV6Address :: Show IpV6Address where
  show (IpV6Address s) = s

toStringIpV6Address :: IpV6Address -> String
toStringIpV6Address = unwrap

newtype Address64
  = Address64 String

derive instance newtypeAddress64 :: Newtype Address64 _

derive instance eqAddress64 :: Eq Address64

instance showAddress64 :: Show Address64 where
  show (Address64 s) = s

newtype EVENT
  = EVENT { num :: Int, sender :: IpV6Address, param :: Maybe Int }

derive instance newtypeEVENT :: Newtype EVENT _

derive instance eqEVENT :: Eq EVENT

instance showEVENT :: Show EVENT where
  show ev =
    let
      s = toStringEvent ev
    in
      joinWith " "
        $ Array.concat
            [ [ s.message ]
            , [ "sender: ", s.sender ]
            , maybe [] (\a -> [ "param: " <> a ]) s.param
            ]

toStringEvent :: EVENT -> { message :: String, sender :: String, param :: Maybe String }
toStringEvent (EVENT ev) =
  { message: eventnum ev.num
  , sender: show ev.sender
  , param: param ev.num <$> ev.param
  }
  where
  eventnum n
    | n == 0x01 = "EVENT 0x01: NS を受信した"
    | n == 0x02 = "EVENT 0x02: NA を受信した"
    | n == 0x05 = "EVENT 0x05: Echo Request を受信した"
    | n == 0x1F = "EVENT 0x1F: ED スキャンが完了した"
    | n == 0x20 = "EVENT 0x20: Beacon を受信した"
    | n == 0x21 = "EVENT 0x21: UDP 送信処理が完了した"
    | n == 0x22 = "EVENT 0x22: アクティブスキャンが完了した"
    | n == 0x24 = "EVENT 0x24: PANA による接続過程でエラーが発生した(接続が完了しなかった)"
    | n == 0x25 = "EVENT 0x25: PANA による接続が完了した"
    | n == 0x26 = "EVENT 0x26: 接続相手からセッション終了要求を受信した"
    | n == 0x27 = "EVENT 0x27: PANA セッションの終了に成功した"
    | n == 0x28 = "EVENT 0x28: PANA セッションの終了要求に対する応答がなくタイムアウトした（セッションは終了）"
    | n == 0x29 = "EVENT 0x29: セッションのライフタイムが経過して期限切れになった"
    | n == 0x32 = "EVENT 0x32: ARIB108 の送信総和時間の制限が発動した（このイベント以後、あらゆるデータ送信要求が内部で自動的にキャンセルされます）"
    | n == 0x33 = "EVENT 0x33: 送信総和時間の制限が解除された"
    | otherwise = "EVENT " <> Int.toStringAs Int.hexadecimal n

  param n p
    | n == 0x1F = "直後にEEDSCAN イベントが発生します"
    | n == 0x20 = "直後にEPANDESC イベントが発生します"
    | n == 0x21 && p == 0 = "UDP の送信に成功"
    | n == 0x21 && p == 1 = "UDP の送信に失敗"
    | n == 0x21 && p == 2 = "UDP を送信する代わりにアドレス要請（Neighbor Solicitation）を行ったことを表します。"
    | otherwise = Int.toStringAs Int.hexadecimal p

newtype RSSI
  = RSSI Number

derive instance newtypeRSSI :: Newtype RSSI _

derive instance eqRSSI :: Eq RSSI

derive instance genericRSSI :: Generic RSSI _

instance showRSSI :: Show RSSI where
  show = genericShow

makeRSSIfromLQI :: UInt -> RSSI
makeRSSIfromLQI x = RSSI (0.275 * (UInt.toNumber x) - 104.27)

newtype EPANDESC
  = EPANDESC
  { channel :: String
  , channelPage :: String
  , panId :: String
  , addr :: Address64
  , lqi :: UInt
  , pairId :: String
  }

derive instance newtypeEPANDESC :: Newtype EPANDESC _

derive instance eqEPANDESC :: Eq EPANDESC

instance showEPANDESC :: Show EPANDESC where
  show (EPANDESC e) =
    joinWith " "
      [ "EPANDESC"
      , show e.channel
      , show e.channelPage
      , show e.panId
      , show e.addr
      , show e.lqi
      , show e.pairId
      ]

newtype ERXUDP
  = ERXUDP
  { sender :: IpV6Address
  , dest :: IpV6Address
  , rport :: Int
  , lport :: Int
  , senderlla :: Address64
  , secured :: Int
  , datalen :: Int
  , payload :: Array UInt
  }

derive instance newtypeERXUDP :: Newtype ERXUDP _

derive instance eqERXUDP :: Eq ERXUDP

instance showERXUDP :: Show ERXUDP where
  show (ERXUDP e) =
    joinWith " "
      [ "ERXUDP"
      , show e.sender
      , show e.dest
      , show e.rport
      , show e.lport
      , show e.senderlla
      , show e.secured
      , show e.datalen
      , show e.payload
      ]

data Responce
  = ResLocalEcho String
  | ResOK
  | ResFAIL ER
  | ResIpV6Address IpV6Address
  | ResEVENT EVENT
  | ResEPANDESC EPANDESC
  | ResERXUDP ERXUDP
  | ResMessage String

derive instance eqResponce :: Eq Responce

derive instance genericResponce :: Generic Responce _

instance showResponce :: Show Responce where
  show = genericShow

parseResponce ::
  forall m.
  MonadEffect m =>
  Maybe String ->
  Uint8Array ->
  m (Either P.ParseError { responce :: Responce, rest :: Array Char })
parseResponce lastSendCommand input = do
  decoder <- liftEffect $ TextDecoder.new UtfLabel.utf8
  string <- liftEffect $ TextDecoder.decode input decoder
  pure $ go string
  where
  go string =
    P.runParser string do
      tokenLocalEcho
        <|> tokenOk
        <|> tokenFail
        <|> tokenErxudp
        <|> tokenEvent
        <|> tokenEpandesc
        <|> ((\a _ c -> { responce: ResIpV6Address a, rest: toCharArray c }) <$> ipv6addr <*> tokenCrLf <*> PS.rest)
        <|> tokenOneLine
        <|> P.fail "err"
    where
    hexadecimalDigit = do
      cs <- Array.many PT.hexDigit
      case Int.fromStringAs Int.hexadecimal $ fromCharArray cs of
        Nothing -> P.fail "value is not numeric"
        Just x -> pure x

    ipv6addr = do
      a1 <- twoOctets
      a2 <- PS.char ':' *> twoOctets
      a3 <- PS.char ':' *> twoOctets
      a4 <- PS.char ':' *> twoOctets
      a5 <- PS.char ':' *> twoOctets
      a6 <- PS.char ':' *> twoOctets
      a7 <- PS.char ':' *> twoOctets
      a8 <- PS.char ':' *> twoOctets
      let
        strings = map fromCharArray [ a1, a2, a3, a4, a5, a6, a7, a8 ]
      pure $ IpV6Address $ String.joinWith ":" strings
      where
      twoOctets = do
        a <- PT.hexDigit
        b <- PT.hexDigit
        c <- PT.hexDigit
        d <- PT.hexDigit
        pure [ a, b, c, d ]

    address64 = do
      str <- PS.takeN 16
      pure $ Address64 $ str

    tokenCrLf = PS.char '\r' *> PS.char '\n' # void

    tokenOneLine = do
      cs <- Array.many PT.alphaNum
      tokenCrLf
      rest <- PS.rest
      pure { responce: ResMessage $ fromCharArray cs, rest: toCharArray rest }

    tokenLocalEcho = do
      str <- P.liftMaybe (\_ -> "this is not echo reply") lastSendCommand
      _ <- PS.string str
      tokenCrLf
      rest <- PS.rest
      pure { responce: ResLocalEcho str, rest: toCharArray rest }

    tokenOk = do
      _ <- PS.string "OK"
      tokenCrLf
      rest <- PS.rest
      pure { responce: ResOK, rest: toCharArray rest }

    tokenFail = do
      _ <- PS.string "FAIL ER"
      n <- hexadecimalDigit
      tokenCrLf
      rest <- PS.rest
      pure { responce: ResFAIL $ ER n, rest: toCharArray rest }

    tokenEvent = do
      _ <- PS.string "EVENT"
      _ <- PS.char ' '
      n <- hexadecimalDigit
      _ <- PS.char ' '
      addr <- ipv6addr
      _ <- PC.optional $ PS.char ' '
      param <- PC.optionMaybe $ hexadecimalDigit
      tokenCrLf
      rest <- PS.rest
      pure { responce: ResEVENT $ EVENT { num: n, sender: addr, param: param }, rest: toCharArray rest }

    tokenEpandesc = do
      _ <- PS.string "EPANDESC"
      tokenCrLf
      _ <- PS.string "  Channel:"
      channel <- Array.many PT.hexDigit
      tokenCrLf
      _ <- PS.string "  Channel Page:"
      channelPage <- Array.many PT.hexDigit
      tokenCrLf
      _ <- PS.string "  Pan ID:"
      panId <- Array.many PT.hexDigit
      tokenCrLf
      _ <- PS.string "  Addr:"
      addr <- address64
      tokenCrLf
      _ <- PS.string "  LQI:"
      lqi <- hexadecimalDigit
      tokenCrLf
      _ <- PS.string "  PairID:"
      pairId <- Array.many PT.hexDigit
      tokenCrLf
      rest <- PS.rest
      pure
        { responce:
            ResEPANDESC
              $ EPANDESC
                  { channel: fromCharArray channel
                  , channelPage: fromCharArray channelPage
                  , panId: fromCharArray panId
                  , addr: addr
                  , lqi: UInt.fromInt lqi
                  , pairId: fromCharArray pairId
                  }
        , rest: toCharArray rest
        }

    tokenErxudp = do
      _ <- PS.string "ERXUDP"
      _ <- PS.char ' '
      sender <- ipv6addr
      _ <- PS.char ' '
      dest <- ipv6addr
      _ <- PS.char ' '
      rport <- hexadecimalDigit
      _ <- PS.char ' '
      lport <- hexadecimalDigit
      _ <- PS.char ' '
      senderlla <- address64
      _ <- PS.char ' '
      secured <- hexadecimalDigit
      _ <- PS.char ' '
      datalen <- hexadecimalDigit
      _ <- PS.char ' '
      payload <- Array.many octet
      rest <- PS.rest
      pure
        { responce:
            ResERXUDP
              $ ERXUDP
                  { sender: sender
                  , dest: dest
                  , rport: rport
                  , lport: lport
                  , senderlla: senderlla
                  , secured: secured
                  , datalen: datalen
                  , payload: payload
                  }
        , rest: toCharArray rest
        }
      where
      octet = do
        a <- PT.hexDigit
        b <- PT.hexDigit
        case UInt.fromInt <$> Int.fromStringAs Int.hexadecimal (fromCharArray [ a, b ]) of
          Nothing -> P.fail "value is not numeric"
          Just x -> pure x

parseResponce' ::
  forall m.
  MonadEffect m =>
  Maybe String ->
  String ->
  m (Either P.ParseError { responce :: Responce, rest :: Array Char })
parseResponce' lastSendCommand input = do
  encoder <- liftEffect $ TextEncoder.new
  parseResponce lastSendCommand $ TextEncoder.encode input encoder
