{-
 https://github.com/ak1211/smartmeter-route-b-app
 Copyright (c) 2023 Akihiro Yamamoto.
 Licensed under the MIT License.
 See LICENSE file in the project root for full license information.
-}
module EchonetLite
  ( EDatum
  , EOJ(..)
  , ESV(..)
  , Frame
  , Property(..)
  , Responce(..)
  , SmartWhmProperty(..)
  , TID(..)
  , getOPC
  , getPDC
  , makeEOJ
  , makeESV
  , makeSmartWhmProperty
  , makeTID
  , parseEchonetLiteFrame
  , serializeEchonetLiteFrame
  , stringWithZeroPadding
  , toBigEndiannessEOJ
  , toBigEndiannessTID
  , toStringEOJ
  , toStringTID
  , toStringWhmProperty
  , toUIntESV
  ) where

import Prelude
import Control.Alternative (guard)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.Types (ArrayBuffer, DataView)
import Data.DateTime (DateTime(..), Time(..))
import Data.DateTime as DateTime
import Data.Either (Either)
import Data.Enum (toEnum)
import Data.Formatter.DateTime as FDT
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Number.Format as F
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String as String
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3)
import Data.Tuple.Nested as TN
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Unfoldable1 (unfoldr1)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Parsing as P
import Parsing.Combinators ((<?>))
import Parsing.Combinators as PC
import Parsing.DataView as PDV

newtype EOJ
  = EOJ (Tuple3 UInt UInt UInt)

derive instance newtypeEOJ :: Newtype EOJ _

derive instance genericEOJ :: Generic EOJ _

instance showEOJ :: Show EOJ where
  show = genericShow

derive instance eqEOJ :: Eq EOJ

makeEOJ :: UInt -> UInt -> UInt -> EOJ
makeEOJ a b c = EOJ (TN.tuple3 a b c)

toBigEndiannessEOJ :: EOJ -> Array UInt
toBigEndiannessEOJ (EOJ a) = [ TN.get1 a, TN.get2 a, TN.get3 a ]

stringWithZeroPadding :: Int -> String -> String
stringWithZeroPadding length input =
  let
    paddings = Array.replicate length (String.codePointFromChar '0')

    arr = paddings <> String.toCodePointArray input
  in
    String.fromCodePointArray
      $ Array.reverse
      $ Array.take length
      $ Array.reverse arr

toStringEOJ :: EOJ -> String
toStringEOJ eoj = joinWith "" $ map toStr $ toBigEndiannessEOJ eoj
  where
  toStr x =
    stringWithZeroPadding 2
      $ Int.toStringAs Int.hexadecimal
      $ UInt.toInt x

newtype TID
  = TID UInt

derive instance newtypeTID :: Newtype TID _

derive instance genericTID :: Generic TID _

instance showTID :: Show TID where
  show = genericShow

derive instance eqTID :: Eq TID

makeTID :: UInt -> Maybe TID
makeTID a = do
  guard $ a <= UInt.fromInt 0xffff
  Just $ TID a

toBigEndiannessTID :: TID -> Array UInt
toBigEndiannessTID (TID a) =
  let
    hi = a `UInt.shr` (UInt.fromInt 8) `UInt.and` (UInt.fromInt 0xff)

    lo = a `UInt.and` (UInt.fromInt 0xff)
  in
    [ hi, lo ]

toStringTID :: TID -> String
toStringTID a = joinWith "" $ map toStr $ toBigEndiannessTID a
  where
  toStr x =
    stringWithZeroPadding 2
      $ Int.toStringAs Int.hexadecimal
      $ UInt.toInt x

-- ECHONET Liteフレーム
type Frame
  = { ehd1 :: UInt
    , ehd2 :: UInt
    , tid :: TID
    , seoj :: EOJ
    , deoj :: EOJ
    , esv :: ESV
    --opcはedataのlength
    --opc :: UInt
    , edata :: NonEmptyArray EDatum
    }

getOPC :: Frame -> UInt
getOPC frame = UInt.fromInt $ NonEmptyArray.length frame.edata

type EDatum
  = { epc :: UInt
    -- pdcはedtのバイト数なのでedtのlength
    -- pdc :: UInt
    , edt :: Array UInt
    }

getPDC :: EDatum -> UInt
getPDC datum = UInt.fromInt $ Array.length datum.edt

-- ECHONET Liteサービス(ESV)
data ESV
  {- 要求用ESVコード一覧表 -}
  = SetI -- プロパティ値書き込み要求（応答不要）
  | SetC -- プロパティ値書き込み要求（応答要）
  | Get -- プロパティ値読み出し要求
  | INF_REQ -- プロパティ値通知要求
  | SetGet -- プロパティ値書き込み・読み出し要求
  {- 応答・通知用ESVコード一覧表 -}
  | Set_Res -- プロパティ値書き込み応答
  | Get_Res -- プロパティ値読み出し応答
  | INF -- プロパティ値通知
  | INFC -- プロパティ値通知（応答要）
  | INFC_Res -- プロパティ値通知応答
  | SetGet_Res -- プロパティ値書き込み・読み出し応答
  {- 不可応答用ESVコード一覧表 -}
  | SetI_SNA -- プロパティ値書き込み要求不可応答
  | SetC_SNA -- プロパティ値書き込み要求不可応答
  | Get_SNA -- プロパティ値読み出し不可応答
  | INF_SNA -- プロパティ値通知不可応答
  | SetGetSNA -- プロパティ値書き込み・読み出し不可応答

derive instance genericESV :: Generic ESV _

derive instance eqESV :: Eq ESV

instance showESV :: Show ESV where
  show = genericShow

makeESV :: UInt -> Maybe ESV
makeESV x = case UInt.toInt x of
  {- 要求用ESVコード一覧表 -}
  0x60 -> Just SetI
  0x61 -> Just SetC
  0x62 -> Just Get
  0x63 -> Just INF_REQ
  0x6E -> Just SetGet
  {- 応答・通知用ESVコード一覧表 -}
  0x71 -> Just Set_Res
  0x72 -> Just Get_Res
  0x73 -> Just INF
  0x74 -> Just INFC
  0x7A -> Just INFC_Res
  0x7E -> Just SetGet_Res
  {- 不可応答用ESVコード一覧表 -}
  0x50 -> Just SetI_SNA
  0x51 -> Just SetC_SNA
  0x52 -> Just Get_SNA
  0x53 -> Just INF_SNA
  0x5E -> Just SetGetSNA
  _ -> Nothing

toUIntESV :: ESV -> UInt
toUIntESV =
  UInt.fromInt
    <<< case _ of
        {- 要求用ESVコード一覧表 -}
        SetI -> 0x60
        SetC -> 0x61
        Get -> 0x62
        INF_REQ -> 0x63
        SetGet -> 0x6E
        {- 応答・通知用ESVコード一覧表 -}
        Set_Res -> 0x71
        Get_Res -> 0x72
        INF -> 0x73
        INFC -> 0x74
        INFC_Res -> 0x7A
        SetGet_Res -> 0x7E
        {- 不可応答用ESVコード一覧表 -}
        SetI_SNA -> 0x50
        SetC_SNA -> 0x51
        Get_SNA -> 0x52
        INF_SNA -> 0x53
        SetGetSNA -> 0x5E

type Responce
  = { ehd1 :: UInt
    , ehd2 :: UInt
    , tid :: UInt
    , seoj :: Array UInt
    , deoj :: Array UInt
    , esv :: ESV
    , opc :: Int
    , props :: Array Property
    }

newtype Property
  = Property { epc :: UInt, pdc :: UInt, edt :: Array UInt }

derive instance newtypeProperty :: Newtype Property _

derive instance eqProperty :: Eq Property

derive instance genericProperty :: Generic Property _

instance showProperty :: Show Property where
  show = genericShow

-- 低圧スマート電力量メータクラス規定
-- クラスグループコード 0x02
-- クラスコード 0x88
-- インスタンスコード 0x01
--
-- ECHONET Liteプロパティ
--
data SmartWhmProperty
  -- スーパークラスより継承
  = X80動作状態 (Maybe { on :: Boolean })
  | X81設置場所 (Maybe UInt)
  | X88異常発生状態 (Maybe { abnormal :: Boolean })
  | X8aメーカーコード (Maybe { manufacturer :: UInt })
  --
  | Xd3係数 (Maybe Int)
  | Xd7積算電力量有効桁数 (Maybe Int)
  | Xe0積算電力量計測値 (Maybe Int)
  | Xe1積算電力量単位 (Maybe { kwh乗率n :: Number })
  | Xe2積算電力量計測値履歴1 (Maybe { day :: Int, cumlativeWH :: Array (Maybe Int) })
  | Xe5積算履歴収集日1 (Maybe Int)
  | Xe7瞬時電力計測値 (Maybe { watt :: Int })
  | Xe8瞬時電流計測値 (Maybe { deciAmpereR :: Int, deciAmpereT :: Int })
  | Xea正方向定時積算電力量計測値 (Maybe { datetime :: DateTime, cumlativeWH :: Int })
  | Xeb逆方向定時積算電力量計測値 (Maybe { datetime :: DateTime, cumlativeWH :: Int })
  | Xed積算履歴収集日2

derive instance genericSmartWhmProperty :: Generic SmartWhmProperty _

instance showSmartWhmProperty :: Show SmartWhmProperty where
  show = genericShow

derive instance eqSmartWhmProperty :: Eq SmartWhmProperty

makeSmartWhmProperty :: Property -> Maybe SmartWhmProperty
makeSmartWhmProperty (Property prop) = case UInt.toInt prop.epc of
  0x80 ->
    Just
      ( X80動作状態
          $ case prop.edt of
              [ x ]
                | x == UInt.fromInt 0x30 -> Just { on: true }
                | x == UInt.fromInt 0x31 -> Just { on: false }
              _ -> Nothing
      )
  0x81 ->
    Just
      ( X81設置場所
          $ case prop.edt of
              [ x ] -> Just x
              _ -> Nothing
      )
  0x88 ->
    Just
      ( X88異常発生状態
          $ case prop.edt of
              [ x ]
                | x == UInt.fromInt 0x41 -> Just { abnormal: true }
                | x == UInt.fromInt 0x42 -> Just { abnormal: false }
              _ -> Nothing
      )
  0x8A ->
    Just $ X8aメーカーコード
      $ case prop.edt of
          [ b2, b1, b0 ] ->
            Just
              { manufacturer:
                  ( (b2 `UInt.shl` (UInt.fromInt 16))
                      `UInt.or`
                        (b1 `UInt.shl` (UInt.fromInt 8))
                      `UInt.or`
                        b0
                  )
              }
          _ -> Nothing
  --
  0xD3 -> Just (Xd3係数 $ Just $ fromMaybe 1 $ fromUInt32BE prop.edt)
  0xD7 -> Just (Xd7積算電力量有効桁数 $ fromUInt8 prop.edt)
  0xE0 -> Just (Xe0積算電力量計測値 $ fromUInt32BE prop.edt)
  0xE1 ->
    Just
      ( Xe1積算電力量単位
          $ case prop.edt of
              [ x ]
                | x == UInt.fromInt 0x00 -> Just { kwh乗率n: 1.0 }
                | x == UInt.fromInt 0x01 -> Just { kwh乗率n: 0.1 }
                | x == UInt.fromInt 0x02 -> Just { kwh乗率n: 0.01 }
                | x == UInt.fromInt 0x03 -> Just { kwh乗率n: 0.001 }
                | x == UInt.fromInt 0x04 -> Just { kwh乗率n: 0.0001 }
                | x == UInt.fromInt 0x0A -> Just { kwh乗率n: 10.0 }
                | x == UInt.fromInt 0x0B -> Just { kwh乗率n: 100.0 }
                | x == UInt.fromInt 0x0C -> Just { kwh乗率n: 1000.0 }
                | x == UInt.fromInt 0x0D -> Just { kwh乗率n: 10000.0 }
              _ -> Nothing
      )
  0xE2 ->
    Just
      $ Xe2積算電力量計測値履歴1 do
          guard $ prop.pdc == UInt.fromInt 194
          let
            maybeDay :: Maybe Int
            maybeDay = fromUInt16BE $ Array.take 2 prop.edt

            array32bits :: Array (Array UInt)
            array32bits = toArrayArray 4 $ Array.drop 2 prop.edt

            arrayMaybes :: Array (Maybe Int)
            arrayMaybes = map fromUInt32BE array32bits
          { day: _, cumlativeWH: arrayMaybes } <$> maybeDay
  0xE5 -> Just (Xe5積算履歴収集日1 $ fromUInt8 prop.edt)
  0xE7 -> Just (Xe7瞬時電力計測値 ({ watt: _ } <$> fromUInt32BE prop.edt))
  0xE8 ->
    Just
      $ Xe8瞬時電流計測値 do
          r <- fromUInt16BE $ Array.take 2 prop.edt
          t <- fromUInt16BE $ Array.drop 2 prop.edt
          Just { deciAmpereR: r, deciAmpereT: t }
  0xEA ->
    Just $ Xea正方向定時積算電力量計測値
      $ case prop.edt of
          [ yearUpper, yearLower, month, day, hour, min, sec, b3, b2, b1, b0 ] -> do
            date <-
              DateTime.canonicalDate
                <$> toEnum (UInt.toInt yearUpper * 256 + UInt.toInt yearLower)
                <*> toEnum (UInt.toInt month)
                <*> toEnum (UInt.toInt day)
            time <- Time <$> toEnum (UInt.toInt hour) <*> toEnum (UInt.toInt min) <*> toEnum (UInt.toInt sec) <*> toEnum 0
            cwh <- fromUInt32BE [ b3, b2, b1, b0 ]
            Just { datetime: DateTime date time, cumlativeWH: cwh }
          _ -> Nothing
  0xEB ->
    Just $ Xeb逆方向定時積算電力量計測値
      $ case prop.edt of
          [ yearUpper, yearLower, month, day, hour, min, sec, b3, b2, b1, b0 ] -> do
            date <-
              DateTime.canonicalDate
                <$> toEnum (UInt.toInt yearUpper * 256 + UInt.toInt yearLower)
                <*> toEnum (UInt.toInt month)
                <*> toEnum (UInt.toInt day)
            time <- Time <$> toEnum (UInt.toInt hour) <*> toEnum (UInt.toInt min) <*> toEnum (UInt.toInt sec) <*> toEnum 0
            cwh <- fromUInt32BE [ b3, b2, b1, b0 ]
            Just { datetime: DateTime date time, cumlativeWH: cwh }
          _ -> Nothing
  0xED -> Just Xed積算履歴収集日2
  _ -> Nothing
  where
  fromUInt8 :: Array UInt -> Maybe Int
  fromUInt8 xs = UInt.toInt <$> Array.head xs

  fromUInt16BE :: Array UInt -> Maybe Int
  fromUInt16BE = case _ of
    [ b1, b0 ] -> UInt.toInt' $ (b1 `shl` ui8) `or` b0
    _ -> Nothing
    where
    ui8 = UInt.fromInt 8

    shl = UInt.shl

    or = UInt.or

  fromUInt32BE :: Array UInt -> Maybe Int
  fromUInt32BE = case _ of
    [ b3, b2, b1, b0 ] -> UInt.toInt' $ (b3 `shl` ui24) `or` (b2 `shl` ui16) `or` (b1 `shl` ui8) `or` b0
    _ -> Nothing
    where
    ui24 = UInt.fromInt 24

    ui16 = UInt.fromInt 16

    ui8 = UInt.fromInt 8

    shl = UInt.shl

    or = UInt.or

toStringWhmProperty :: SmartWhmProperty -> String
toStringWhmProperty = case _ of
  X80動作状態 a ->
    joinWith " "
      [ "動作状態"
      , maybe "" ((if _ then "ON" else "OFF") <<< _.on) a
      ]
  X81設置場所 a ->
    joinWith " "
      [ "設置場所"
      , maybe "" (Int.toStringAs Int.decimal <<< UInt.toInt) a
      ]
  X88異常発生状態 a ->
    joinWith " "
      [ "異常発生状態"
      , maybe "" ((if _ then "異常発生あり" else "異常発生なし") <<< _.abnormal) a
      ]
  X8aメーカーコード a ->
    joinWith " "
      [ "メーカーコード"
      , maybe "" (Int.toStringAs Int.hexadecimal <<< UInt.toInt <<< _.manufacturer) a
      ]
  Xd3係数 a ->
    joinWith " "
      [ "係数"
      , maybe "" (Int.toStringAs Int.decimal) a
      ]
  Xd7積算電力量有効桁数 a ->
    joinWith " "
      $ Array.concat
          [ [ "積算電力量有効桁数" ]
          , case a of
              Nothing -> []
              Just x -> [ Int.toStringAs Int.decimal x, "digits" ]
          ]
  Xe0積算電力量計測値 a ->
    joinWith " "
      [ "積算電力量計測値"
      , maybe "" (Int.toStringAs Int.decimal) a
      ]
  Xe1積算電力量単位 a ->
    joinWith " "
      [ "積算電力量単位"
      , maybe "" (F.toStringWith (F.fixed 4) <<< _.kwh乗率n) a
      ]
  Xe2積算電力量計測値履歴1 a ->
    joinWith " "
      $ Array.concat
          [ [ "積算電力量計測値履歴1" ]
          , case a of
              Nothing -> []
              Just x ->
                [ "day:" <> Int.toStringAs Int.decimal x.day, "cwh:" ]
                  <> map (maybe "NA" (Int.toStringAs Int.decimal)) x.cumlativeWH
          ]
  Xe5積算履歴収集日1 a ->
    joinWith " "
      $ Array.concat
          [ [ "積算履歴収集日1" ]
          , maybe [] (\x -> [ "day:", Int.toStringAs Int.decimal x ]) a
          ]
  Xe7瞬時電力計測値 a ->
    joinWith " "
      $ Array.concat
          [ [ "瞬時電力計測値" ]
          , maybe [] (\x -> [ Int.toStringAs Int.decimal x.watt, "W" ]) a
          ]
  Xe8瞬時電流計測値 a ->
    joinWith " "
      $ Array.concat
          [ [ "瞬時電流計測値" ]
          , case a of
              Nothing -> []
              Just x ->
                [ "R:"
                , Int.toStringAs Int.decimal x.deciAmpereR
                , "deciA"
                , "T:"
                , Int.toStringAs Int.decimal x.deciAmpereT
                , "deciA"
                ]
          ]
  Xea正方向定時積算電力量計測値 a ->
    joinWith " "
      $ Array.concat
          [ [ "正方向定時積算電力量計測値" ]
          , case a of
              Nothing -> []
              Just x ->
                [ printDateTime x.datetime
                , Int.toStringAs Int.decimal x.cumlativeWH
                ]
          ]
  Xeb逆方向定時積算電力量計測値 a ->
    joinWith " "
      $ Array.concat
          [ [ "逆方向定時積算電力量計測値" ]
          , case a of
              Nothing -> []
              Just x ->
                [ printDateTime x.datetime
                , Int.toStringAs Int.decimal x.cumlativeWH
                ]
          ]
  Xed積算履歴収集日2 -> "積算履歴収集日2"
  where
  printDateTime :: DateTime -> String
  printDateTime =
    FDT.format
      $ fromFoldable
          [ FDT.YearFull
          , FDT.Placeholder "-"
          , FDT.MonthTwoDigits
          , FDT.Placeholder "-"
          , FDT.DayOfMonthTwoDigits
          , FDT.Placeholder "T"
          , FDT.Hours24
          , FDT.Placeholder ":"
          , FDT.Minutes
          , FDT.Placeholder ":"
          , FDT.SecondsTwoDigits
          ]

--
parseEchonetLiteFrame :: forall m. MonadRec m => MonadEffect m => DataView -> m (Either P.ParseError Responce)
parseEchonetLiteFrame input =
  P.runParserT input do
    ehd1 <- PDV.anyUint8 <?> "EHD1"
    ehd2 <- PDV.anyUint8 <?> "EHD2"
    tid <- PDV.anyUint16be <?> "TID"
    seoj <- PC.replicateA 3 PDV.anyUint8 <?> "SEOJ"
    deoj <- PC.replicateA 3 PDV.anyUint8 <?> "DEOJ"
    maybeESV <- makeESV <$> PDV.anyUint8 <?> "ESV"
    opc <- PDV.anyInt8 <?> "OPC"
    props <- PC.replicateA opc prop <?> "PROPS"
    case maybeESV of
      Nothing -> P.fail "ESV is invalid code"
      Just esv ->
        pure
          { ehd1: ehd1
          , ehd2: ehd2
          , tid: tid
          , seoj: seoj
          , deoj: deoj
          , esv: esv
          , opc: opc
          , props: props
          }
  where
  prop = do
    epc <- PDV.anyUint8 <?> "EPC"
    pdc <- PDV.anyUint8 <?> "PDC"
    edt <- PC.replicateA (UInt.toInt pdc) PDV.anyUint8 <?> "EDT"
    pure $ Property { epc: epc, pdc: pdc, edt: edt }

toArrayArray :: forall a. Int -> Array a -> Array (Array a)
toArrayArray n = unfoldr1 chop
  where
  chop :: Array a -> Tuple (Array a) (Maybe (Array a))
  chop xs = Tuple (Array.take n xs) (toMaybe $ Array.drop n xs)

  toMaybe :: Array a -> Maybe (Array a)
  toMaybe = map NonEmptyArray.toArray <<< NonEmptyArray.fromArray

serializeEchonetLiteFrame ∷ Frame -> Effect ArrayBuffer
serializeEchonetLiteFrame frame =
  Builder.execPut do
    Builder.putUint8 frame.ehd1 -- 1 byte
    Builder.putUint8 frame.ehd2 -- 1 byte
    Builder.putUint16be $ unwrap frame.tid -- 2 bytes
    traverse_ (Builder.putUint8) $ toBigEndiannessEOJ frame.seoj -- 3 bytes
    traverse_ (Builder.putUint8) $ toBigEndiannessEOJ frame.deoj -- 3 bytes
    Builder.putUint8 $ toUIntESV frame.esv -- 1 byte
    Builder.putUint8 $ getOPC frame -- 1 byte
    traverse_ putEData frame.edata
  where
  putEData :: forall m. MonadEffect m => EDatum -> Builder.PutM m Unit
  putEData datum = do
    Builder.putUint8 datum.epc -- 1 byte
    Builder.putUint8 $ getPDC datum -- 1 byte
    traverse_ Builder.putUint8 datum.edt -- any
