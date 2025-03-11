module Utility
  ( Size
  , dword
  , elemWithIndex
  , nibble
  , octet
  , toArrayArray
  , toStringHexAs
  , unwrapAllMaybes
  , word
  , zeroSizeArrayBuffer
  ) where

import Prelude
import Data.Array as Array
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Unfoldable (unfoldr1)
import Effect.Unsafe (unsafePerformEffect)

zeroSizeArrayBuffer :: ArrayBuffer
zeroSizeArrayBuffer = unsafePerformEffect $ ArrayBuffer.empty 0

newtype Size
  = MkSize Int

dword :: Size
dword = MkSize 32

word :: Size
word = MkSize 16

octet :: Size
octet = MkSize 8

nibble :: Size
nibble = MkSize 4

toStringHexAs :: Size -> UInt -> String
toStringHexAs (MkSize 32) x = toStringHexAs word (UInt.shr x $ UInt.fromInt 16) <> toStringHexAs word x

toStringHexAs (MkSize 16) x = f (UInt.and x $ UInt.fromInt 0xffff)
  where
  f n = toStringHexAs octet (UInt.shr n $ UInt.fromInt 8) <> toStringHexAs octet n

toStringHexAs (MkSize 8) x = f (UInt.and x $ UInt.fromInt 0xff)
  where
  f n = toStringHexAs nibble (UInt.shr n $ UInt.fromInt 4) <> toStringHexAs nibble n

toStringHexAs (MkSize 4) x = f (UInt.and x $ UInt.fromInt 0xf)
  where
  f = Int.toStringAs Int.hexadecimal <<< UInt.toInt

toStringHexAs _ x = toStringHexAs dword x

-- |
toArrayArray :: forall a. Int -> Array a -> Array (Array a)
toArrayArray n = unfoldr1 chop
  where
  chop :: Array a -> Tuple (Array a) (Maybe (Array a))
  chop xs =
    let
      a = Array.take n xs

      b = Array.drop n xs
    in
      if Array.null b then
        Tuple a Nothing
      else
        Tuple a (Just b)

elemWithIndex :: forall a. Array a -> Array { index :: Int, elem :: a }
elemWithIndex = case _ of
  [] -> []
  array -> map (\(Tuple a b) -> { index: a, elem: b }) $ Array.zip (range array) array
  where
  range array = Array.range 0 $ Array.length array - 1

unwrapAllMaybes :: forall a. Array (Maybe a) -> Maybe (Array a)
unwrapAllMaybes = Array.foldM (\acc maybeItem -> Array.snoc acc <$> maybeItem) []
