{-
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2025 Akihiro Yamamoto <github.com/ak1211>
-}
module Page.ToastMessage
  ( ToastMessage(..)
  , bootstrapToastShow
  ) where

import Prelude
import Data.DateTime.Instant (Instant)
import Effect (Effect)

--
foreign import bootstrapToastShow :: Effect Unit

data ToastMessage
  = ToastMessage { at :: Instant, message :: String }
  | ToastMessageError { at :: Instant, message :: String }
  | ToastMessageEvent { at :: Instant, message :: String }
  | ToastMessageLocalEcho { at :: Instant, message :: String }
  | ToastMessageNotify { at :: Instant, message :: String }
  | ToastMessageCommandResponse { at :: Instant, message :: String }

derive instance eqToastMessage :: Eq ToastMessage
