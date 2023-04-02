{-
 https://github.com/ak1211/smartmeter-route-b-app
 Copyright (c) 2023 Akihiro Yamamoto.
 Licensed under the MIT License.
 See LICENSE file in the project root for full license information.
-}
module WebSerialApi
  ( available
  , module WebSerialApi.SerialPort
  ) where

import WebSerialApi.SerialPort
import Effect (Effect)

--
foreign import available :: Effect Boolean
