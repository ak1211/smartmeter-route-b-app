{-
 https://github.com/ak1211/smartmeter-route-b-app
 Copyright (c) 2023 Akihiro Yamamoto.
 Licensed under the MIT License.
 See LICENSE file in the project root for full license information.
-}
module Main where

import Prelude
import Effect (Effect)
import Page as Page
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Page.component unit body
