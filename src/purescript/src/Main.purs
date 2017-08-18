module Main where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import App as App

-- | Run the app.
main :: Eff (HA.HalogenEffects (dom :: DOM, ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI App.ui "" body
  io.query $ H.action $ App.MakeRequest App.statusUrl
