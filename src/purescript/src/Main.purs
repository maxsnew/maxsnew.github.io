module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import App as App

-- | Run the app.
main :: Eff (HA.HalogenEffects (App.Effs ())) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI App.ui "" body
  io.query $ H.action $ App.Refresh
