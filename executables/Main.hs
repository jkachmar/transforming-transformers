module Main where

import Prelude

import Accounts (AccountConfig (..), Email (..), Password (..),
                 WidgetConfig (..), createAccount, createWidget)
import App (App, Config (..), runApp)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  let appConfig = Config AccountConfig {} WidgetConfig {}
  runApp appConfig demo

--------------------------------------------------------------------------------
demo :: App ()
demo = do
  let
    email = Email "foo@example.com"
    password = Password "12345_luggage"

  createAccount email password
  createWidget
