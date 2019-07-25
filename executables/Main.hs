module Main where

import Prelude

import Accounts (AccountConfig (..), Accounts, Email (..), Password (..),
                 WidgetConfig (..), Widgets, createAccount, createWidget)
import App (Config (..), runApp)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  let appConfig = Config AccountConfig {} WidgetConfig {}
  runApp appConfig demo

--------------------------------------------------------------------------------
demo :: (Accounts io, Widgets io) => io ()
demo = do
  let
    email = Email "foo@example.com"
    password = Password "12345_luggage"

  createAccount email password
  createWidget
