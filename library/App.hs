{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

-- * Import common prelude and lens functionality unqualified and unrestricted
import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)

import Accounts (AccountConfig (..), AccountT (..), Accounts (..),
                 HasAccountConfig (..), HasWidgetConfig (..), WidgetConfig (..),
                 WidgetT (..), Widgets)

--------------------------------------------------------------------------------
-- | Application configuration
data Config = Config
  { accountConfig :: AccountConfig
  , widgetConfig  :: WidgetConfig
  } deriving (Eq, Generic, Show)

-- | Provide a way to recover the 'AccountConfig' configuration record from the
-- application configuration record (i.e. 'Config')
instance HasAccountConfig Config where
  accountConfigL = typed @AccountConfig

instance HasWidgetConfig Config where
  widgetConfigL = typed @WidgetConfig

--------------------------------------------------------------------------------
-- | Main application type
--
-- This is effectively a function from the application's configuration to a
-- result in some context @m@
--
-- Common typeclass instances are derived from 'ReaderT Config m' using the
-- @DerivingVia@ extension
--
-- Account management capability is provided by the 'Accounts' typeclass,
-- derived via the instance from the 'AccountT' newtype
--
-- Widget creation capability is provided by the 'Widgets' typeclass,
-- derived via the instance from the 'WidgetT' newtype
newtype App a = App { _runApp :: Config -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)
    via (ReaderT Config IO)
  deriving (Accounts)
    via (AccountT App)
  deriving (Widgets)
    via (WidgetT App)

--------------------------------------------------------------------------------
-- | Convenience function for running an 'App' action
runApp :: Config -> App a -> IO a
runApp cfg app = _runApp app cfg
