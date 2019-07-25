{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

-- * Import common prelude and lens functionality unqualified and unrestricted
import Control.Lens
import Prelude

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)

import Accounts (AccountConfig (..), AccountT (..), Accounts (..), Email (..),
                 HasAccountConfig (..), Password (..))

--------------------------------------------------------------------------------
-- | Application configuration
newtype Config = Config
  { accountConfig :: AccountConfig
  } deriving (Eq, Generic, Show)

-- | Provide a way to recover the 'AccountConfig' configuration record from the
-- application configuration record (i.e. 'Config')
instance HasAccountConfig Config where
  accountConfigL = typed @AccountConfig

--------------------------------------------------------------------------------
newtype ConfigReaderT m a = ConfigReaderT (ReaderT Config m a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Config)

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
newtype AppT m a = AppT { evalAppT :: Config -> m a }
  deriving ( Functor, Applicative, Monad
           , MonadReader Config
           ) via (ConfigReaderT m)

  deriving Accounts via (AccountT (ReaderT Config m))

-- | Convenience function for running an 'AppT' action with its arguments
-- reversed
runAppT :: Config -> AppT m a -> m a
runAppT cfg app = evalAppT app cfg

--------------------------------------------------------------------------------
-- | Convenience alias for the most common 'AppT', whose monadic context is
-- simply 'IO'
type App = AppT IO

-- | Convenience function for running an 'App' action
runApp :: Config -> App a -> IO a
runApp = runAppT
