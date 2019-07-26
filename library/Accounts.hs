{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, UndecidableInstances #-}

module Accounts where

-- * Import common prelude and lens functionality unqualified and unrestricted
import Control.Lens
import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- | Customer account management interface
class Monad m => Accounts m where
  createAccount :: Email -> Password -> m ()

newtype Email = Email Text
  deriving newtype (Eq, Show)

newtype Password = Password Text
  deriving newtype (Eq)

--------------------------------------------------------------------------------
-- | Account management configuration
data AccountConfig = AccountConfig {}
  deriving stock (Eq, Generic, Show)

-- | Account management configuration retrieval interface
--
-- This interface provides a 'Lens\'' that focuses on the 'AccountConfig' record
-- within a larger @environment@
class HasAccountConfig environment where
  accountConfigL :: Lens' environment AccountConfig

-- | An 'AccountConfig' can retrieve itself from itself via the identity
-- function
instance HasAccountConfig AccountConfig where
  accountConfigL = id

-- | Convenience alias expressing that a given @environment@ and execution
-- context provide the capability to retrieve an 'AccountConfig'
type WithAccountConfig environment m =
  ( HasAccountConfig environment
  , MonadReader environment m
  )

-- | An identity 'Monad' transformer that can be used with @deriving via@ to
-- supply an 'Accounts' instance to some application newtype
newtype AccountT (m :: Type -> Type) (result :: Type)
  = AccountT (m result)
  deriving newtype ( Functor, Applicative, Monad
                   , MonadIO, MonadReader environment
                   )

-- | Production implementation for the 'Accounts' interface
instance (MonadIO m, WithAccountConfig environment m)
  => Accounts (AccountT m) where
  createAccount (Email email) _password = do
    _ <- view accountConfigL
    liftIO . print $
      "Created an account for a user with the email address: " <> email

--------------------------------------------------------------------------------
-- | Customer widget creation interface
class Monad m => Widgets m where
  createWidget :: m ()

-- | Widget creation configuration
data WidgetConfig = WidgetConfig {}
  deriving stock (Eq, Generic, Show)

-- | Widget creation configuration retrieval interface
--
-- This interface provides a 'Lens\'' that focuses on the 'WidgetConfig' record
-- within a larger @environment@
class HasWidgetConfig environment where
  widgetConfigL :: Lens' environment WidgetConfig

-- | A 'WidgetConfig' can retrieve itself from itself via the identity
-- function
instance HasWidgetConfig WidgetConfig where
  widgetConfigL = id

-- | Convenience alias expressing that a given @environment@ and execution
-- context provide the capability to retrieve an 'AccountConfig'
type WithWidgetConfig environment m =
  ( HasWidgetConfig environment
  , MonadReader environment m
  )

-- | An identity 'Monad' transformer that can be used with @deriving via@ to
-- supply a 'Widgets' instance to some application newtype
newtype WidgetT (m :: Type -> Type) (result :: Type)
  = WidgetT (m result)
  deriving newtype ( Functor, Applicative, Monad
                   , MonadIO, MonadReader environment
                   )

-- | Production implementation for the 'Accounts' interface
instance (MonadIO m, WithWidgetConfig environment m)
  => Widgets (WidgetT m) where
  createWidget = do
    _ <- view widgetConfigL
    liftIO . print $ ("Created a widget!" :: Text)
