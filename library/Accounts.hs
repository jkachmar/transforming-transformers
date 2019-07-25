{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, UndecidableInstances #-}

module Accounts where

-- * Import common prelude and lens functionality unqualified and unrestricted
import Control.Lens
import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT (..))
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
data AccountConfig
  = AccountConfig
  {
  } deriving stock (Eq, Generic, Show)

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

--------------------------------------------------------------------------------
-- | Newtype "containing" the production implementation for the 'Accounts'
-- interface
--
-- Since we will be implementing our production functionality in terms of 'IO',
-- we will need to derive an instance for 'MonadIO'
--
-- So long as our final application transformer does not derive 'MonadIO' for
-- itself, the main application context cannot supply that capability
newtype AccountT m a = AccountT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Production implementation for the 'Accounts' interface
--
-- This is "affixed" to the 'AccountT' type so that different transformers can
-- derive this capability with the @DerivingVia@ language extension
instance (MonadIO io, WithAccountConfig environment io)
  => Accounts (AccountT io) where
  createAccount (Email email) _password = do
    _ <- view accountConfigL
    liftIO . print $
      "Created an account for a user with the email address: " <> email
