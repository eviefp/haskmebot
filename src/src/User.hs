module User
    ( Role(..)
    , User (..)
    , role
    ) where

import Prelude

import Data.Aeson
    (FromJSON, ToJSON)
import Data.Text
    (Text)
import GHC.Generics
    (Generic)

data User
  = User
      { _name :: Text
      , _role :: Role
      }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data Role = Regular | Trusted
    deriving stock (Eq, Ord, Show, Read)
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)

role :: forall a. a -> a -> Role -> a
role regular trusted = \case
    Regular -> regular
    Trusted -> trusted
