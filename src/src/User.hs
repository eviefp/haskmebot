module User
    ( Role(..)
    , User (..)
    ) where

import Prelude

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data User = User
    { name :: Text
    , role :: Role
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)

data Role = Regular | Trusted
    deriving stock (Eq, Ord, Show, Read)
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)

