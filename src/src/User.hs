module User
    ( Role(..)
    , User (..)
    ) where

import Prelude

import Data.Text (Text)
import GHC.Generics (Generic)

data User = User
    { name :: Text
    , role :: Role
    } deriving stock (Generic)

data Role = Regular | Trusted
   deriving stock (Eq, Ord, Show, Read)

