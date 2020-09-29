module State
    ( State (..)
    , Notification (..)
    , Second (..)
    , override
    , load
    ) where

import Prelude

import User (Role)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

newtype Second = Second Int
    deriving newtype Num
    deriving newtype (ToJSON, FromJSON)

data Notification = Notification
    { message    :: Text
    , recurrence :: Second
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)

data State = State
    { customCommands :: Map Text Text
    , userRoles      :: Map Text Role
    , notifications  :: [Notification]
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)

override :: State -> IO ()
override = Yaml.encodeFile "haskmebot.yaml"

load :: IO State
load = Yaml.decodeFileThrow "haskmebot.yaml"

