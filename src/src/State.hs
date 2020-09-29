module State
    ( State (..)
    , overrideCommands
    , overrideUsers
    , load
    ) where

import Prelude

import User (Role)
import Data.Text (Text)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

data State = State
    { customCommands :: Map Text Text
    , userRoles      :: Map Text Role
    }
    deriving stock (Generic)

overrideCommands :: Map Text Text -> IO ()
overrideCommands = writeFile "database.show" . show

overrideUsers :: Map Text Role -> IO ()
overrideUsers = writeFile "users.show" . show

readCommands :: IO (Map Text Text)
readCommands = read <$> readFile "database.show"

readUsers :: IO (Map Text Role)
readUsers = read <$> readFile "users.show"

load :: IO State
load = State <$> readCommands <*> readUsers

