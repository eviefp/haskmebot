module Interpreter
    ( eval
    ) where

import           Control.Applicative
    ( empty
    )
import           Data.Text
    ( Text
    )
import qualified Network.IRC.Client        as IRC
import qualified Network.IRC.Client.Events as Events
import qualified Parser                    as P
import           Prelude

commands
    :: [ ( Text, Text ) ]
commands =
    [ ( "dotfiles"
          , "The dotfiles are over at https://github.com/vladciobanu/dotfiles"
          )
    ]

eval
    :: P.Action -> IRC.IRC s ()
eval (P.Command text) = maybe empty go (lookup text commands)
  where
    go
        :: Text -> IRC.IRC s ()
    go = IRC.send . Events.Privmsg "#cvladfp" . Right
