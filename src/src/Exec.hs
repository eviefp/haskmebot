module Exec
    ( run
    ) where

import           Control.Lens
    ( (%~)
    , (.~)
    )
import           Data.ByteString
    ( ByteString
    )
import           Data.Function
    ( (&)
    )
import           Data.Text
    ( Text
    )
import           Interpreter               as I
import qualified Network.IRC.Client        as IRC
import qualified Network.IRC.Client.Events as IRCEvents
import qualified Network.IRC.Client.Lens   as IRCLens
import qualified Parser                    as P
import qualified Data.Map.Strict as M
import           Prelude

host
    :: ByteString
host = "irc.chat.twitch.tv"

port
    :: Int
port = 6667

nick
    :: Text
nick = "haskmebot"

pass
    :: Maybe Text
pass = Just "oauth:9yrc1aitdjpbwaqkmfikm5hqwwh54y"

commands
    :: I.State
commands = M.fromList
    [ ( "mydotfiles"
      , "The dotfiles are over at https://github.com/vladciobanu/dotfiles"
      )
    ]
run
    :: IO ()
run = IRC.runClient conn cfg commands
  where
    conn =
        IRC.plainConnection host port
        & IRCLens.logfunc .~ IRC.fileLogger "haskmebot.log"
        & IRCLens.password .~ pass

    cfg  = IRC.defaultInstanceConfig nick & IRCLens.handlers %~ (allHandler :)

data BotEvent
    = Connected
    | PerformAction P.Action

allHandler :: IRC.EventHandler I.State
allHandler = IRC.EventHandler parse handle
  where
    parse
        :: IRC.Event Text -> Maybe BotEvent
    parse =
        \case
            IRCEvents.Event{ IRCEvents._message = IRCEvents.Numeric 1 _ }
                -> Just Connected
            IRCEvents.Event{ IRCEvents._message =
                                 IRCEvents.Privmsg "#cvladfp" (Right text)
                           } -> PerformAction <$> P.parse text
            _ -> Nothing

    handle :: IRC.Source Text -> BotEvent -> IRC.IRC I.State ()
    handle _ =
        \case
            Connected            -> IRC.send
                $ IRCEvents.Join "#cvladfp"
            PerformAction action -> I.eval action
