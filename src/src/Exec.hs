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

run
    :: IO ()
run = IRC.runClient conn cfg ()
  where
    conn =
        IRC.plainConnection host port
        & IRCLens.logfunc .~ IRC.fileLogger "haskmebot.log"
        & IRCLens.password .~ pass

    cfg  = IRC.defaultInstanceConfig nick & IRCLens.handlers %~ (allHandler :)

data BotEvent
    = Connected
    | PerformAction P.Action

allHandler
    :: forall s
    .  IRC.EventHandler s
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

    handle
        :: IRC.Source Text -> BotEvent -> IRC.IRC s ()
    handle _ =
        \case
            Connected            -> IRC.send
                $ IRCEvents.Join "#cvladfp"
            PerformAction action -> I.eval action
