module Exec
    ( run
    ) where

import           Control.Lens
    ( (%~)
    , (.~)
    )
import           Control.Monad.IO.Class
    ( liftIO
    )
import           Data.ByteString
    ( ByteString
    )
import           Data.Function
    ( (&)
    )
import qualified Data.Text                 as T
import           Data.Text
    ( Text
    )
import qualified Network.IRC.Client        as IRC
import qualified Network.IRC.Client.Events as IRCEvents
import qualified Network.IRC.Client.Lens   as IRCLens
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
        & IRCLens.logfunc .~ IRC.stdoutLogger
        & IRCLens.password .~ pass

    cfg  =
        IRC.defaultInstanceConfig nick & IRCLens.handlers %~ (joinOurChannel :)

data Event
    = Connected
    | Message Text

joinOurChannel
    :: forall s
    .  IRC.EventHandler s
joinOurChannel = IRC.EventHandler parse handle
  where
    parse
        :: IRC.Event Text -> Maybe Event
    parse =
        \case
            IRCEvents.Event{ IRCEvents._message = IRCEvents.Numeric 1 _ }
                -> Just Connected
            IRCEvents.Event{ IRCEvents._message =
                                 IRCEvents.Privmsg "#cvladfp" (Right text)
                           } -> Just
                $ Message text
            _ -> Nothing

    handle
        :: IRC.Source Text -> Event -> IRC.IRC s ()
    handle source =
        \case
            Connected    -> IRC.send
                $ IRCEvents.Join "#cvladfp"
            Message text -> do
                liftIO
                    $ putStrLn ("<" <> show source <> ">: " <> T.unpack text)
