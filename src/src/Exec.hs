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
import qualified Data.Text as T
import qualified System.Environment as Env

import qualified State as S

host :: ByteString
host = "irc.chat.twitch.tv"

port :: Int
port = 6667

nick
    :: Text
nick = "haskmebot"

pass
    :: IO (Maybe Text)
pass = Just . T.pack <$> Env.getEnv "HASKMEBOT_PASS"

run
    :: IO ()
run = do
    password <- pass
    S.load >>= IRC.runClient (conn password) cfg
  where
    conn password =
        IRC.plainConnection host port
        & IRCLens.logfunc .~ IRC.fileLogger "haskmebot.log"
        & IRCLens.password .~ password

    cfg  = IRC.defaultInstanceConfig nick & IRCLens.handlers %~ (allHandler :)

data BotEvent
    = Connected
    | PerformAction P.Action

allHandler :: IRC.EventHandler S.State
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

    handle :: IRC.Source Text -> BotEvent -> IRC.IRC S.State ()
    handle source =
        \case
            Connected            -> IRC.send
                $ IRCEvents.Join "#cvladfp"
            PerformAction action -> I.eval source action
