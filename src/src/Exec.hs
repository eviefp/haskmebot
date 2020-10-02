module Exec
    ( run
    ) where

import qualified Control.Concurrent           as Conc
import           Control.Lens
    (view, (%~), (.~), (^.))
import           Control.Monad
    (forever)
import qualified Control.Monad.IO.Class       as IO
import           Data.ByteString
    (ByteString)
import           Data.Foldable
    (traverse_)
import           Data.Function
    ((&))
import           Data.Generics.Product.Fields as F
import qualified Data.Map.Strict              as M
import           Data.Maybe
    (fromMaybe)
import           Data.Text
    (Text)
import qualified Data.Text                    as T
import qualified GHC.Conc                     as GhcConc
import           Interpreter                  as I
import qualified Network.IRC.Client           as IRC
import qualified Network.IRC.Client.Events    as IRCEvents
import qualified Network.IRC.Client.Lens      as IRCLens
import qualified Parser                       as P
import           Prelude
import qualified State                        as S
import qualified System.Environment           as Env
import qualified User                         as U

-- Things to move to config:
--  host, port, nick, env variable, logfile
--
host :: ByteString
host = "irc.chat.twitch.tv"

port :: Int
port = 6667

nick :: Text
nick = "haskmebot"

pass :: IO (Maybe Text)
pass = Just . T.pack <$> Env.getEnv "HASKMEBOT_PASS"

-- What we're doing here:
--   1. Grabbing the IRC OAUTH token from the environment
--   2. Running the IRC client
run :: IO ()
run = do
    password <- pass
    S.load >>= IRC.runClient (conn password) cfg
  where
    conn password =
        IRC.plainConnection host port
        & IRCLens.logfunc .~ IRC.fileLogger "haskmebot.log"
        & IRCLens.password .~ password

    cfg  = IRC.defaultInstanceConfig nick
            & IRCLens.handlers %~ (\xs -> allHandler : IRC.joinOnWelcome : xs)
            & IRCLens.channels .~ [ "#cvladfp" ]

data BotEvent
    = Connected
    | PerformAction P.Action

allHandler :: IRC.EventHandler S.State
allHandler = IRC.EventHandler parse handle
  where
    parse :: IRC.Event Text -> Maybe BotEvent
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
            Connected            -> do
                state <- view IRC.userState
                state' <- IO.liftIO $ GhcConc.readTVarIO state
                let notifications = S.notifications state'

                traverse_ (IRC.fork . doNotifications) notifications
            PerformAction action -> do
                role <- getRole source
                I.eval source role action

    doNotifications :: S.Notification -> IRC.IRC S.State ()
    doNotifications S.Notification { S.message, S.recurrence } =
        let (S.Second seconds) = recurrence
         in forever
            $ IO.liftIO (Conc.threadDelay (seconds * 1_000_000))
                *> I.eval
                    (IRC.User "")
                    U.Regular
                    (P.SendMessage message)

toUsername :: IRC.Source Text -> Maybe Text
toUsername = \case
    IRC.User nickname -> Just nickname
    IRC.Channel _ nickname -> Just nickname
    IRC.Server _ -> Nothing

getRole' :: Text -> IRC.IRC S.State U.Role
getRole' nickname = do
    state <- view IRC.userState
    state' <- IO.liftIO $ GhcConc.readTVarIO state
    let roles = state' ^. F.field @"userRoles"
    pure $ fromMaybe U.Regular (M.lookup nickname roles)

getRole :: IRC.Source Text -> IRC.IRC S.State U.Role
getRole s =
    maybe (pure U.Regular) getRole' (toUsername s)

