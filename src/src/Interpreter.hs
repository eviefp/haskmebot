module Interpreter
    ( eval
    ) where

import           Control.Lens
    (view, (%~), (^.))
import           Control.Monad.IO.Class
    (MonadIO (liftIO))
import           Data.Aeson                   as Aeson
import           Data.Function
    ((&))
import           Data.Generics.Product.Fields as F
import qualified Data.Map.Strict              as M
import           Data.Maybe
    (fromMaybe)
import           Data.Text
    (Text)
import qualified GHC.Conc                     as T
import qualified Network.IRC.Client           as IRC
import qualified Network.IRC.Client.Events    as Events
import           Network.Wreq                 as Wreq
import qualified Parser                       as P
import           Prelude

import State as S
import User  as U

toUsername :: IRC.Source Text -> Maybe Text
toUsername = \case
    IRC.User nickname -> Just nickname
    IRC.Channel _ nickname -> Just nickname
    IRC.Server _ -> Nothing

getRole' :: Text -> IRC.IRC S.State U.Role
getRole' nickname = do
    state <- view IRC.userState
    state' <- liftIO $ T.readTVarIO state
    let roles = state' ^. F.field @"userRoles"
    pure $ fromMaybe Regular (M.lookup nickname roles)

getRole :: IRC.Source Text -> IRC.IRC S.State U.Role
getRole s =
    maybe (pure Regular) getRole' (toUsername s)

eval :: IRC.Source Text -> P.Action -> IRC.IRC S.State ()
eval source =
    \case
        P.SetStartTime t -> do
            getRole source >>= \case
                Regular -> go "You do not have the right to change the stream start time."
                Trusted -> do
                    res <-
                        liftIO
                            $ Wreq.post "http://localhost:3000/start-time" (Aeson.toJSON t)
                    case res ^. Wreq.responseStatus . Wreq.statusCode of
                        200 -> go "New start time set."
                        _   -> go "Some error occured. Uh-oh."
        P.RunCommand text -> do
            state <- view IRC.userState
            state' <- liftIO $ T.readTVarIO state
            let commands = state' ^. F.field @"customCommands"
            maybe (pure ()) go (M.lookup text commands)
        P.AddCommand key value -> do
            state <- view IRC.userState
            newDb <- liftIO $ T.atomically do
                state' <- T.readTVar state
                let
                    commands =
                        state'
                            & F.field @"customCommands"
                            %~ M.alter
                                    (maybe (Just value) Just)
                                    key
                T.writeTVar state commands
                pure commands
            liftIO $ S.override newDb
        P.SendMessage text -> go text

  where
    go :: Text -> IRC.IRC s ()
    go = IRC.send . Events.Privmsg "#cvladfp" . Right

