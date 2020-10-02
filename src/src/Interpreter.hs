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
import           Data.Text
    (Text)
import qualified GHC.Conc                     as T
import qualified Network.IRC.Client           as IRC
import qualified Network.IRC.Client.Events    as Events
import           Network.Wreq                 as Wreq
import qualified Parser                       as P
import           Prelude

import           State as S
import qualified User  as U

eval :: IRC.Source Text -> U.Role -> P.Action -> IRC.IRC S.State ()
eval _ role =
    \case
        P.SetStartTime t -> do
            case role of
                U.Regular -> go "You do not have the right to change the stream start time."
                U.Trusted -> do
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
        P.SetCommand key value -> do
            state <- view IRC.userState
            newDb <- liftIO $ T.atomically do
                state' <- T.readTVar state
                let
                    commands =
                        state'
                            & F.field @"customCommands"
                            %~ M.alter
                                    (addOrUpdate role value)
                                    key
                T.writeTVar state commands
                pure commands
            liftIO $ S.override newDb
        P.SendMessage text -> go text

  where
    go :: Text -> IRC.IRC s ()
    go = IRC.send . Events.Privmsg "#cvladfp" . Right

    addOrUpdate :: U.Role -> Text -> Maybe Text -> Maybe Text
    addOrUpdate r value = maybe (Just value) (U.role Just (const (Just value)) r)

