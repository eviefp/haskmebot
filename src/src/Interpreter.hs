module Interpreter
    ( eval
    , readDb
    , State
    ) where

import Data.Map.Strict (Map)
import           Data.Text
    ( Text
    )
import qualified Network.IRC.Client        as IRC
import qualified Network.IRC.Client.Events as Events
import qualified Parser                    as P
import           Prelude
import qualified GHC.Conc as T
import Control.Lens (view)
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class (MonadIO(liftIO))

type State = Map Text Text

eval :: P.Action -> IRC.IRC State ()
eval =
    \case
        P.RunCommand text -> do
            state <- view IRC.userState
            commands <- liftIO $ T.readTVarIO state
            maybe (pure ()) go (M.lookup text commands)
        P.AddCommand key value -> do
            state <- view IRC.userState
            newDb <- liftIO $ T.atomically do
                commands <- T.readTVar state
                let commands' = (M.alter
                                    (maybe (Just value) Just)
                                    key
                                    commands
                                )
                T.writeTVar state commands'
                pure commands'
            liftIO $ storeDb newDb

  where
    go
        :: Text -> IRC.IRC s ()
    go = IRC.send . Events.Privmsg "#cvladfp" . Right

storeDb :: State -> IO ()
storeDb = writeFile "database.show" . show

readDb :: IO State
readDb = do
    contents <- readFile "database.show"
    print $ length contents
    pure $ read contents

