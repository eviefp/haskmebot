module Interpreter
    ( eval
    , State
    ) where

import           Control.Applicative
    ( empty
    )
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
eval (P.RunCommand text) = do
    state <- view IRC.userState
    commands <- liftIO $ T.readTVarIO state
    maybe empty go (M.lookup text commands)
  where
    go
        :: Text -> IRC.IRC s ()
    go = IRC.send . Events.Privmsg "#cvladfp" . Right
