module TwitchHooks
    ( run
    ) where

import           Control.Lens
    ((&), (?~))
import qualified Data.Aeson      as Aeson
import           Data.ByteString
    (ByteString)
import           GHC.Generics
    (Generic)
import qualified Network.Wreq    as Wreq
import           Prelude
import qualified Web.Scotty      as Scotty

data Authentication
  = Authentication
      { callback      :: String
      , mode          :: String
      , topic         :: String
      , lease_seconds :: Int
      }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

run :: ByteString -> IO ()
run token = do
  putStrLn "Starting server on port 3000"
  initializeTwtichSubscriptions token
  Scotty.scotty 4000 $
    Scotty.get "/:word" $ do
      beam <- Scotty.param "word"
      Scotty.text $ "hello, " <> beam

twitchHubUrl :: String
twitchHubUrl = "https://api.twitch.tv/helix/webhooks/hub"

selfUrl :: String
selfUrl = ""

cvladfpId :: Int
cvladfpId = 2

twitchFollowsUrl :: String
twitchFollowsUrl = "https://api.twitch.tv/helix/users/follows?first=1&to_id=" <> show cvladfpId

initializeTwtichSubscriptions :: ByteString -> IO ()
initializeTwtichSubscriptions token = do
  let
      payload = Authentication
        { callback = selfUrl <> "/hubCallback"
        , mode = "subscribe"
        , topic = twitchFollowsUrl
        , lease_seconds = 864000
        }
      options = Wreq.defaults
        & Wreq.auth ?~ Wreq.oauth2Bearer token
  _ <- Wreq.postWith options twitchHubUrl (Aeson.toJSON payload)
  pure ()
