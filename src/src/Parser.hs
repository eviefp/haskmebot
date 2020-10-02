module Parser
    ( parse
    , Action(..)
    ) where

import           Control.Applicative
    ((<|>))
import           Data.Functor
    (void)
import           Data.Text
    (Text)
import qualified Data.Text            as T
import           Prelude
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Char as C

type Parser = M.Parsec () Text

data Action
    = RunCommand Text
    | SetCommand Text Text
    | SetStartTime Text
    | SendMessage Text

parse :: Text -> Maybe Action
parse = M.parseMaybe innerParser

innerParser :: Parser Action
innerParser = parseAdd <|> parseStartTime <|> parseRun

parseAdd :: Parser Action
parseAdd = do
    void $ C.string "!command set "
    key <- M.many $ M.satisfy (/= ' ')
    _ <- C.char ' '
    value <- M.many C.asciiChar
    M.eof
    pure $ SetCommand (T.pack key) (T.pack value)

parseRun :: Parser Action
parseRun = RunCommand . T.pack <$> (C.char '!' *> M.many C.asciiChar <* M.eof)

parseStartTime :: Parser Action
parseStartTime =
    SetStartTime . T.pack
        <$> (C.string "!starttime " *> M.many C.asciiChar <* M.eof)
