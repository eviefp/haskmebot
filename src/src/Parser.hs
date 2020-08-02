module Parser
    ( parse
    , Action(..)
    ) where

import qualified Data.Text            as T
import           Data.Text
    ( Text
    )
import           Prelude
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Char as C

type Parser = M.Parsec () Text

data Action = Command Text

parse
    :: Text -> Maybe Action
parse = M.parseMaybe innerParser

innerParser
    :: Parser Action
innerParser = Command . T.pack <$> (C.char '!' *> M.many C.asciiChar <* M.eof)
