module Topoi (main) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

main :: IO ()
main = putStrLn "hello from topoi"

-- Should I use String or Text here ?
-- type TestParser = MP.Parsec ParseError String
-- instance CanParse (MP.ParsecT ParseError String Identity)