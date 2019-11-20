module Topoi (main) where

import           Control.Monad.Except       (Except)
import           Control.Monad.State        (StateT)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Loc                   (Loc, Pos)
import           Data.Text                  (Text)
import           Language.Lexer.Applicative

main :: IO ()
main = putStrLn "hello from topoi"

type Parser = StateT ParserState (Except ParseError)

data Token
    = TokenAssign
    | TokenType
    | TokenTermName Text
    | TokenTypeName Text
    | TokenComment Text
    | TokenWhitespace
    | TokenEOF
    deriving (Eq, Show)

data ParseError
    = Lexical ByteString Pos
    | Syntatical ByteString Loc Token
    deriving (Show)

data ParserState = ParserState
    { currentLoc :: Loc
    -- , tokenStream :: TokenStream (L Token)
    , rawSource  :: ByteString -- for error reporting
    } deriving (Show)
