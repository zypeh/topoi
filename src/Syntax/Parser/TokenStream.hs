module Syntax.Parser.TokenStream where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Loc
import Data.Proxy

instance Ord tok => Ord (TokenStream (L tok)) where
    compare _ _ = EQ

instance (Ord tok, Show tok) => Stream (TokenStream (L tok)) where
    type Token (TokenStream (L tok)) = L tok
    type Tokens (TokenStream (L tok)) = [L tok]
    tokenToChunk Proxy tok = [tok]
    tokensToChunk Proxy = id
    chunkToTokens Proxy = id
    chunkLength Proxy = chunkLength'
    chunkEmpty Proxy = chunkEmpty'
    take1_ = take1_'
    takeN_ = takeN_'
    takeWhile_ = takeWhile_'
    showTokens Proxy = showTokens'
    reachOffset = reachOffset'

chunkLength' :: [L tok] -> Int
chunkLength' = length

chunkEmpty' :: [L tok] -> Bool
chunkEmpty' = chunkLength' == 0

streamEmpty :: TokenStream (L tok) -> Bool
streamEmpty (TsToken _ _) = False
streamEmpty TsEof         = True
streamEmpty (TsError _)   = True

take1_' :: TokenStream (L tok) -> Maybe (L tok, TokenStream (L tok))
take1_' (TsToken tok rest) = Just (tok, rest)
take1_' _                  = Nothing

takeN_' :: Int -> TokenStream (L tok) -> Maybe ([L tok], TokenStream (L tok))
takeN_' n s
  | n <= 0        = Just ([], s)
  | streamEmpty s = Just ([], s)
  | otherwise     = Just (jump n s)
  where
    jump :: Int -> TokenStream (L tok) -> ([L tok], TokenStream (L tok))
    jump _ TsEof          = ([], TsEof)
    jump _ (TsError _)    = ([], TsEof)
    jump 0 (TsToken x xs) = ([], TsToken x xs)
    jump m (TsToken x xs) = let (ys, zs) = jump (m - 1) xs in (x:ys, zs)

takeWhile_' :: (L tok -> Bool) -> TokenStream (L tok) -> ([L tok], TokenStream (L tok))
takeWhile_' p stream = case take1_' stream of
    Nothing        -> ([], stream)
    Just (x, rest) ->
        if p x
          then let (xs, rest') = takeWhile_' p rest in (x:xs, rest')
          else ([], stream)

          reachOffset' :: Show tok => Int
          -> PosState (TokenStream (L tok))
          -> (String, PosState (TokenStream (L tok)))
