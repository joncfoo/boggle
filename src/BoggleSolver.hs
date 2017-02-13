{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BoggleSolver
  ( adjacent
  , loadDict
  , wordsInBoard
  )
  where

import Protolude
import qualified Prelude
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import qualified Data.Vector as Vector
import Data.Maybe (fromJust)
import Data.Trie (Trie)
import Data.Vector (Vector)


loadDict :: FilePath -> IO Trie
loadDict file = do
  contents <- readFile file
  contents
    & Text.lines
    & filter (\l -> Text.length l >= 3)
    & foldl' addToTrie Trie.empty
    & pure
  where
    addToTrie trie word =
      case Text.find (== '\'') word of
        Nothing -> Trie.insert (toS (Text.toUpper word)) trie
        Just _  -> trie


isWord :: Trie -> Prelude.String -> Bool
isWord dict word =
  Just "" == ((Prelude.head . Trie.toList) <$> Trie.lookupPrefix word dict)


-- linearized adjacent indices
adjacent :: Int -> [Int]
adjacent i =
  let x = i `mod` 4
      y = i `div` 4
  in
    [ (x - 1, y - 1)
    , (x    , y - 1)
    , (x + 1, y - 1)
    , (x - 1, y    )
    , (x + 1, y    )
    , (x - 1, y + 1)
    , (x    , y + 1)
    , (x + 1, y + 1)
    ]
    & filter (\(x, y) -> x > -1 && x < 4 && y > -1 && y < 4)
    & map (\(x, y) -> y * 4 + x)


wordsInBoard :: Trie -> Vector Text -> [Text]
wordsInBoard dict board =
  map (wordsFromIndex dict (map Just board) "" Trie.empty) [0..Vector.length board - 1]
  & map Trie.toList
  & mconcat
  & filter (\s -> length s >= 3 && isWord dict s)
  & map toS
  & sort


wordsFromIndex :: Trie -> Vector (Maybe Text) -> Text -> Trie -> Int -> Trie
wordsFromIndex dict board word trie idx =
  let
    newBoard =
      board Vector.// [(idx, Nothing)]
    adjacentIndices =
      adjacent idx
      & filter (\i -> isJust (newBoard Vector.! i))
    newWord
      = word <> fromJust (board Vector.! idx)
    newWordS =
      toS newWord
  in
    if adjacentIndices == mempty || isNothing (Trie.lookupPrefix newWordS dict)
    then trie
    else
      let
        newTrie =
          Trie.insert newWordS trie
      in
        map (wordsFromIndex dict newBoard newWord newTrie) adjacentIndices
        & map Trie.toList
        & mconcat
        & Trie.fromList
