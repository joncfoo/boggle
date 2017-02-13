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


-- load a file with words into a Trie
loadDict :: FilePath -> IO Trie
loadDict file = do
  contents <- readFile file
  contents
    & Text.lines                         -- split on newlines
    & filter (\l -> Text.length l >= 3)  -- only use words with length 3 or higher
    & foldl' addToTrie Trie.empty        -- build up a Trie starting with an empty one
    & pure                               -- (kind of like 'return' in other languages)
  where
    addToTrie trie word =
      -- only add word to Trie if it doesn't include apostrophes
      case Text.find (== '\'') word of
        Nothing -> Trie.insert (toS (Text.toUpper word)) trie
        Just _  -> trie


isWord :: Trie -> Prelude.String -> Bool
isWord dict word =
  -- a word is considered to be contained in the Trie if a sub-trie contains an
  -- empty string as part of possible suffixes under the sub-trie
  Just "" == ((Prelude.head . Trie.toList) <$> Trie.lookupPrefix word dict)


-- linearized adjacent indices
adjacent :: Int -> [Int]
adjacent i =
  let x = i `mod` 4
      y = i `div` 4
  in
    -- calculate adjacent cell indexes as if they were contained in a matrix
    [ (x - 1, y - 1)
    , (x    , y - 1)
    , (x + 1, y - 1)
    , (x - 1, y    )
    , (x + 1, y    )
    , (x - 1, y + 1)
    , (x    , y + 1)
    , (x + 1, y + 1)
    ]
    -- only include indexes contained in the board
    & filter (\(x, y) -> x > -1 && x < 4 && y > -1 && y < 4)
    -- convert matrix to linear coordinates
    & map (\(x, y) -> y * 4 + x)


wordsInBoard :: Trie -> Vector Text -> [Text]
wordsInBoard dict board =
  -- get possible words starting at each index in the board
  map (wordsFromIndex dict (map Just board) "" Trie.empty) [0..Vector.length board - 1]
  & map Trie.toList  -- convert each trie to a list of words
  & mconcat          -- flatten the list so that it only contains words [[word+]+] -> [word+]
  -- ensure word is contained in the dictionary
  & filter (\s -> length s >= 3 && isWord dict s)
  & map toS  -- convert Text to String (different data types)
  & sort


-- recursively builds a Trie containing possible prefixes from a given index of the board
wordsFromIndex :: Trie -> Vector (Maybe Text) -> Text -> Trie -> Int -> Trie
wordsFromIndex dict board word trie idx =
  let
    newBoard =
      -- update board so that we can no longer revisit the current index for successive calls
      board Vector.// [(idx, Nothing)]
    adjacentIndices =
      -- find adjacent indices that can be visited in the board
      adjacent idx
      & filter (\i -> isJust (newBoard Vector.! i))
    newWord =
      -- add current index to word
      word <> fromJust (board Vector.! idx)
    newWordS =
      -- convert word to String (Trie library requirement)
      toS newWord
      -- new Trie formed by adding the new word
    newTrie =
      Trie.insert newWordS trie
  in
    -- if no indexes adjacent to the current index are visit-able then return the existing Trie
    -- also return if the dictionary does not have the prefix formed by the new word
    if adjacentIndices == mempty || isNothing (Trie.lookupPrefix newWordS dict)
    then trie
    else
      -- recursively build a list of Tries by visiting adjacent indices and
      -- seeing if appending the letter at each index to the existing word forms
      -- a prefix contained in the dictionary
      map (wordsFromIndex dict newBoard newWord newTrie) adjacentIndices
      & map Trie.toList  -- convert each Trie to a list of words
      & mconcat          -- flatten the list of list of words [[word+]+] -> [word+]
      & Trie.fromList    -- return the list of words as a Trie
