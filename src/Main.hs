{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Protolude hiding (get)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Network.HTTP.Types.Status (badRequest400)
import Web.Scotty

import BoggleSolver (loadDict, wordsInBoard)


main :: IO ()
main = do
  dict <- loadDict "/usr/share/dict/words"

  scotty 5000 $ do
    get "/api/:board" $ do
      board <- (mkBoard . Text.toUpper) <$> param "board"

      if length board /= 16
        then do
          status badRequest400
          text "Invalid board"
        else wordsInBoard dict board
             & Text.unlines
             & toSL
             & text


mkBoard :: Text -> Vector Text
mkBoard word =
  case Text.splitOn "QU" word of
    [prefix, suffix] ->
      Vector.concat [textToVector prefix, Vector.singleton "QU", textToVector suffix]
    _ ->
      textToVector word
  where
    textToVector =
      Text.foldl' (\v c -> Vector.snoc v (Text.singleton c)) Vector.empty
