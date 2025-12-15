{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)

data Hello = Hello { multiplicator :: Text, rootValue :: Text } deriving (Show, Generic)
instance ToJSON Hello

main :: IO ()
main = scotty 3000 $ do
  get "/exactSquareRoot/:radicand" $ do
    name <- param "radicand"
    json $ Hello { multiplicator = "test", rootValue = name }
