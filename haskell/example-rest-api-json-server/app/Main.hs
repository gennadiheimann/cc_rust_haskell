{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)

data Hello = Hello { message :: Text } deriving (Show, Generic)
instance ToJSON Hello

main :: IO ()
main = scotty 3000 $ do
  get "/hello/:name" $ do
    name <- param "name"
    json $ Hello { message = "Hello, " <> name <> "!" }
