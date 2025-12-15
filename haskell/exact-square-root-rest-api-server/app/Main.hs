{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as TL 
import qualified ExactRoot as ER ( berechneExacteWurzel, Res ( .. )) 

data Hello = Hello { multiplicator :: TL.Text, rootValue :: TL.Text } deriving (Show, Generic)
instance ToJSON Hello

main :: IO ()
main = scotty 8082 $ do
  get "/exactSquareRoot/:radicand" $ do
    radicandText <- param "radicand"
    let radicandInt = read (TL.unpack radicandText) :: Int
    liftIO $ print ( "Exact Root" ++ show(ER.berechneExacteWurzel radicandInt))
    json $ Hello { multiplicator = "test", rootValue = radicandText }
