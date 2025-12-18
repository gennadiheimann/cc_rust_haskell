{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as TL 
import qualified ExactRoot as ER ( berechneExacteWurzel, Res ( .. )) 

data ExactSquareRoot = ExactSquareRoot { multiplicator :: TL.Text, rootValue :: TL.Text } deriving (Show, Generic)
instance ToJSON ExactSquareRoot

main :: IO ()
main = scotty 8082 $ do
  get "/exactSquareRoot/:radicand" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    radicandText <- param "radicand"
    let radicandInt = read (TL.unpack radicandText) :: Int
    liftIO $ print ( "Exact Root" ++ show(ER.berechneExacteWurzel radicandInt))
    let result = head $ ER.berechneExacteWurzel radicandInt
    json $ ExactSquareRoot { multiplicator = TL.pack (show $ ER.multiplikator result ), rootValue = TL.pack (show $ ER.wurzelwert result ) }
