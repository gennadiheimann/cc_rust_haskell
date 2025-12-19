{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as TL 
import qualified ExactRoot as ER ( berechneExacteWurzel, Res ( .. )) 

data ExactSquareRoot = ExactSquareRoot 
  { 
    multiplicator :: Int, 
    rootValue :: Int, 
    radicand:: Int 
  } deriving (Show, Generic)
instance ToJSON ExactSquareRoot

main :: IO ()
main = scotty 8082 $ do
  get "/exactSquareRoot/:radicand" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    radicandText <- pathParam "radicand"
    let radicandInt = read (TL.unpack radicandText) :: Int
    -- liftIO $ print ( "Exact Root" ++ show(ER.berechneExacteWurzel radicandInt))
    json $  [ ExactSquareRoot {  
                                multiplicator = ER.multiplikator result
                              , rootValue = ER.wurzelwert result
                              , radicand = ER.radikand result 
                              } | result <- ER.berechneExacteWurzel radicandInt
            ]
