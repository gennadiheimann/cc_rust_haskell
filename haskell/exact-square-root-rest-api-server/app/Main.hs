{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as TL 
import qualified ExactRoot as ER ( getExactSqrt ) 

data ExactSquareRoot = ExactSquareRoot 
  { 
    multiplicator :: Integer, 
    squareRoot :: Integer
  } deriving (Show, Generic)
instance ToJSON ExactSquareRoot

main :: IO ()
main = scotty 8082 $ do
  get "/exactSquareRoot/:radicand" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    radicandText <- pathParam "radicand"
    let radicandInt = read (TL.unpack radicandText) :: Integer
    -- liftIO $ print ( "Exact Root" ++ show(ER.berechneExacteWurzel radicandInt))
    json $  [ ExactSquareRoot {  
                                multiplicator = m
                              , squareRoot = r
                              } | (m, r) <- ER.getExactSqrt radicandInt
            ]
