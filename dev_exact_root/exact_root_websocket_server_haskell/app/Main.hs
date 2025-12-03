{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text (Text)
import Control.Monad (forM_, forever, mzero)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified ExactRoot as ER ( berechneExacteWurzel, Res ( .. )) 
import Data.Aeson
import qualified Data.ByteString.Lazy as LB

type Clients = [WS.Connection]

-- newClient
newClients :: Clients
newClients = []

-- Get the number of active clients:

-- new addClient
addClient :: WS.Connection -> Clients -> Clients
addClient conn clients = conn : clients

-- {"jsonrpc":"2.0","method":"exact_root","params":{"radikand":50},"id":1}
-- {"jsonrpc":"2.0","result":{"multiplikator":5,"wurzelwert":2},"id":1}

data Response = Response {
  response_jsonrpc :: String,
  response_result :: [Result'],
  response_id :: Int
} deriving (Show)

data Result' = Result' {
  result_multiplikator :: Int,
  result_wurzelwert :: Int,
  result_radikand :: Int
} deriving (Show)

data Request = Request {
  request_jsonrpc :: String,
  request_method :: String,
  request_params :: Params,
  request_id :: Int
} deriving (Show)

newtype Params = Params {
  params_radikand :: Int
} deriving (Show)

instance FromJSON Request where
 parseJSON (Object v) =
    Request <$> v .: "jsonrpc"
           <*> v .: "method"
           <*> v .: "params"
           <*> v .: "id"
 parseJSON _ = mzero

instance FromJSON Params where 
  parseJSON (Object v) = 
    Params <$> v .: "radikand"
  parseJSON _ = mzero

instance ToJSON Response where
 toJSON (Response response_jsonrpc response_result response_id) =
    object [ "jsonrpc"  .= response_jsonrpc
           , "result"   .= response_result
           , "id"       .= response_id
             ]

instance ToJSON Result' where
  toJSON (Result' result_multiplikator result_wurzelwert result_radikand) = 
    object [ "multiplikator" .= result_multiplikator
           , "wurzelwert" .= result_wurzelwert
           , "radikand" .= result_radikand
    ]
main :: IO ()
main = do
--    state <- newMVar newServerState
    T.putStrLn "Server started."
    
    clients <- newMVar newClients
    WS.runServer "127.0.0.1" 9160 $ application clients

application :: MVar Clients -> WS.ServerApp

application clients pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
      modifyMVar_ clients $ \c -> do
        let cl = addClient conn c
        T.putStrLn "Client Connected."
        return cl
      -- WS.sendTextData conn ("Client connected" :: Text)
      talk conn clients

talk :: WS.Connection -> MVar Clients -> IO ()
talk conn clients = forever $ do
  msg <- WS.receiveData conn
  print msg
  handleMsg (decodeJson msg) conn

handleMsg :: Maybe Request -> WS.Connection -> IO ()
handleMsg jObject conn = case jObject of
  Just obj | isExactRoot $ request_method obj -> 
               WS.sendTextData conn (convertToJsonResponse (params_radikand (request_params obj)))
           | otherwise -> WS.sendTextData conn ("Unbekannte Anfrage" :: Text)
  Nothing -> WS.sendTextData conn ("Unbekante Anfrage" :: Text)

convertToJsonResponse :: Int -> LB.ByteString
convertToJsonResponse radicand = encode (
  Response {
    response_jsonrpc = "2.0",
    -- response_result = Result' multiplier wurzelwert radikand,
    response_result = mapExactRoot exactRoot, 
    response_id = 1
  })
  where 
    -- multiplier = getMuliplier exactRoot
    -- wurzelwert = getSqrt exactRoot
    -- radikand = getRadikand exactRoot
    exactRoot = execExactRoot radicand
    mapExactRoot :: [ER.Res] -> [Result']
    mapExactRoot res = map (\(ER.Res m w r) -> Result' m w r) res

isExactRoot :: String -> Bool
isExactRoot method =
  method == "exact_root"

decodeJson :: Text -> Maybe Request
decodeJson msg = decode (WS.toLazyByteString msg) :: Maybe Request

execExactRoot :: Int -> [ER.Res]
execExactRoot = ER.berechneExacteWurzel

getMuliplier :: ER.Res -> Int
getMuliplier = ER.multiplikator 

getSqrt :: ER.Res -> Int
getSqrt = ER.wurzelwert

getRadikand :: ER.Res -> Int
getRadikand = ER.radikand