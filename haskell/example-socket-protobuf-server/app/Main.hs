{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Socket
import qualified Network.Socket.ByteString as NSB
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BL
import Data.ProtoLens.Encoding (decodeMessage)
import Proto.Proto.HelloWorld_Fields -- automatisch generiert
import Proto.Proto.HelloWorld -- automatisch generiert
import qualified Proto.Proto.HelloWorld_Fields as Fields
import Lens.Micro ((^.))

main :: IO ()
main = withSocketsDo $ do
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4000")
    sock <- socket (addrFamily addr) Stream defaultProtocol
    bind sock (addrAddress addr)
    listen sock 1
    putStrLn "Server wartet auf Nachricht..."
    (conn, _) <- accept sock
    msg <- NSB.recv conn 1024
    case decodeMessage msg of
      Left err -> putStrLn $ "Decode error: " ++ show err
      Right chatMsg ->
        let chatMsg' = chatMsg :: ChatMessage
        in putStrLn $ "Empfangen: " ++ show (chatMsg ^. Fields.text) ++ " (id=" ++ show (chatMsg ^. Fields.id) ++ ")"
    close conn
    close sock