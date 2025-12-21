module Main where
import qualified Websocket as WS ( runWebsocketServer)
import Lib

main :: IO ()
main = WS.runWebsocketServer
