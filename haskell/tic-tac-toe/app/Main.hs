module Main (main) where

import Control.Monad.State
import Data.List (intercalate)

import TicTacToeGPTPure

-- IO Layer (nur UI!)
{-
drop n xs entfernt die ersten n Elemente einer Liste und gibt den Rest zurück.
-}
{- 
printBoard :: Board -> IO ()
printBoard b = putStrLn $ intercalate "\n" rows
  where
    showCell Nothing  = " "
    showCell (Just p) = show p
    rows = [row 0, row 3, row 6]
    row i = " " ++ intercalate " | " (map showCell (take 3 (drop i b))) ++ " "
-}

getBoardString :: Board -> String
getBoardString b = intercalate "\n" rows
  where
    showCell Nothing  = " "
    showCell (Just p) = show p
    rows = [row 0, row 3, row 6]
    row i = "| " ++ intercalate " | " (map showCell (take 3 (drop i b))) ++ " |"

{-
StataT State Monad + IO Monad
lift -> State  -> IO
reads -> Parse einen Wert vom Anfang des Strings und gib den restlichen String zurück.
-}

printToConsole :: String -> StateT GameState IO()
printToConsole s = liftIO $ putStrLn s

-- gameLoop :: StateT GameState IO ()
-- gameLoop = do
--   gs <- get
--   -- liftIO $ printBoard (board gs)
--   printToConsole (getBoardString (board gs))
--   printToConsole "State: PlayerTurn"
--   printToConsole ("Spieler " ++ show (current gs) ++ " (1-9):")
--   input <- liftIO getLine

--   case reads input of
--     [(n,"")] -> do
--       let (res, status) = stepGame (n-1) gs
--       case res of
--         Left err -> printToConsole err >> gameLoop
--         Right gs' -> do
--           put gs'
--           case status of
--             Running -> gameLoop
--             Won p   -> printToConsole ("Gewonnen: " ++ show p)
--             Draw    -> printToConsole "Unentschieden!"
--             GameOver -> printToConsole "Spiel beendet!"
--             Report  -> printToConsole "Spielbericht"
--     _ -> (printToConsole "Ungültige Eingabe") >> gameLoop

gameLoop :: StateT GameState IO ()
gameLoop = do
  gs <- get
  -- liftIO $ printBoard (board gs)
  printToConsole (getBoardString (board gs))
  printToConsole "State: PlayerTurn"
  printToConsole ("Spieler " ++ show (current gs) ++ " (1-9):")
  input <- liftIO getLine

  case reads input of
    [(n,"")] -> do
      let gs' = stepGame' (n-1) gs
      case gs' of
        GameState{state = GameError err} -> printToConsole err >> gameLoop
        gs'@GameState{state = PlayerTurn} -> do
          put gs'
          -- case status of
          --   Running -> gameLoop
          --   Won p   -> printToConsole ("Gewonnen: " ++ show p)
          --   Draw    -> printToConsole "Unentschieden!"
          --   GameOver -> printToConsole "Spiel beendet!"
          --   Report  -> printToConsole "Spielbericht"
    _ -> (printToConsole "Ungültige Eingabe") >> gameLoop

main :: IO ()
main = evalStateT gameLoop initState