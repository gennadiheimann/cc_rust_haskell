{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.State
import Data.List (intercalate)

-- Spieler
data Player = X | O deriving (Eq, Show)

-- Spielfeld: 9 Felder
type Board = [Maybe Player]

-- Spielzustand
data GameState = GameState
  { board  :: Board
  , current :: Player
  }

-- Initialer Zustand
initState :: GameState
initState = GameState (replicate 9 Nothing) X

-- Spielfeld anzeigen
printBoard :: Board -> IO ()
printBoard b = putStrLn $ intercalate "\n" $ rows b
  where
    showCell Nothing  = " "
    showCell (Just p) = show p
    rows xs = [row 0, row 3, row 6]
      where
        row i = " " ++ intercalate " | " (map showCell (take 3 (drop i xs))) ++ " "

-- Gewinnmuster
wins :: [[Int]]
wins =
  [ [0,1,2],[3,4,5],[6,7,8]
  , [0,3,6],[1,4,7],[2,5,8]
  , [0,4,8],[2,4,6]
  ]

checkWin :: Board -> Player -> Bool
checkWin b p = any (all (\i -> b !! i == Just p)) wins

-- Spielzug ausführen
makeMove :: Int -> StateT GameState IO ()
makeMove i = do
  gs <- get
  let b = board gs
      p = current gs

  if b !! i /= Nothing
    then liftIO (putStrLn "Feld belegt!") >> gameLoop
    else do
      let b' = take i b ++ [Just p] ++ drop (i+1) b
      put gs { board = b', current = switch p }
      liftIO (printBoard b')
      if checkWin b' p
        then liftIO $ putStrLn $ "Spieler " ++ show p ++ " gewinnt!"
        else gameLoop

-- Spieler wechseln
switch :: Player -> Player
switch X = O
switch O = X

-- Spielschleife (State Monad)
gameLoop :: StateT GameState IO ()
gameLoop = do
  gs <- get
  liftIO $ putStrLn $ "\nSpieler " ++ show (current gs) ++ " (1-9):"
  input <- liftIO getLine
  case reads input of
    [(n, "")] | n >= 1 && n <= 9 -> makeMove (n-1)
    _ -> liftIO (putStrLn "Ungültige Eingabe!") >> gameLoop

-- Main
main :: IO ()
main = evalStateT gameLoop initState