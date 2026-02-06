import Control.Monad.State
import Data.List (intercalate)

-- IO Layer (nur UI!)
{-
drop n xs entfernt die ersten n Elemente einer Liste und gibt den Rest zurück.
-}
printBoard :: Board -> IO ()
printBoard b = putStrLn $ intercalate "\n" rows
  where
    showCell Nothing  = " "
    showCell (Just p) = show p
    rows = [row 0, row 3, row 6]
    row i = " " ++ intercalate " | " (map showCell (take 3 (drop i b))) ++ " "

{-
StataT State Monad + IO Monad
lift -> State  -> IO
reads -> Parse einen Wert vom Anfang des Strings und gib den restlichen String zurück.
-}

gameLoop :: StateT GameState IO ()
gameLoop = do
  gs <- get
  liftIO $ printBoard (board gs)
  liftIO $ putStrLn $ "Spieler " ++ show (current gs) ++ " (1-9):"
  input <- liftIO getLine

  case reads input of
    [(n,"")] -> do
      let (res, status) = stepGame (n-1) gs
      case res of
        Left err -> liftIO (putStrLn err) >> gameLoop
        Right gs' -> do
          put gs'
          case status of
            Running -> gameLoop
            Won p   -> liftIO $ putStrLn $ "Gewonnen: " ++ show p
            Draw    -> liftIO $ putStrLn "Unentschieden!"
    _ -> liftIO (putStrLn "Ungültige Eingabe") >> gameLoop

main :: IO ()
main = evalStateT gameLoop initState