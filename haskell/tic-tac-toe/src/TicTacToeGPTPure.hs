
-- Pure Datenstrukturen

data Player = X | O deriving (Eq, Show)

type Board = [Maybe Player]

data GameState = GameState
  { board   :: Board
  , current :: Player
  } deriving Show

-- Pure State Machine Step Function

stepGame :: Int -> GameState -> (Either String GameState, GameStatus)
data GameStatus = Running | Won Player | Draw deriving Show

stepGame move gs =
  case makeMovePure move gs of
    Left err -> (Left err, Running)
    Right gs' ->
      let p = switch (current gs') -- vorheriger Spieler
      in if checkWin (board gs') p
           then (Right gs', Won p)
           else if isDraw (board gs')
                then (Right gs', Draw)
                else (Right gs', Running)

-- Gewinnlogik (PURE)

wins :: [[Int]]
wins =
  [ [0,1,2],[3,4,5],[6,7,8]
  , [0,3,6],[1,4,7],[2,5,8]
  , [0,4,8],[2,4,6]
  ]

checkWin :: Board -> Player -> Bool
checkWin b p = any (all (\i -> b !! i == Just p)) wins

isDraw :: Board -> Bool
isDraw = all (/= Nothing)

-- Reine Spiellogik (PURE, KEIN IO)

initState :: GameState
initState = GameState (replicate 9 Nothing) X

switch :: Player -> Player
switch X = O
switch O = X

makeMovePure :: Int -> GameState -> Either String GameState
makeMovePure i gs
  | i < 0 || i > 8 = Left "Ungültiges Feld"
  | board gs !! i /= Nothing = Left "Feld belegt"
  | otherwise =
      Right gs
        { board = take i (board gs) ++ [Just (current gs)] ++ drop (i+1) (board gs)
        , current = switch (current gs)
        }



