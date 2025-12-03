module ExactRoot
(
  berechneExacteWurzel,
  Res( .. )
) where

data Res = Res {
  multiplikator :: Int,
  wurzelwert :: Int,
  radikand :: Int
} deriving (Eq, Show)

berechneExacteWurzel :: Int -> Res
berechneExacteWurzel radikand = 
  if(radikand < 0)
    then berechneNegativenWurzel
    else berechenPositivenWurzel radikand
  
berechenPositivenWurzel :: Int -> Res
berechenPositivenWurzel radikand = do
  let ungeradeZahlen      = berechneUngeradeZahlen radikand
  let einfacheReihe       = berechneEinfacheReihe radikand
  let standardWerte       = berechneStandardWerte ungeradeZahlen
  let radikandWurzelwerte = zippen einfacheReihe standardWerte
  let einfacheWurzelwerte = berechneEinfacheWurzelwert radikand radikandWurzelwerte
  let komplexeWurzelwerte = berechneKomplexeWurzelwert radikand radikandWurzelwerte
  case einfacheWurzelwerte of
    Just w  -> Res (-1) w (-1)
    Nothing -> case komplexeWurzelwerte of 
        Just (m, w)  -> Res m w (-1)
        Nothing   -> Res (-1) (-1) radikand

berechneNegativenWurzel :: Res
berechneNegativenWurzel = Res (-1) (-1) (-1)

berechneUngeradeZahlen :: Int -> [Int]
berechneUngeradeZahlen radikand = filter odd [1 .. radikand]

berechneEinfacheReihe :: Int -> [Int]
berechneEinfacheReihe radikand = [2 .. ((radikand `quot` 2) +1)]

-- die Berechnung bis Resultat der Addition < Radikand 
berechneStandardWerte :: [Int] -> [Int]
berechneStandardWerte [] = []
berechneStandardWerte [x] = []
berechneStandardWerte (x:y:xs) = x + y : berechneStandardWerte(x + y :xs)

zippen :: [Int] -> [Int] -> [(Int, Int)]
zippen = zip

berechneEinfacheWurzelwert :: Int -> [(Int, Int)] -> Maybe Int
berechneEinfacheWurzelwert radikand radikandWurzelwert = auspacken sucheEinfacheWurzelWert
  where
    auspacken :: [(Int, Int)] -> Maybe Int
    auspacken [(r,_)] = Just r
    auspacken ((_,_):y) = Nothing
    auspacken []  = Nothing
    sucheEinfacheWurzelWert :: [(Int, Int)]
    sucheEinfacheWurzelWert = filter (\(r, w) -> w == radikand) radikandWurzelwert

berechneKomplexeWurzelwert :: Int -> [(Int, Int)] -> Maybe (Int, Int)
berechneKomplexeWurzelwert radikand radikandWurzelwert = auspacken sucheKomplexeWurzelwert
  where
    auspacken :: [(Int, Int)] -> Maybe(Int, Int)
    auspacken [] = Nothing
    auspacken [(r, w)] = Just (r, radikand `quot` w)
    auspacken ((r, w) : y) = Just(r, radikand `quot` w)
    sucheKomplexeWurzelwert :: [(Int, Int)]
    sucheKomplexeWurzelwert = filter(\(r, w) -> (radikand `mod` w) == 0) radikandWurzelwert

