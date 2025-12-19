-- TODO
-- [ ] - ist radicand im Json ueberfluessig?
-- [ ] - quellcode uebersetzen

module ExactRoot
(
  berechneExacteWurzel,
  Res( .. )
) where
  
import Data.List ( partition )

data Res = Res {
  multiplikator :: Int,
  wurzelwert :: Int,
  radikand :: Int
} deriving (Eq, Show)

berechneExacteWurzel :: Int -> [Res]
berechneExacteWurzel inputRadikand = do
  if (inputRadikand < 0)
    then berechneNegativenWurzel
    else berechenPositivenWurzel inputRadikand

berechenPositivenWurzel :: Int -> [Res]
berechenPositivenWurzel positiveRadikand = do
  let ungeradeZahlen      = berechneUngeradeZahlen positiveRadikand
  let einfacheReihe       = berechneEinfacheReihe positiveRadikand
  let standardWerte       = reduziereStandardWerte positiveRadikand (berechneStandardWerte ungeradeZahlen)
  let radikandWurzelwerte = zippen einfacheReihe standardWerte
  let einfacheWurzelwerte = berechneEinfacheWurzelwert positiveRadikand radikandWurzelwerte
  -- let komplexeWurzelwerte = berechneKomplexeWurzelwert positiveRadikand radikandWurzelwerte
  let komplexeWurzelwerte' = berechneKomplexeWurzelwert' positiveRadikand radikandWurzelwerte
  -- case einfacheWurzelwerte of
  --  Just w  -> Res (-1) w (-1)
  --  Nothing -> case komplexeWurzelwerte of 
  --      Just (m, w)  -> Res m w (-1)
  --      Nothing   -> Res (-1) (-1) radikand
  case einfacheWurzelwerte of 
    Just w -> [Res (-1) w (-1)]
    Nothing -> map (\(r,w) -> Res r w (-1)) komplexeWurzelwerte' 

berechneNegativenWurzel :: [Res]
berechneNegativenWurzel = [Res (-1) (-1) (-1)]

berechneUngeradeZahlen :: Int -> [Int]
berechneUngeradeZahlen radikandForUngeradeZahlen = filter odd [1 .. radikandForUngeradeZahlen]

berechneEinfacheReihe :: Int -> [Int]
berechneEinfacheReihe radikandForEinfacheReihe = [2 .. ((radikandForEinfacheReihe `quot` 2) +1)]

-- die Berechnung bis Resultat der Addition < Radikand 
berechneStandardWerte :: [Int] -> [Int]
berechneStandardWerte  [] = []
berechneStandardWerte  [_] = []
berechneStandardWerte  (x:y:xs) = sumOfDefaultValues : berechneStandardWerte (sumOfDefaultValues : xs)
  where sumOfDefaultValues = x + y

-- TODO
--  error, called at libraries/ghc-internal/src/GHC/Internal/List.hs:2030:3 in ghc-internal:GHC.Internal.List
--  errorEmptyList, called at libraries/ghc-internal/src/GHC/Internal/List.hs:96:11 in ghc-internal:GHC.Internal.List
--  badHead, called at libraries/ghc-internal/src/GHC/Internal/List.hs:90:28 in ghc-internal:GHC.Internal.List
--  head, called at src/ExactRoot.hs:62:56 in exact-square-root-rest-api-server-0.1.0.0-KuYOQvFF0Ty6mV7TJ0bbqb:ExactRoot

reduziereStandardWerte :: Int -> [Int] -> [Int]
reduziereStandardWerte radiKand stWerte = fst part ++ [head (snd part)]
  where part = partition (< radiKand) stWerte 

zippen :: [Int] -> [Int] -> [(Int, Int)]
zippen = zip

berechneEinfacheWurzelwert :: Int -> [(Int, Int)] -> Maybe Int
berechneEinfacheWurzelwert radikandForEinfacheWurzelwert radikandWurzelwert = auspacken sucheEinfacheWurzelWert
  where
    auspacken :: [(Int, Int)] -> Maybe Int
    auspacken [(r,_)] = Just r
    auspacken ((_,_):_) = Nothing
    auspacken []  = Nothing
    sucheEinfacheWurzelWert :: [(Int, Int)]
    sucheEinfacheWurzelWert = filter (\(_, w) -> w == radikandForEinfacheWurzelwert) radikandWurzelwert

-- berechneKomplexeWurzelwert :: Int -> [(Int, Int)] -> Maybe (Int, Int)
-- berechneKomplexeWurzelwert radikand radikandWurzelwert = auspacken sucheKomplexeWurzelwert
--   where
--     auspacken :: [(Int, Int)] -> Maybe(Int, Int)
--     auspacken [] = Nothing
--     auspacken [(r, w)] = Just (r, radikand `quot` w)
--     auspacken ((r, w) : y) = Just(r, radikand `quot` w)
--     sucheKomplexeWurzelwert :: [(Int, Int)]
--     sucheKomplexeWurzelwert = filter(\(r, w) -> (radikand `mod` w) == 0) radikandWurzelwert

berechneKomplexeWurzelwert' :: Int -> [(Int, Int)] -> [(Int, Int)]
berechneKomplexeWurzelwert' radikandForKomplexeWurzelwerte radikandWurzelwert = auspacken sucheKomplexeWurzelwert
  where
    auspacken :: [(Int, Int)] -> [(Int, Int)]
    auspacken [] = []
    auspacken [(r, w)] = [(r, radikandForKomplexeWurzelwerte `quot` w)]
    auspacken ((r, w) : y) = (r, radikandForKomplexeWurzelwerte `quot` w): auspacken y
    sucheKomplexeWurzelwert :: [(Int, Int)]
    sucheKomplexeWurzelwert = filter(\(_, w) -> (radikandForKomplexeWurzelwerte `mod` w) == 0) radikandWurzelwert