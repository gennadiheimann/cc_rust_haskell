-- TODO
-- [ ] - ist radicand im Json ueberfluessig?
-- [ ] - quellcode uebersetzen

module ExactRoot
(
  getExactSqrt,
  Res( .. )
) where
  
import Data.List ( partition )

data Res = Res {
  multiplicator :: Int,
  sqrt :: Int,
  radicand :: Int
} deriving (Eq, Show)

getExactSqrt :: Int -> [Res]
getExactSqrt inputRadikand = do
  if (inputRadikand < 0)
    then calcNegativeSqrt
    else calcPositiveSqrt inputRadikand

calcPositiveSqrt :: Int -> [Res]
calcPositiveSqrt positiveRadicand = do
  let oddNums                    = createOddNumbers positiveRadicand
  let numsUpToRadicandHalf       = getNumsUpToRadicandHalf positiveRadicand
  let standardWerte       = reduziereStandardWerte positiveRadicand (calcDefaultSqrts oddNums)
  let radikandWurzelwerte = zippen numsUpToRadicandHalf standardWerte
  let einfacheWurzelwerte = berechneEinfacheWurzelwert positiveRadicand radikandWurzelwerte
  -- let komplexeWurzelwerte = berechneKomplexeWurzelwert positiveRadicand radikandWurzelwerte
  let komplexeWurzelwerte' = berechneKomplexeWurzelwert' positiveRadicand radikandWurzelwerte
  -- case einfacheWurzelwerte of
  --  Just w  -> Res (-1) w (-1)
  --  Nothing -> case komplexeWurzelwerte of 
  --      Just (m, w)  -> Res m w (-1)
  --      Nothing   -> Res (-1) (-1) radikand
  case einfacheWurzelwerte of 
    Just w -> [Res (-1) w (-1)]
    Nothing -> map (\(r,w) -> Res r w (-1)) komplexeWurzelwerte' 

calcNegativeSqrt :: [Res]
calcNegativeSqrt = [Res (-1) (-1) (-1)]

createOddNumbers :: Int -> [Int]
createOddNumbers radikandForUngeradeZahlen = filter odd [1 .. radikandForUngeradeZahlen]

getNumsUpToRadicandHalf :: Int -> [Int]
getNumsUpToRadicandHalf radikandFornumsUpToRadicandHalf = [2 .. ((radikandFornumsUpToRadicandHalf `quot` 2) +1)]

-- die Berechnung bis Resultat der Addition < Radikand 
calcDefaultSqrts :: [Int] -> [Int]
calcDefaultSqrts  [] = []
calcDefaultSqrts  [_] = []
calcDefaultSqrts  (x:y:xs) = sumOfDefaultValues : calcDefaultSqrts (sumOfDefaultValues : xs)
  where sumOfDefaultValues = x + y

-- TODO
--  error, called at libraries/ghc-internal/src/GHC/Internal/List.hs:2030:3 in ghc-internal:GHC.Internal.List
--  errorEmptyList, called at libraries/ghc-internal/src/GHC/Internal/List.hs:96:11 in ghc-internal:GHC.Internal.List
--  badHead, called at libraries/ghc-internal/src/GHC/Internal/List.hs:90:28 in ghc-internal:GHC.Internal.List
--  head, called at src/ExactRoot.hs:62:56 in exact-square-root-rest-api-server-0.1.0.0-KuYOQvFF0Ty6mV7TJ0bbqb:ExactRoot

reduziereStandardWerte :: Int -> [Int] -> [Int]
reduziereStandardWerte radiKand stWerte = 
  case partition (< radiKand) stWerte of
    (xs, y:_) -> xs ++ [y]
    (xs, [])  -> xs

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