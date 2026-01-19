module ExactRoot(getExactSqrt) where

import Prelude hiding (odd, sqrt)

getExactSqrt :: Integer -> [(Integer, Integer)]
getExactSqrt inputRadikand = do
  if (inputRadikand < 0)
    then [((-1), (-1))]
    else 
      if inputRadikand == 0 || inputRadikand == 1
        then [((-1), inputRadikand)]
        else calcPositiveSqrt inputRadikand

calcPositiveSqrt :: Integer -> [(Integer, Integer)]
calcPositiveSqrt positiveRadicand = do
  let radicandAndSqrt      = getSqrtAndRadicand positiveRadicand
  let simpleSqrt           = searchSimpleSqrt positiveRadicand radicandAndSqrt
  let complexSqrt          = searchComplexSqrt positiveRadicand radicandAndSqrt
  case simpleSqrt of 
    Just w -> [((-1), w)]
    Nothing -> map (\(r,w) -> (r, w)) complexSqrt

getSqrtAndRadicand :: Integer -> [(Integer, Integer)]
getSqrtAndRadicand radicand = go 1 1 1 []
  where
    go odd acc sqrt result
        | acc >= radicand = reverse result
        | otherwise =
            let odd'  = odd + 2
                acc'  = acc + odd'
                sqrt' = sqrt + 1
            in go odd' acc' sqrt' ((sqrt', acc') : result)

searchSimpleSqrt :: Integer -> [(Integer, Integer)] -> Maybe Integer
searchSimpleSqrt rForSimple radicandAndSqrt = unpuck (filter (\(_, b) -> b == rForSimple) radicandAndSqrt)
  where
    unpuck [(a,_)] = Just a
    unpuck ((_,_):_) = Nothing
    unpuck []  = Nothing

searchComplexSqrt :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
searchComplexSqrt rForComplex radicandAndSqrt = unpuck (filter(\(_, b) -> (rForComplex `mod` b) == 0) radicandAndSqrt)
  where
    unpuck [] = []
    unpuck [(a, b)] = [(a, rForComplex `quot` b)]
    unpuck ((a, b) : c) = (a, rForComplex `quot` b): unpuck c