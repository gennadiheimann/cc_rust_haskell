module QuadEquation (
  berechneZweiNullStellen
  , berechneEineNullStelle
  , ermittleDiskriminante
  , berechneWurzelVonDiskriment
  , berechne
  , Form(..)
  , ErgebnisDiskriminante (..)
  , AnzahlNullStellen (..)
  , Ergebnis (..)
  , ErgebnisX1 (..)
  , ErgebnisX2 (..)
) where

import qualified ExactRoot as ER (berechneExacteWurzel, Res ( .. ))
import qualified Data.Maybe as DM
import qualified Data.Ratio as DR
import ExactRoot (Res(multiplikator))

{--
ax² + bx + c = 0

       -b +- sqrt(D)
x12 = ----------------------
              2a
D = b² - 4ac

D == 0 x1
D < 0 
D > 0 x1/2

m * sqrt(D)
sqrt(D)

Devidend
---------
Devisor

--}
-- berecheDiskriminante gibt Anzahl der Nullstellen zureuck
data AnzahlNullStellen =  Keine | Eine | Zwei | Ungueltig deriving (Show, Eq)

data Form = F {
  f_a :: Int,
  f_b :: Int,
  f_c :: Int
}
-- Splitten auf ErgebnisEineNullstelle und ErgebnissZweiNullstellen
data Ergebnis = E {
  e_x1 :: ErgebnisX1,
  e_x2 :: ErgebnisX2
} deriving (Eq, Show)

-- Alle Int to Rational ersetzen
data ErgebnisX1 = EX1 {
  ex1_radikand :: Maybe Int,
  ex1_miltiplikator :: Maybe Rational,
  ex1_b :: Maybe Int,
  ex1_DSqrt :: Maybe Int,
  ex1_a :: Maybe Int
} deriving (Eq, Show)

data ErgebnisX2 = EX2 {
  ex2_radikand :: Maybe Int,
  ex2_miltiplikator :: Maybe Rational,
  ex2_b :: Maybe Int,
  ex2_DSqrt :: Maybe Int,
  ex2_a :: Maybe Int
} deriving (Eq, Show)

data ErgebnisDiskriminante = ED {
  ed_diskriminante :: Int,
  ed_anzahlNullstellen :: AnzahlNullStellen
} deriving (Show, Eq)

berechne :: Form -> Ergebnis
berechne form = case ermittleDiskriminante form of
    ED d Eine -> berechneEineNullStelle form
    ED d Keine -> undefined
    ED d Zwei -> berechneZweiNullStellen form (berechneWurzelVonDiskriment d)
    ED d Ungueltig -> undefined

ermittleDiskriminante :: Form -> ErgebnisDiskriminante
ermittleDiskriminante f = pruefeDiskriminate berechneDiskriminante
  where b = f_b f
        a = f_a f
        c = f_c f
        berechneDiskriminante :: Int
        berechneDiskriminante = (b * b) - (4 * a * c)
        pruefeDiskriminate :: Int -> ErgebnisDiskriminante
        pruefeDiskriminate d
          | d == 0 = ED d Eine
          | d <  0 = ED d Keine
          | d >  0 = ED d Zwei
          | otherwise = ED d Ungueltig

berechneWurzelVonDiskriment :: Int -> ER.Res
berechneWurzelVonDiskriment = ER.berechneExacteWurzel

{- 
Wenn wurzelwert > 0 (Just) rechne mit wurzelwert
Wenn radikand > 0 (Just) rechne mit multiplikator und schleppe radikand mit
-}
berechneZweiNullStellen :: Form -> ER.Res-> Ergebnis
berechneZweiNullStellen f d
  | dMult == (-1) && dWurzelWert >= 1 && dRadikand == (-1) = do -- Einfache Wurzelberechnung rechne mit dWurzelwert
    let x1 = toInteger ((-1) * b + dWurzelWert) DR.% toInteger (2 * a)
    let x2 = toInteger ((-1) * b - dWurzelWert) DR.% toInteger (2 * a)
    E (
        EX1 Nothing (Just x1) Nothing Nothing Nothing
      ) 
      (
        EX2 Nothing (Just x2) Nothing Nothing Nothing
      )
  | dMult >= 1 && dWurzelWert >= 1 && dRadikand == (-1) = do -- rechne mit Multiplikator und Wurzelwert mitschleppen
    let x1b = Just ((-1) * b)
    let x1a = Just (2 * a)
    let x2b = Just ((-1) * b)
    let x2a = Just (2 * a)
    E (
        EX1 Nothing (Just(toInteger dMult DR.% 1)) x1b (Just dWurzelWert) x1a
      ) 
      (
        EX2 Nothing (Just(toInteger dMult DR.% 1)) x2b (Just dWurzelWert) x2a
      )
    
  | dMult == (-1) && dWurzelWert == (-1) && dRadikand >= 1 = do -- rechne mit Multiplikator = 1 und Radikand mitschleppen
    let x1b = Just ((-1) * b)
    let x1a = Just (2 * a)
    let x2b = Just ((-1) * b)
    let x2a = Just (2 * a)
    E (
        EX1 Nothing Nothing x1b (Just dRadikand) x1a
      ) 
      (
        EX2 Nothing Nothing x2b (Just dRadikand) x2a
      )
  | otherwise = undefined
  where a = f_a f
        b = f_b f
        c = f_c f
        dMult = ER.multiplikator d
        dRadikand = ER.radikand d
        dWurzelWert = ER.wurzelwert d

berechneEineNullStelle :: Form -> Ergebnis
berechneEineNullStelle f = do
  let x = toInteger ((-1) * b) DR.% toInteger (2 * a)
  E (
      EX1 Nothing (Just x) Nothing Nothing Nothing
    ) 
    (
      EX2 Nothing Nothing Nothing Nothing Nothing
    )
  where a = f_a f
        b = f_b f
        c = f_c f