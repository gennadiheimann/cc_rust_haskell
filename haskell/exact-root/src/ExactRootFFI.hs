{-# LANGUAGE ForeignFunctionInterface #-}

module ExactRootFFI where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import ExactRoot (berechneExacteWurzel, Res(..))

-- C-kompatibles Struct
data CRes = CRes {
  c_multiplikator :: CInt,
  c_wurzelwert :: CInt,
  c_radikand :: CInt
}

instance Storable CRes where
  sizeOf _ = 3 * sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    m <- peekElemOff (castPtr ptr) 0
    w <- peekElemOff (castPtr ptr) 1
    r <- peekElemOff (castPtr ptr) 2
    return $ CRes m w r
  poke ptr (CRes m w r) = do
    pokeElemOff (castPtr ptr) 0 m
    pokeElemOff (castPtr ptr) 1 w
    pokeElemOff (castPtr ptr) 2 r

-- Exportierte Funktion: Array von CRes + Länge
foreign export ccall berechneExacteWurzel_array :: CInt -> Ptr CInt -> IO (Ptr CRes)
berechneExacteWurzel_array :: CInt -> Ptr CInt -> IO (Ptr CRes)
berechneExacteWurzel_array n lenPtr = do
  let results = berechneExacteWurzel (fromIntegral n)
      cresArr = map (\(Res m w r) -> CRes (fromIntegral m) (fromIntegral w) (fromIntegral r)) results
      arrLen = length cresArr
  poke lenPtr (fromIntegral arrLen)
  newArray cresArr