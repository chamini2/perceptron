module Main where

import           Control.Arrow         ((&&&))
import           Control.Monad.Reader  (ReaderT, asks, runReaderT, liftIO)
import           Data.Foldable         (foldlM)
import           Foreign.Marshal.Utils (fromBool)

data Información = Info
    { umbral :: Double
    , tasa   :: Double
    }

informaciónInicial :: Información
informaciónInicial = Info 0.5 0.1

type Lector = ReaderT Información IO

correrLector :: [([Double], Double)] -> IO [Double]
correrLector conjunto = do
    vectorW <- correrRecursivo 100 pesosIniciales conjunto
    mapM_ showTuple $ zip [0..] vectorW
    return vectorW
    where
        showTuple (n,x) = putStrLn $ "W" ++ show n ++ " = " ++ show x

correrRecursivo :: Int -> [Double] -> [([Double], Double)] -> IO [Double]
correrRecursivo it pesos conjunto =
    if it > 0 then do
        pesos' <- runReaderT (perceptrón pesos conjunto) informaciónInicial
        correrRecursivo (it - 1) pesos' conjunto
    else return pesos

--------------------------------------------------------------------------------

pesosIniciales :: [Double]
pesosIniciales = [0, 0, 0]

----------------------------------------
-- Conjuntos de aprendizaje

conjunto_nand :: [([Double], Double)]
conjunto_nand = [ ([1, 0, 0], 1)
                , ([1, 0, 1], 1)
                , ([1, 1, 0], 1)
                , ([1, 1, 1], 0)
                ]

conjunto_and :: [([Double], Double)]
conjunto_and = [ ([1, 0, 0], 0)
               , ([1, 0, 1], 0)
               , ([1, 1, 0], 0)
               , ([1, 1, 1], 1)
               ]

conjunto_or :: [([Double], Double)]
conjunto_or = [ ([1, 0, 0], 0)
              , ([1, 0, 1], 1)
              , ([1, 1, 0], 1)
              , ([1, 1, 1], 1)
              ]

--------------------------------------------------------------------------------

perceptrón :: [Double] -> [([Double], Double)] -> Lector [Double]
perceptrón = foldlM func
    where
        func :: [Double] -> ([Double], Double) -> Lector [Double]
        func vectorW (vectorX, z) = do
            (u, t) <- asks (umbral &&& tasa)
            let s = sum $ zipWith (*) vectorX vectorW
                n = fromBool (s > u)                    -- True == 1, False == 0
                e = z - n
                d = t * e
            -- liftIO . putStrLn $ "error = " ++ show e
            return $ zipWith (+) vectorW $ map (*d) vectorX     -- nuevo vectorW
