module Main where

import           Control.Arrow                          (second, (&&&))
import           Control.Monad                          (forM_, liftM, when)
import           Control.Monad.Reader                   (ReaderT, asks, liftIO,
                                                         runReaderT)
import           Data.Foldable                          (foldlM)
import           Data.List                              (genericLength)
import           Data.Traversable                       (forM)
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Directory                       (doesFileExist,
                                                         removeFile)

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

fromBool :: Num a => Bool -> a
fromBool True  = 1
fromBool False = 0

--------------------------------------------------------------------------------

-- 1.a)
pregunta1_a :: IO ()
pregunta1_a = do
    doesFileExist "pregunta1_a.txt" >>= flip when (removeFile "pregunta1_a.txt")
    erroress <- forM [(conjunto_and, "AND"), (conjunto_or, "OR"), (conjunto_xor, "XOR")] $ \(conjunto, nombre) -> do
        (pesos, errores) <- correrLector infoInicial conjunto
        appendFile "pregunta1_a.txt" $ "pesos       " ++ nombre ++ ": " ++ show pesos            ++ "\n"
        appendFile "pregunta1_a.txt" $ "iteraciones " ++ nombre ++ ": " ++ show (length errores) ++ "\n"
        appendFile "pregunta1_a.txt" $ "errores     " ++ nombre ++ ": " ++ show errores          ++ "\n\n"
        return (errores, nombre)
    -- grafica
    toFile def "pregunta1_a.png" $ do
        layout_title .= "Error para perceptrón"
        forM_ erroress $ \(errores, nombre) -> do
            plot (line nombre  [errores])

-- 1.b)
pregunta1_b :: IO ()
pregunta1_b = do
    doesFileExist "pregunta1_b.txt" >>= flip when (removeFile "pregunta1_b.txt")
    forM_ [0.01, 0.1, 0.2, 0.5, 0.99] $ \t -> do
        appendFile "pregunta1_b.txt" $ "TASA: " ++ show t ++ "\n"
        forM [(conjunto_and, "AND"), (conjunto_or, "OR")] $ \(conjunto, nombre) -> do
            (pesos, errores) <- correrLector infoInicial { tasa = t } conjunto
            appendFile "pregunta1_b.txt" $ "\tpesos       " ++ nombre ++ ": " ++ show pesos            ++ "\n"
            appendFile "pregunta1_b.txt" $ "\titeraciones " ++ nombre ++ ": " ++ show (length errores) ++ "\n"
            appendFile "pregunta1_b.txt" $ "\terrores     " ++ nombre ++ ": " ++ show errores          ++ "\n\n"


--------------------------------------------------------------------------------

data Información = Info
    { umbral :: Double
    , tasa   :: Double
    }

infoInicial :: Información
infoInicial = Info 0.5 0.1

type Lector = ReaderT Información IO

--------------------------------------------------------------------------------

correrLector :: Información -> Conjunto -> IO ([Double], [(Double, Double)])
correrLector info = liftM (second func) . correrRecursivo 10 pesosIniciales [] info
    where
        func = zip [1..] . map sum . reverse

correrRecursivo :: Int -> [Double] -> [[Double]] -> Información -> Conjunto -> IO ([Double], [[Double]])
correrRecursivo it pesos errores info conjunto =
    if it > 0 then do
        (pesos', errores') <- runReaderT (perceptrón pesos conjunto) info
        -- cuando no hubo errores
        if all (==0) errores'
          then return (pesos', errores' : errores)
          else correrRecursivo (pred it) pesos' (errores' : errores) info conjunto
    else return (pesos, errores)

--------------------------------------------------------------------------------

pesosIniciales :: [Double]
pesosIniciales = repeat 0

----------------------------------------
-- Conjuntos de aprendizaje

type Conjunto = [([Bool], Bool)]

conjunto_and :: Conjunto
conjunto_and = [ ([ True, False, False], False)
               , ([ True, False,  True], False)
               , ([ True,  True, False], False)
               , ([ True,  True,  True],  True) ]

conjunto_nand :: Conjunto
conjunto_nand = [ ([ True, False, False],  True)
                , ([ True, False,  True],  True)
                , ([ True,  True, False],  True)
                , ([ True,  True,  True], False) ]

conjunto_or :: Conjunto
conjunto_or = [ ([ True, False, False], False)
              , ([ True, False,  True],  True)
              , ([ True,  True, False],  True)
              , ([ True,  True,  True],  True) ]

conjunto_xor :: Conjunto
conjunto_xor = [ ([ True, False, False], False)
               , ([ True, False,  True],  True)
               , ([ True,  True, False],  True)
               , ([ True,  True,  True], False) ]

--------------------------------------------------------------------------------

perceptrón :: [Double] -> Conjunto -> Lector ([Double], [Double])
perceptrón pesos conjunto = foldlM func (pesos, []) conjunto
    where
        func :: ([Double], [Double]) -> ([Bool], Bool) -> Lector ([Double], [Double])
        func (ws, errores) (xs, z) = do
            (u, t) <- asks (umbral &&& tasa)
            let s = sum $ zipWith (\x w -> fromBool x * w) xs ws
                n = fromBool $ s > u
                e = fromBool z - n
                d = t * e
                -- (nuevo ws, error de este caso)
            return (zipWith (+) ws $ map ((*d) . fromBool) xs, e : errores)

delta :: [Double] -> Conjunto -> Lector ([Double], [Double])
delta pesos conjunto = foldlM func (pesos, []) conjunto
    where
        func :: ([Double], [Double]) -> ([Bool], Bool) -> Lector ([Double], [Double])
        func (ws, errores) (xs, z) = do
            (u, t) <- asks (umbral &&& tasa)
            let s = sum $ zipWith (\x w -> fromBool x * w) xs ws
                n = fromBool $ s > u
                e = fromBool z - n
                d = t * e
                -- (nuevo ws, error de este caso)
            return (zipWith (+) ws $ map ((*d) . fromBool) xs, e : errores)
