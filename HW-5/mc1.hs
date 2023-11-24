--
-- Monte Carlo Test 1D
--

-- module MonteCarlo where

import System.IO
import System.Random

-- Histogram descriptor - Descriptor de histograma

-- HistDesc = HD (lb, ub, nb)
-- lb :: Double: lower bound (limite inferior)
-- ub :: Double: upper bound (limite superior)
-- nb :: Int: number of buckets (numero de contenedores -> barras)
newtype HistDesc = HD (Double, Double, Int)
    deriving Show

-- lower bound
lb :: HistDesc -> Double
lb (HD (lbx, _, _)) = lbx

-- upper bound
ub :: HistDesc -> Double
ub (HD (_, ubx, _)) = ubx

-- number of buckets
nb :: HistDesc -> Int
nb (HD (_, _, nbx)) = nbx

-- un histograma es un par: descriptor y arreglo de frecuencias
newtype Histogram = H (HistDesc, [Int])
    deriving Show

-- convenience function to create a new histogram
-- funcion para crear un histograma con facilidad
-- notar que el vector de frecuencias tiene 2 celdas extra
-- la primera celda (indice 0) es para contar las ocurrencias menores que lb (x < lb)
-- la ultima (indice nb + 1) cuenta las ocurrencias mayores o iguales a ub (ub <= x)
-- las demas cuentan las ocurrencias en el rango [lb .. ub), dividido en segmentos iguales
histogram :: (Double, Double, Int) -> Histogram
histogram (lb, ub, nb) = H (HD (lb, ub, nb), fvec (nb + 2))

desc :: Histogram -> HistDesc
desc (H (desc, _)) = desc

freq :: Histogram -> [Int]
freq (H (_, fvec)) = fvec -- vector de frecuencias

-- tambien se puede implementar usando la funcion 'replicate'
fvec :: Int -> [Int]
fvec 0 = []
fvec n = 0:fvec (n-1)

-- add observation (x) to the histogram
-- agregar una ocurrencia (es decir la observacion o muestra 'x') al histograma
-- OJO: el nuevo arreglo de frecuencias es ... ¿igual al viejo? ...
-- ... claramente esto no funciona todavia ;-)
-- ustedes deben determinar como calcularlo para reflejar la nueva ocurrencia
add :: Histogram -> Double -> Histogram
add (H (desc, oldf)) x = H (desc, newf)
    where newf = oldf -- ... ¿igual al viejo? ... NO!

-- for debugging only - para facilitar debugging, de ser necesario
debug = False

-- tipo funcion que no toma argumentos y retorna un numero Double aleatorio
type Random01 = () -> IO Double

-- Monte Carlo
-- 'hist': histograma de frecuencias de entrada, inicialmente en "ceros"
-- 'f': funcion aleatoria de tipo Random01
-- 'n': numero de muestras (es decir: corridas, experimentos, o simulaciones)
-- retorna el histograma de frecuencias final, correspondiente a todas las muestras
mc :: Histogram -> Random01 -> Int -> IO Histogram
mc hist f 0 = return hist
mc hist f n = do 
    x <- f ()
    if debug then putStrLn (show x) else putStr ""
    let h = add hist x
    if debug then putStrLn (show h) else putStr ""
    mc h f (n - 1)

-- Test cases
-- Casos de prueba

-- standard uniform
-- uniforme estandar
u :: Random01
u () = randomIO :: IO Double

bmh :: Random01
bmh () = do
            u1 <- u()
            u2 <- u()
            let r = sqrt(-2 * (log u1))
            let a = (2 * pi * u2)
            return (r * cos a)

-- standard normal
-- normal estandar (promedio 0, varianza 1) (sugerencia: consideren Box-Muller)
-- OJO: no esta implementada correctamente
n () = do
    x <- u()
    y <- u()
    z <- u()
    return ((x + y + z) / 3)

-- g ... as you like! (suggestion: consider logarithmic transformation)
-- OJO: deben agregar una funcion aleatoria a su gusto, ver sugerencia arriba
g () = do
    x <- u()
    y <- u()
    return (x - y)

-- tipo de un caso de prueba
type Test = (String, Random01, Histogram)

-- 3 casos de prueba
-- OJO deben ajustar los histogramas para que se vean bien y sean utiles
tests :: [Test]
tests = 
    [
        ( "u", u, histogram (-0.0, 1.0, 20) ),
        ( "n", n, histogram (-3.0, 3.0, 20) ),
        ( "g", g, histogram (-1.0, 1.0, 20) )
    ]

-- OJO siempre selecciona el ultimo caso (incorrecto) en vez del indicado por el usuario
select :: String -> [Test] -> Test
select _ tests = tests !! (length tests - 1)

run h f n = do
        result <- mc h f n
        putStrLn (show result)

choices :: [Test] -> [String]
choices [] = []
choices ((s, _, _):xs) = s:choices xs

{-
    run1 - (8 puntos) simulaciones Monte Carlo en 1D: () -> IO Double
-}

run1 = do
    putStr ("Choices " ++ show (choices tests) ++ ": ")
    hFlush stdout
    fstr <- getLine
    let (s, f, h) = select fstr tests
    if debug then putStrLn (show (s, h)) else putStr ""
    putStr "N evaluaciones: "
    hFlush stdout
    nstr <- getLine
    let n = (read nstr) :: Int
    run h f n
