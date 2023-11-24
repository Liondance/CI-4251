--
-- Monte Carlo Test 1D
--

-- module MonteCarlo where

import System.IO
import System.Random

type Random01 = () -> IO Double
type Random02 = () -> IO (Double, Double)

-- standard uniform!
u :: Random01
u () = randomIO :: IO Double

-- Box-Muller! (en forma monadica en bandeja de plata)
bm :: Random02
bm () = do
            u1 <- u ()
            u2 <- u ()
            let r = sqrt(-2 * (log u1))
            let theta = (2 * pi * u2)
            return (r * cos theta, r * sin theta)

{-
	run2 - (7 puntos) simulaciones Monte Carlo en 2D: () -> IO (Double, Double)

    El objetivo es generalizar lo hecho en mc1 a dos dimensiones.
    Usen la funcion Box-Muller como caso de prueba, pero no la deben "cablear" ...
    ... es decir, el codigo debe funcionar si pasan otra funcion como parametro.
    En cada muestra, la funcion mc (Monte Carlo) obtiene un par de numeros aleatorios
    llamando a la funcion pasada como parametro, igual a como lo hicimos en mc1.
    Noten que las funciones de prueba no estan cableadas dentro de 'mc', en mc1.

    En vez de un arreglo de frecuencias, el histograma va a ser ... ¡pienselo!
    Deben acumular las frecuencias de manera analoga a lo hecho en mc1, pero ...
    ... la estructura va a ser distinta, con mas datos que imprimir.

    Deben computar y poder imprimir el histograma de frecuencias, pero ...
    ... su "jefe" ademas quiere una presentacion mas "visual", parecida a
    lo que pueden ver aqui:

    | | | | |.| | | |
    | |:|:|:|:|:|.| |
    | |:|+|+|+|+|:| |
    |.|:|*|*|*|+|:|.|
    | |:|+|*|*|+|:| |
    | |:|+|+|+|+|:| |
    | |:|:|:|:|:|:| |
    | | |.| |.| | | |

    La idea es que las celdas con frecuencia cero, o muy baja, se muestren
    con un espacio en blanco y las con frecuencia mas alta con un asterisco.
    Otros caracteres se pueden usar para frecuencias intermedias. Aqui les
    va mi sugerencia, en orden creciente de densidad de datos:

        [ " ", ".", ":", "+", "*" ]

    Entre otras cosas, van a tener que "entonar" de manera inteligente la
    decision de donde tomar los "cortes" para pasar de un nivel (caracter)
    a otro. Notar que decidir esto con valores enteros absolutos no sirve:
    el resultado dependeria no solo de la distribucion sino del numero de
    muestras, en vez de ser relativo. Describan en la funcion correspondiente,
    con un comentario, que criterios usaron para determinas los cortes.

    Se evalua ... todo!
    - la facilidad de uso
    - sus decisiones de diseno
    - la claridad y elegancia del codigo
    - su consistencia con lo hecho en mc1
    - el uso adecuado de comentarios
    - la estetica de la salida
    - y mas ...
-}

bm'test 0 = return ()
bm'test n = do
    (x, y) <- bm ()
    putStrLn (show (x, y))
    bm'test (n - 1)

test = do
    putStrLn "Let's do it!"
    bm'test 18
    putStrLn "Done!"

run2 = "inspirado en mc1"
