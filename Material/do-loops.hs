-- Ejemplos usando do con lazos
-- inspirados por articulos y StackOverflow

import System.IO
import Control.Monad

-- Define a recursive function that prints "a string" n times

imprimirNVeces :: (Eq t, Num t) => t -> IO ()
imprimirNVeces 0 = return ()
imprimirNVeces n =
  do
    putStrLn "Hola Guerreros!"
    imprimirNVeces (n-1)

test1 = imprimirNVeces 8

imprimirStringNVeces s 0 = return ()
imprimirStringNVeces s n =
  do
    putStrLn s
    imprimirStringNVeces s (n-1)

test1s = imprimirNVeces 8

-- Generalizacion que repite una accion n veces:

repetirNVeces 0 _ = return ()
repetirNVeces n action =
  do
    action
    repetirNVeces (n-1) action

test2 = repetirNVeces 8 (putStrLn "Camara ... Accion!")

-- La misma generalizacion anterior esta definida en Control.Monad y se llama replicateM_:

test3 = replicateM_ 8 putStrLn "This is how we do it!"
