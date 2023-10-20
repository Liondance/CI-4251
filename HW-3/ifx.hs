--
-- MIT TEST
--
-- ifx: conditional expression (if expression)
-- ifx: expresión condicional
--

-- recuerdan fibo?
fib :: Int -> Int
fib = \n -> if n < 2 then n else fibo (n-1) + fibo (n-2)

-- Ejercicio

-- La sintaxis de la expresión condicional en Haskell es muy verbosa.
-- Vamos a reemplazat esa sintaxis con una más tersa usando abstraccion funcional.
-- En vez de:
--
--     if <exp> then <exp> else <exp>
--
-- queremos escribir:
--
--     ifx <exp> <exp> <exp>
--

-- H) Haskell

-- H.1) Implemente ifx en Haskell
ifx :: Bool -> e -> e -> e
ifx cond exp_t exp_f = ... deben completar esto, obviamente ...

-- H.2) Pruebe la implementación usando la función de fibonacci
fibo :: Int -> Int
fibo = \n -> ifx (n < 2) n (fibo (n - 1) + fibo (n - 2))

-- A) Lenguaje Alterno

-- A.1) Implemente ifx en por lo menos uno de estos lenguajes:
--      C/C++, Java (yuck!), TypeScript, JavaScript, Python (double yuck!)

-- A.2) Pruebe la implementación usando la función de fibonacci ...
--      ... `mutatis mutandis`, claro está

-- Preguntas .. y Respuestas (contesten con honestidad!)

-- P.1) ¿Se esperaban alguna diferencia entre H.2 y A.2 en el comportamiento del código?

    -- [ ] si
    -- [ ] no

-- P.2) ¿Hay diferencia entre H.2 y A.2 en el comportamiento del código?

    -- [ ] no: es obvio que se comportan igual
    -- [ ] si: ¡y no me la esperaba!
    -- [ ] si, pero me la esperaba

-- P.3) ¿Saben porque hay diferencia?

    -- [ ] no sé de que hablan: obviamente deben comportarse igual
    -- [ ] no sé, y no entiendo qué está pasando ... esto se ve extraño
    -- [ ] ahora si: no sabía esto, pero lo averigué (diga donde/como en P.4)
    -- [ ] si: yo ya sabía esto (diga donde/como se enteró en P.4)

-- P.4)

    {-
        Si contestaron 'si' en P.3, expliquen con sus palabras qué está pasando ...
        ... y agreguen la información que se pidió en P.3
    -}
