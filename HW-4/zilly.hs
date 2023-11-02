--
-- Zilly
--

{-
    Preliminares
    ------------

    1. Antes que nada, disculpen la falta de acentos, etc.
    Trate de esmerarme hasta ahora, pero my equipo no coopera. 
    Escribir me quita mucho tiempo.

    2. No se asusten: esta es una tarea facil: por eso solo vale 10 puntos.
    Pero tienen que leer con cuidado las reglas y sugerencias dadas aqui.
    La explicacion es más larga de lo que yo quisiera, para que quede clara.
    He notado que aun estan desarrollando su "intuición computista".

    3. La hora de entrega es este domingo (5 de noviembre) a la medianoche.
    Queria darles esta tarea ayer en la noche y me he retrasado.
    Asi que voy a ser "benevolente" si necesitan uno o dos dias más.
    Sugiero que se liberen de esta tarea de una vez, para atacar otras materias.

    4. En esta tarea van a emular un lenguaje, llamado Zilly, en Haskell.
    Haskell se presta a esto y Zilly es ... "minimalistic". Suena complicado, ...
    pero no lo es. Sin embargo, lo repito, deben leer todo esto con cuidado.
    El espiritu esta tarea es:
    - hacerlos pensar (!) ... y divertirse creciendo su ambiente de programacion!
    - desarrollar su intuición acerca de la ingenieria de computacion
    - darse cuenta del poder de componer abstracciones funcionales
    - liberarse (un poco) del apego al "azucar sintactico"

    5. Felicitaciones: ya llevamos 50 puntos evaluados y van bien hasta ahora.

    Pero, por favor, no se vuelvan complacientes y no me decepcionen.

    =====
    Zilly
    =====

    Zilly es el lenguaje de una máquina virtual hipotética (todas lo son!) capaz 
    de realizar operaciones aritmeticas sobre numeros enteros de cualquier tamaño, 
    es decir: 10 dígitos, 100 dígitos, 1000 dígitos, ... en fin cualquier tamaño!
    Ademas, Zilly permite tener un numero arbitrario de variables enteras. 

    Como pueden imaginarse, el tipo 'entero' es un tipo fundamental en Zilly. 
    El símbolo asociado a este tipo es Z: https://en.wikipedia.org/wiki/Integer
    De hecho el nombre 'Zilly' se deriva del nombre de otro lenguaje: 'Lilly'.

    más aun, Z es el *único* tipo primitivo en Zilly, y lo vamos a emular asi:
-}

type Z = Integer

{-
    más facil no se puede ...

    En Zilly (bueno, en esta variante, Zilly[0], ... hay otras) no hay tuplas,
    no hay listas, no hay ... mejor dejenme decirles que hay: funciones!
    Sorpresa? Espero que no: les recuerdo que curso están viendo.

    La sintaxis de los tipos soportados en Zilly puede describirse asi:

        T ==> Z
        T ==> T -> T

    Como representamos esos tipos en Haskell? Directamente, descaradamente!
    En Haskell pueden estribir esto:

        type ZZ = Z -> Z

    o esto:

        type SuperDuperApplicator = (Z -> (Z -> Z)) -> Z -> Z -> Z

    No van a tener que definir nuevos tipos en esta tarea. Solo para que sepan.
    Vamos a encaramarnos (descaradamente) en la inferencia de tipos de Haskell.
    Pero no esta prohibido agregar declaraciones de tipos, si las quieren.

    Ya conocemos los tipos, ahora vamos a ver las expresiones en Zilly:

        E ==> <n>               -- constantes enteras
        E ==> <symbol>          -- simbolos
        E ==> \<symbol> -> E    -- abstracción funcional ... Arriba Alonzo!
        E ==> E E               -- aplicación funcional
        E ==> ifx E E E         -- expresión condicional

    Concretamente (del punto de vista lexicografico), una constante entera <n>
    es el dígito 0 o un dígito distinto de 0 seguido de cero o más dígitos.
    Eso es todo. Noten que no mencionamos ningun "guion" ('-') o culebrita ('~').
    Para obtener valores enteros negativos usamos una función que veremos luego.
    La sintaxis de expresiones no incluye ningun "azucar" de "operadores", i.e.
    no hay expresiones de la forma -E, E + E, E - E, E * E, ... no más azucar!

    Para identificar parametros y variables libres necesitamos simbolos.
    Para <symbol> vamos a seguir las mismas convenciones de Haskell.

    Las expresiones lambda (abstracciones funcionales) son iguales a las de Haskell.
    Noten que cada lambda utiliza exactamente un parametro.

    La sintaxis de aplicaciones funcionales tambien es igual a la de Haskell.

    La sintaxis de expresiones condicionales solo tiene estas diferencias:
    - las palabras reservadas 'then' y 'else' se omiten
    - en vez de usar el símbolo reservado 'if' usamos 'ifx'

    Noten que la condicion de 'ifx' es de tipo 'Z': no hay 'Bool' en Zilly.
    Solo tenemos numeros enteros y funciones. Donde sea necesario interpretar 
    un numero entero como booleano, Zilly lo hace de acuerdo a estas reglas:

        0 se interpreta como falso
        todo numero distinto de 0 se interpreta como verdadero

    Ademas, como buena practica, cada vez que computamos un entero que
    queremos de interprete como booleano, seguimos esta convencion:

        usamos 0 para indicar falso
        usamos 1 para indicar verdadero

    es decir, normalizamos la representacion del valor de verdad con el 1.

    Por ultimo, la sintaxis para definir simbolos es igual a la de Haskell:

        D ==> <symbol> = E

    Solo deben usar las formas sintacticas definidas aqui, y las funciones
    predefinidas en el preludio que veremos en un momento.

        tran x y = 2 * x + 3 * y         -- TRAMPA: no hay pattern matching
        tran = \x -> \y -> 2 * x + 3 * y -- TRAMPA: no tenemos operadores
        tran = \x -> \y -> plus (mult 2 x) (mult 3 y) -- OK

    Tampoco deberian agregar declaraciones de tipos, como:

        tran :: Z -> Z -> Z

    pero si quieren hacerlo no hay problema: no pervierte el espiritu de la tarea.

    En cambio, usar sintaxis de Haskell no bendecida, usar 'if' en lugar de 'ifx', 
    o usar funciones predefinidas en el preludio de Haskell si pervierte lo que
    queremos hacer: programar en Zilly, no en Haskell. Nos estamos aprovechando
    de Haskell (descaradamente) para emular una máquina virtual de Zilly.
    
    No hagan nada que eche a perder ese objetivo. No hagan trampa.

-}

--
-- Magic Section: Zilly implementation
--
-- Usamos Haskell para crear las abstracciones fundamentales de Zilly
-- Noten las "trampas", pero aqui no son "trampa". Estamos operando
-- a bajo nivel, haciendo la magia que implementa la VM de Zilly.
--

--- Zilly language adapter - bool <-> int conversions

-- convierte Z (valor entero) a Bool (valor booleano)
b :: Z -> Bool
b n = n /= 0 

-- convierte un Bool de Haskell (valor booleano) a Z
z :: Bool -> Z
z b = if b then 1 else 0;

{-
    Opcional: si quieren, dado que en Haskell "true" y "false" no son simbolos
    predefinidos ("True" y "False" lo son) pueden agregar esto al preludio:

    false :: Integer
    false = 0

    true :: Integer
    true = 1

    hasta pueden usar nombres como "verdadero" y "falso".
    Son constantes enteras, pero "meten la coba" de parecer booleanas.
-}

-- conditional expression - expresión condicional
ifx :: Z -> e -> e -> e
ifx pred x y = if b pred then x else y

-- Noten el uso del adaptador 'b' en la definicion de ifx!
-- Si se abstraen del azucar sintactico, esto es literalmente 
-- lo que hacemos para crear 'ifx' a partir del 'if' de Haskell.
-- Haskell necesita un 'Bool' en la condicion.

--
-- primitive functions - funciones primitivas (magicas!)
--

-- less than - "menor que"
lt :: Z -> Z -> Z
lt x y = z (x < y) -- noten el uso del adaptador z!
-- lt x y = if x < y then 1 else 0 -- implementación alterna

-- minus
minus :: Z -> Z -> Z
minus x y = x - y -- descaradamente ...

--
-- End Magic Section. Se acabo la magia! Hacer magia más adelante es hacer trampa.
--

{-
    Comienza la parte de Ustedes, guerreros de programacion funcional

    Van a "crecer el lenguaje", como diria Guy L. Steele Jr. [1]
    A partir de origenes humildes (solo tenemos dos funciones predefinidas),
    van a extender la funcionalidad de este "preludio", sin hacer trampas!

    Es decir usando *solamente* las funciones primitivas dadas y las funciones
    que Ustedes deriven, directamente o transitivamente, a partir de ellas.

    Van a notar varias trampas aqui. Las puso el coordinador del laboratorio
    para que puedan correr este archivo, en el ghci.
    
    Su entrega tambien *debe* poder cargarse y ejecutarse en el ghci.

    En lo que sigue:
    - Algunas funciones ya están implementadas correctamente en Zilly.
    - Otras funcionan correctamente, pero están implementadas haciendo trampa.
    - y otras no están implementadas: regresan algo tonto para que compilen.

    (A) Su trabajo consiste en "crecer a Zilly" completando este programa
    (B) Es parte de la evaluacion entender que funciones ya están listas
    (C) Es parte de la evaluacion detectar las trampas y eliminarlas
    (D) Es parte de la evaluacion implementar las funciones que no lo están
    (E) Y es parte de la evalucion extender la suite de prueba, sin exagerar

    Ustedes son inteligentes: usen esta tarea para desarrollar su intuición
    de ingenieria de software. Imaginense que usuarios finales, por ejemplo
    programadores de aplicaciones, van a usar lo que hicieron. Ustedes, los
    programadores de sistemas, deben ayudar a los "application programmers"
    a tener una vida más facil: por eso le están agregando funciones a la
    libreria de "runtime". Imaginense que otros excelentes graduados de la 
    carrera de ingenieria de computacion, "systems programmers", quieren seguir
    extendiendo esta libreria. Haganle la vida más facil a ellos tambien,
    y al coordinador del laboratorio, escribiendo codigo claro.

    Tip: si estan desarrollando una función g que depende de una función f,
    tiene sentido - para el desarrollo - que hagan trampa en la implementacion
    de f para probar a g. De esta manera se aseguran que cuando implementen a
    f, g va a funcionar. Si no logran terminar la tarea, es mucho mejor dejar
    algunas trampas "vivas" que dejar de implementar varias funciones debido 
    a las dependencias entre las funciones! Pero indiquen claramente cada
    función que hace TRAMPA, con un comentario.

    ---
    [1] Evidencia: para que vean quien es Guy Steele. No recomiendo ver todo el
    video a menos que el tema les interese. No va a ayudarlos en esta tarea.

    https://en.wikipedia.org/wiki/Guy_L._Steele_Jr.
    https://www.youtube.com/watch?v=_ahvzDzKdB0
-}


--
-- operadores booleanos
--

and :: Z -> Z -> Z
and = \x -> \y -> 0

or :: Z -> Z -> Z
or = \x -> \y -> 0

not :: Z -> Z
not = \x -> 0

--
-- comparadores
--

{-
    implementen todas las funciones de comparacion
    esta es la correspondencia con los operadores de Haskell

    lt: <
    le: <=
    eq: ==
    ne: /=
    ge: >=
    ne: /=

    Como vieron, lt ya esta implementado.

    El codigo que sigue implementa eq (tramposamente) para correr pruebas

-}

le :: Z -> Z -> Z
le = \x -> \y -> 0

eq :: Z -> Z -> Z
eq = \x -> \y -> z (x == y)

ne :: Z -> Z -> Z
ne = \x -> \y -> 0

ge :: Z -> Z -> Z
ge = \x -> \y -> 0

gt :: Z -> Z -> Z
gt = \x -> \y -> 0

--
-- aritmetica!
--

-- chs -- change sign - cambio de signo, es decir la negación
chs :: Z -> Z -- semantica: chs x = -x 
chs x = -x

-- plus
plus :: Z -> Z -> Z -- semantica: plus x y = x + y
plus x y = x + y

-- mult
mult :: Z -> Z -> Z -- semantica: mult x y = x * y
mult x y = x * y

--
-- funciones
--

--  factorial
fact :: Z -> Z
fact = \n -> ifx (lt n 2) 1 (mult n (fact (n - 1))) -- ... algo se ve diferente aqui ...

--  fibonacci!
fibo :: Z -> Z
fibo = \n -> 42 -- ??? 

-- greatest common divisor - máximo común divisor
gcdx :: Z -> Z -> Z
gcdx = \x -> \y -> gcd x y -- detecten y eliminen la TRAMPA!

--
-- End of Zilly library runtime
--

--
--  Test scaffolding - Andamio para pruebas
-- 

type Test = (String, Z)

check :: Test -> String
check (des, exp) = des ++ ": " ++ if b exp then "PASSED" else "FAILED"

-- Suite: a test suite - una serie de pruebas
type Suite = [Test]

comparadores :: Suite
comparadores =
    [
        -- lt
        ("-7 < -3", lt (chs 7) (chs 3)),
        ("-7 <  3", lt (chs 7)      3 ),
        ("-3 <  7", lt (chs 3)      7 ),
        (" 3 <  7", lt      3       7 )

        -- le

        -- eq

        -- ne

        -- ge

        -- gt
    ]

aritmeticos :: Suite
aritmeticos =
    [
        -- plus

        -- minus
        ("eq (minus 0 0) 0", eq (minus 0 0) 0)

        -- mult
    ]

factorial :: Suite
factorial =
    [
        ("eq (fact 1)   1", (eq (fact 1)   1)),
        ("eq (fact 5) 120", (eq (fact 5) 120)),
        ("eq (fact 6) 720", (eq (fact 6) 720))
    ]

fibonacci :: Suite
fibonacci =
    [
        ("eq (fibo  0)    0", eq (fibo  0)    0),
        ("eq (fibo  1)    1", eq (fibo  1)    1),
        ("eq (fibo 20) 6765", eq (fibo 20) 6765)
    ]

gcds :: Suite
gcds =
    [
        ("eq (gcdx 6097 19638906) 469", eq (gcdx 6097 19638906) 469)
    ]

-- Full suite of suites - serie completa (extensible) de series de prueba

full :: [Suite]
full =
    [
        comparadores,
        aritmeticos,
        factorial,
        fibonacci,
        gcds
    ]

-- run one test suite - ejemplo: run factorial
run :: Suite -> IO ()
run [] = return ()
run (t:ts) = do
    let result = check t
    putStrLn result
    run ts

-- run many test suites - ejemplo: test full
test :: [Suite] ->  IO ()
test [] = return ()
test (s:ss) = do
    run s
    putStrLn "--------"
    test ss
