{-

    TMX: targeted marketing example (phase I)

-}

{-
    Aliases de tipos (type aliases) para mejorar la expresividad del código.

    Lo que llamamos "productos" ("products") no son necesariamente bienes tangibles.
    De hecho, en este caso los productos son "marcas" ("brands"), no modelos de carros
    en particular. Los productos también pueden ser servicios, partidos políticos, o 
    cualquier otra cosa que alguien quiere vender. Los "objetivos" ("targets") del 
    mercadeo casi nunca son personas específicas. En este ejemplo, como lo son a menudo,
    son centroides ("centroids") que representan un sector demográfico.

    Van a notar que los "productos" y los "objetivos" tienen el mismo tipo. Esto tiene 
    algunas ventajas: hace el código es más reversible y general. Noten que el problema
    de conseguir el mejor apareamiento (match) para un "objetivo" es dual con hacer lo
    mismo para un "producto". Además, en algunos modelos (no en este) tiene sentido
    medir la distancia entre dos objetivos o entre dos productos.

    Pero tener el mismo tipo viene con desventajas. Por ejemplo, pueden pasar un
    argumento de tipo "objetivo" a un parámetro de tipo "producto".

    Opcional: pueden definir el tipo 'Product' y el tipo 'Target' ustedes mismos.
    Si lo hacen usando la syntaxis de 'type' (aliases) esto va a ser casi trivial,
    pero no van a agregar ninguna protección, aunque van a hacer el código mas leible.

    Si usan la syntaxis de 'newtype' o 'data', van a tener la protección que se desea
    de un sistema de tipos, pero van a tener que sobrecargar la función 'Show' y
    posiblemente otras funciones predefinidas que sólo trabajan con tipos predefinidos.
-}

type Label = String
type Coordinates = [Double]
type Point = (Label, Coordinates)

type Match = (Point, Point)
type Result = [(Label, Label)]

{-
    Datos para prueba. Estos son sólo para ayudarlos a arrancar y probar el programa.
    Estoy usando los mismos datos del modelo ZenSheet TMX que vieron en clase, claro
    está que el ejemplo específico es sólo eso: un ejemplo.

    Su código debe funcionar para TODO espacio N-Dimensional. En este ejemplo N=3.

    Sean inteligentes (you are CI students at the USB: I assume you are!) no pierdan
    tiempo con cosas que no son relevante al aspecto computacional del proyecto. En
    particular, no inventen nombres bonitos. Nombres sugestivos sirven para hacer una
    presentación de producto ... nuevamente, un aspecto de mercadeo :D El trabajo
    de ustedes es escribir código que se puede pegar a la base de datos de una empresa.
    No importa si esa produce licores, juguetes, prendas de vestir, etc.

    Aqui les doy una plantilla:

    products :: [Point] -- o quizás [Product]
    products =  [
                    ("P0", [...]), 
                    ("P1", [...]), 
                    ("P2", [...]) 
                ]

    targets :: [Point] -- o quizás [Target]
    targets =   [
                    ("T0", [...]),
                    ("T1", [...]),
                    ("T2", [...]),
                    ("T3", [...]),
                    ("T4", [...])
                ]

    No hemos visto módulos todavía, que serían utiles para tener ejemplos en 
    archivos distintos. Pongan todos sus ejemplos (2 son suficientes, 3 a lo sumo)
    cerca de los demás, en este archivo, y usen comentarios para excluir a todos
    menos uno. Yo voy a probar sus ejemplos y también correr los mios. Por favor,
    no cambien los nombres de estas variables, para simplificarme el trabajo.
-}

products :: [Point]
products =  [
                ("Cadillac", [9.0, 8.0, 8.0]), 
                ("Buick", [7.0, 7.0, 7.0]), 
                ("Pontiac", [6.0, 6.0, 7.0]), 
                ("Chevrolet", [4.0, 5.0, 6.0])
            ]

targets :: [Point]
targets =   [
                ("Alice", [8.0, 8.0, 8.0]),
                ("Betty", [6.0, 7.0, 8.0]),
                ("Charles", [6.0, 7.0, 8.0]),
                ("Daniela", [5.0, 5.0, 8.0]),
                ("Eric", [5.0, 5.0, 6.0])
            ]

-- Etiqueta de un punto
label :: Point -> Label
label p = fst p

-- Coordenadas de un punto
coordinates :: Point -> Coordinates
coordinates p = snd p

-- Distancia entre dos puntos
-- COMPLETAR: ustedes van a necesitar una función como esta!
distance :: Point -> Point -> Double
distance p q = 42

{-
    *Todo* lo que sigue es completamente opcional: sólo para "inspiración" si la quieren 
    Borren o cambien lo que quieran. Los únicos requerimientos, no negociables, son:

    - el programa debe tener una variable llamada "result" o "resultado" al final
    - el resultado debe ser de tipo (... drum roll ...) 'Result', obviamente
    - la "columna izquierda" del resultado debe corresponder a las etiquetas de los 'targets'
    - otra forma de decir lo anterior es que un 'Match' es de la forma ('Target', 'Product')

    - Esta es la fase I del proyecto. La entrega es el viernes 20 a la medianoche en Venezuela.

    El código que sigue compila ... y corre (!) ... pero no hace lo que queremos, obviamente.
-}

-- Completar (opcional): función que retorna el punto más cercano a otro en un conjunto de puntos
closest :: [Point] -> Point -> Point
-- Note: cleverly designed for currying ... you will see ... ;-)
closest points point = ficticious
    where ficticious = points !! (42 `mod` length products) -- <== :D

-- Best match
match :: [Point] -> Point -> Match
-- Note again: designed for currying ... you will see ... ;-)
match points point = (point, closest points point)

-- Test ... try this
test :: [Match]
-- Note how the first argument to 'fmap' is a partially evaluated function: Curry for the win!
test = fmap (match products) targets

-- Labels (a little gift for you, in case you need it)
labels :: Match -> (Label, Label)
labels match = (label (fst match), label (snd match))

-- Result ... try this
result :: Result
result = fmap (labels) test
