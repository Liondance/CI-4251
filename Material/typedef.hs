{-
    Select example by removing comments
-}

{-

-- example 1: 'type'
-- type alias (structural equivalence)

type Distancia = Double
type Masa      = Double
type Tiempo    = Double

altura :: Distancia
altura = 1.75

peso :: Masa
peso = 81

type Medida = (Distancia, Masa)
medida :: Distancia -> Masa -> Medida
medida d m = (d, m)

-}

{-

-- example 2: 'type'
-- type alias (structural equivalence)

type PuntoR = (Double, Double)

shiftx :: Double -> PuntoR -> PuntoR
shiftx delta (x, y) = (x + delta, y)

p1 :: PuntoR
p1 = (6.0, 7.0)

type PuntoP = (Double, Double)

-- declare p2 :: PuntoP
-- define p2
-- show
-- shift p2 to the right ... this ain't right!
-- redefine p2 using p1 as initializer

-}


{-
-- example 3: 'newtype'
-- type constructor with nominal equivalence
-- does not support recursive types

-- MKS system
newtype Distancia = M Double
    deriving Show
newtype Masa      = K Double
    deriving Show
newtype Tiempo    = S Double
    deriving Show

altura = M 1.70
peso = K 51

masa_cruda :: Masa -> Double
masa_cruda (K m) = m

newtype Medidas = Medidas (Distancia, Masa)
    deriving Show
enzo = Medidas (M 1.75, K 81)
medidas_crudas (Medidas (M metros, K kilos)) = (metros, kilos)
-}


{-

-- example 4: 'data'
-- type constructor with nominal equivalence
-- supports recursive types

data Rect = Rect (Double, Double)
    deriving Show
data Polar = Polar (Double, Double)
    deriving Show

pr1 = Rect (6, 7)
pp1 = Polar (6, 7)

shiftx :: Double -> Rect -> Rect
shiftx delta (Rect (x, y)) = Rect (x + delta, y)

-}

{-

-- example 5: 'data'
-- type constructor with nominal equivalence
-- supports polymorphism and recursive types

data Point = Rect (Double, Double) | Polar (Double, Double)
    deriving (Show)

pr1 = Rect (6, 7)
pp1 = Polar (6, 7)

shiftx :: Double -> Point -> Point
shiftx delta (Rect (x, y)) = Rect (x + delta, y)
shiftx delta (Polar (m, a)) = Polar (0, 0)

-}

-- ACTIVE {-

-- example6: 'data'
-- type constructor with nominal equivalence
-- supports polymorphism and recursive types

data Exp = F | T | Not Exp | And Exp Exp | Or Exp Exp | Eq Exp Exp
    deriving (Show)

eval :: Exp -> Bool
eval F = False
eval T = True
eval (Not arg) = not (eval arg)
eval (And lhs rhs) = (eval lhs) && (eval rhs)
eval (Or lhs rhs) = (eval lhs) || (eval rhs)
eval (Eq lhs rhs) = (eval lhs) == (eval rhs)

-- -}

{-

    Ejercicios:

    a) evaluar: F, T, (Not T), (And (Or T (Not T)) (Not F))

    b)  Sea:
            exp2 x y = (And (Or x (Not y)) (Not x))

        b.1 evalue:
            exp2 T T
            exp2 T F
            exp2 F T
            exp2 F F

        b.2 evalue:
            eval (exp2 T T)
            eval (exp2 T F)
            eval (exp2 F T)
            eval (exp2 F F)

    c) Usando la tecnica de "List Comprehension" (en una linea!)

        c.1 evalue 'exp2 x y' para todas la combinaciones de valores de 'x' y 'y'

        c.2 evalue 'eval (exp2 x y)' para todas la combinaciones de valores de 'x' y 'y'

    d) Sea 'exp3' una funcion de tres variables de tipo Exp definida asi:
            exp3 x y z = And (exp2 x y) (Not z)

        c.1 evalue 'exp3 x y z' para todas la combinaciones de valores de 'x', 'y' y 'z'

        c.2 evalue 'eval (exp3 x y z)' para todas la combinaciones de valores de 'x' y 'y'

-}
