{-
    Zero-Order Logic
    Calculo de Proposiciones

    Este codigo es el ejemplo 6 en typedef.hs (Materials)

    Los comentarios explican lo que hay que hacer.
-}

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


{-

    HW-3:

    (a) evaluar: F, T, (Not T), (And (Or T (Not T)) (Not F))

    (b)  Sea:
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

    (c) Usando la tecnica de "List Comprehension" (en una linea!)

        Hint: evalue esto en el Haskell REPL: [ (x, y) | x <- [F, T], y <- [F, T]]

        c.1 evalue 'exp2 x y' para todas la combinaciones de valores de 'x' y 'y'

        c.2 evalue 'eval (exp2 x y)' para todas la combinaciones de valores de 'x' y 'y'

    (d) Sea 'exp3' una funcion de tres variables de tipo Exp definida asi:
            exp3 x y z = And (exp2 x y) (Not z)

        c.1 evalue 'exp3 x y z' para todas la combinaciones de valores de 'x', 'y' y 'z'

        c.2 evalue 'eval (exp3 x y z)' para todas la combinaciones de valores de 'x' y 'y'

    (e) Implemente un probador de tautologias 'proof2' para proposiciones con dos variables:
       la funcion 'proof2 recibe un solo parametro de tipo Exp, aplicable a dos parametros

        proof2 :: (Exp -> Exp -> Exp) -> Bool
        proof2 exp = ...

    (f) ¡Utilice lo hecho en (e) para probar las leyes de De Morgan!
        https://en.wikipedia.org/wiki/De_Morgan's_laws

        Muestre como hizo el test en el codigo. Por ejemplo:

        dm1 = ... una de las leyes de De Morgan expresada con Exp ...
        ...
        rs1 = proof2 dm1
        ...

    (g) Repetir todo lo hecho en (e) y (f) pero para tres variables: 'proof3'

        proof3 :: (Exp -> Exp -> Exp -> Exp) -> Bool
        proof3 exp = ...

-}

exp2 x y = (And (Or x (Not y)) (Not x))
dem2 x y = False -- implementar la ley de De Morgan en 2 variables

proof2 :: (Exp -> Exp -> Exp) -> Bool
proof2 f2 = False -- implementar probador

exp3 x y z = And (exp2 x y) (Not z)
dem3 x y z = False -- implementar la ley de De Morgan en 3 variables

proof3 :: (Exp -> Exp -> Exp -> Exp) -> Bool
proof3 f3 = False -- implementar probador
