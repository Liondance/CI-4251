--
-- State Monad
--

-- Nota:
-- Todos los ejemplos "re-etiquetan" un arbol de caracteres
-- creando un arbol de enteros. Los ejemplos b y c crean la
-- ilusion de tener un contador, aunque no hay variables mutables!

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)

-- a Tree of Char - un arbol de caracteres
tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Node (Leaf 'c') (Leaf 'd'))

-- las "nuevas" etiquetas van a ser enteros
type Label = Integer

-- Example a: Not using an Applicative State Transformer Functor
-- Ejemplo a: Sin usar un Funtor Aplicativo Transformador de Estado 

rlabel :: Tree a -> Label -> (Tree Label, Label)
rlabel (Leaf _)   n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
    where
        (l', n')  = rlabel l n
        (r', n'') = rlabel r n'

res'a = rlabel tree 0

--
-- Defining the Applicative State Transformer Functor ST
-- Definiendo el Funtor Aplicativo Transformador de Estado ST
--

-- el tipo del estado del sistema es el mismo de la etiqueta
-- el "contador" invisible provee la proxima etiqueta a ser usada
type State = Label

-- given a state, a transformer returns a pair with the new state
newtype ST a = S (State -> (a, State))

-- function that applies the transformer to a state
apply :: ST a -> State -> (a, State)
apply (S st) x = st x

-- ST is a Functor
instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x, s') = apply st s in (g x, s'))

-- ST is an Applicative Functor
instance Applicative ST where
    -- pure = a -> ST a
    pure x = S (\s -> (x, s))
    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s ->
        let (f, s' ) = apply stf s
            (x, s'') = apply stx s' in
                (f x, s''))

-- Example b: Using the Applicative State Transformer Functor ST
-- Ejemplo b: Usando el Funtor Aplicativo Transformador de Estado ST

fresh :: ST State
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree State)
alabel (Leaf _)   =  pure Leaf <*> fresh
alabel (Node l r) =  pure Node <*> alabel l <*> alabel r

res'b = (apply (alabel tree) 0)

--
-- Defining the State Transformer Monad ST
-- Definiendo la Monada Transformadora de Estado ST
--

-- ST is a Monad
instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x, s') = apply st s in apply (f x) s')

-- Example c: like b, but using the ST Monad and the do-syntax sugar
-- Ejemplo c: igual que b, pero usando la Monada ST y la sintaxis azucarada 'do'

mlabel :: Tree a -> ST (Tree State)
mlabel (Leaf _)   = do
                        n <- fresh
                        return (Leaf n)

mlabel (Node l r) = do
                        l' <- mlabel l
                        r' <- mlabel r
                        return (Node l' r')

res'c = (apply (mlabel tree) 0)
