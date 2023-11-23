
{-

fmap0 :: a -> f a
fmap1 :: (a -> b) -> f a -> f b
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap3 :: (a -> b -> c -> d) -> ...

-}


{-

-- funciones basicas para aplicativos

pure :: a -> f a

(<*>) :: f (a -> b) -> f a -> f b

-- asociatividad

x <*> y <*> z <*> t
es
((x <*> y) <*> z) <*> t 

-- combinacion tipica de las funciones basicas

pure g <*> x1 <*> x2 <*> ... <*> xn

-}



{-

fmap0 = pure
fmap1 g x = pure g <*> x

-- evaluar el tipo de fmap0 ... fmapN

-}


fmap1 g x = pure g <*> x
fmap2 g x y = pure g <*> x <*> y

-- evaluar sumas con numeros y maybe numeros

{-
-- en el preludio de Haskell

instance Applicative [] where
    -- pure :: a -> [a]
    pure x = [x]

    -- (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [ g x | g <- gs, x <- xs ]

instance Applicative IO where
    -- pure :: a -> IO a
    pure = return    

    -- (<*>) :: IO (a -> b) -> IO a -> IO b
    mg <*> mx = do { g <- mg; x <- mx; return (g x)}
-}

getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

{-
    actionSequence es la funcion sequenceA del preludio
-}

actionSequence :: Applicative f => [f a] -> f [a]
actionSequence []       = pure []
actionSequence (x:xs)   = pure (:) <*> x <*> actionSequence xs

getCharacters n = sequenceA (replicate n getChar)
getNChars n = actionSequence (replicate n getChar)

{-
    ================
    Applicative Laws
    ================

    pure id <*> x   = x
    pure (g x)      = pure g <*> pure x
    x <*> pure y    = pure (\g -> g y) <*> x
    x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-}
