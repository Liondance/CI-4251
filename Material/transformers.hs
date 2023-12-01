import Debug.Trace
import Data.Functor.Identity (Identity (runIdentity))



{- newtype State s a = State { runState :: s -> (s,a) }

instance Functor (State s) where
  fmap f ma = do
    a <-  ma
    pure $ f a

instance Applicative (State s) where 
  pure = return 
  fa <*> ma = do
    f <- fa
    a <- ma
    pure $ f a

instance Monad (State s) where
  return x = State $ \s -> (s,x)
  -- ma :: s -> (s,a)
  -- f :: a -> State s b
  -- f :: a -> (s -> (s,b))
  -- s -> (s -> (s,b))
  (State ma) >>= f = State $ \s -> 
    let (s',a)    = ma s
        (State g) = f a
    in g s'

set :: s -> State s ()
set s = State $ \_ -> (s,())

get :: State s s
get = State $ \s -> (s,s)



increment  :: State Int ()
increment = get >>= set . (+1)

programita :: State Int ()
programita = do
  increment
  n <- get
  trace (show n) pure ()
  increment
  n' <- get
  trace (show n') pure ()


main = runState programita 30
 -}

newtype Compose f g a = Compose (f (g a))


instance (Functor f, Functor g) => Functor (Compose f g) where 
  fmap f (Compose fgx) = Compose $ fmap (fmap f) fgx 


newtype StateT m s a = StateT { runStateT :: s -> m (s,a) }

instance (Monad m) => Functor (StateT m s) where
  fmap f ma = do
    a <-  ma
    pure $ f a

instance (Monad m) => Applicative (StateT m s) where 
  pure = return 
  fa <*> ma = do
    f <- fa
    a <- ma
    pure $ f a


instance Monad m => Monad (StateT m s) where
  return x = StateT $ \s -> return (s,x)
  -- f :: s -> m (s,a)
  -- g :: a -> StateT (s -> m (s,b))
  StateT f >>= g = StateT $ \s -> do
    (s',a) <- f s
    let (StateT h) = g a
    h s' 


-- IO /= StateT IO Int ()


lift :: Monad m => m a ->  StateT m s a
lift ma = StateT $ \s -> (\a -> (s,a)) <$> ma

set' :: Monad m => s -> StateT m s ()
set' s = StateT $ \_ -> pure (s,())

get' :: Monad m => StateT m s s
get' = StateT $ \s -> pure (s,s)

increment'  :: Monad m => StateT m Int ()
increment' = get' >>= set' . (+1)

programita' :: StateT IO Int ()
programita' = do
  increment'
  n <- get'
  lift $ print n
  increment'
  n' <- get'
  lift $ print n'


m :: StateT (StateT IO Float) Int ()
m = undefined

type State s a = StateT Identity s a

runState :: State s a -> s -> (s,a)
runState mia s = runIdentity $ runStateT mia s
