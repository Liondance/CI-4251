{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Tarea.Parser where

import Data.Proxy
import Tarea.Typ
import Text.Parsec hiding ((<|>),many)
import Data.Functor
import Control.Applicative
import Control.Monad 
import Data.Maybe
import Data.String (IsString(..))
import System.IO
import Data.Function
import Data.Functor.Identity
import Data.Coerce
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.Except
import Data.Bifunctor
import Data.Kind (Constraint, Type)
import Control.Monad.State.Lazy
import Control.Exception

type Parser m a =  ParsecT [Char] () m a

instance (a ~ String, Monad m) => IsString (ParsecT [Char] u m a) where
  fromString s = string' s <* spaces




-------------------------
-- Utilities
-------------------------

askVar :: MonadState EnvD m => [Char] -> m (Maybe Dynamic)
askVar = gets . M.lookup 


askVar' :: (MonadState EnvD m, MonadError IOException m) => [Char] -> m (Dynamic)
askVar' v = askVar v >>= flip maybe pure
  (throwError . userError  . show $ Err errMSG Nothing ) 
  where 
    errMSG = "Variable: " <> show v <> " not found in the environment"


-- Modify the parser so it also accepts:
-- .2354
-- 3e5
-- 3e-2
-- 3.25
-- 0.32
pFloat :: Monad m => Parser m Float
pFloat = undefined



lexeme :: Monad m => Parser m a -> Parser m a
lexeme p = p <* spaces

pVarName :: Monad m => Parser m [Char]
pVarName = lexeme $ (:) <$> asum [char '_', letter] <*> many (alphaNum <|> char '_') 


pDF :: Monad m => Parser m Dynamic
pDF = Dynamic tFloat <$> lexeme pFloat

pVar :: (MonadState EnvD m, MonadError IOException m) => Parser m Dynamic
pVar = pVarName >>= askVar'

pBool :: Monad m => Parser m Bool
pBool = "true" $> True <|> "false" $> False

pDB :: Monad m => Parser m Dynamic
pDB = Dynamic tBool <$> pBool 


parens :: Monad m => Parser m a -> Parser m a
parens = between "(" ")"


infixl1 :: Monad m => (a -> b) -> Parser m a -> Parser m (b -> a -> b) -> Parser m b
infixl1 wrap p op = (wrap <$> p) <**> rest
  where
    rest = flip (.) <$> (flip <$> op <*> p) <*> rest <|> pure id

pBinaryInfixL :: Monad m => [Parser m (a -> a -> a)] -> Parser m a -> Parser m a
pBinaryInfixL infixList next = chainl1 next $ asum infixList

pBinaryInfixL' :: Monad m => (a -> b) -> [Parser m (b -> a -> b)] -> Parser m a -> Parser m b
pBinaryInfixL' f infixList next = infixl1 f next $ asum infixList

-- rewrite without backtracking the whole first segment
pBIL :: Monad m => (b -> Dynamic) -> (Dynamic -> m b) -> [Parser m (m b -> Dynamic -> m b)] -> Parser m Dynamic -> Parser m Dynamic
pBIL back f infixList next = undefined


pIf :: (MonadState EnvD m, MonadError IOException m) => Parser m Bool -> Parser m Dynamic -> Parser m Dynamic -> Parser m Dynamic
pIf pb ptrue pfalse = undefined


pTern :: (MonadState EnvD m, MonadError IOException m) => Parser m Dynamic -> Parser m Dynamic -> Parser m Dynamic -> Parser m Dynamic
pTern pA pB pC = undefined



{-
should parse EVERY expression
-}
pExpr' :: forall m. (MonadState EnvD m, MonadError IOException m, TypeCheck m) => Parser m Dynamic
pExpr' = term
  where
    term :: Parser m Dynamic
    term = pTern pExpr' pExpr' pExpr'  <|> parens pExpr' <|> pDF <|> pDB <|> pIf pBool pExpr' pExpr' <|> pVar 

class MonadIO m => Actions m where
  -- `<expr> ; <expr>`
  seq    :: m a -> m b -> m b
  -- `Var <varname> : <type> = <expr>`, binds varname to expr in env
  assign :: [Char] -> Dynamic ->  m ()
  -- quits the program. 
  quit   :: m ()
  -- prints the environment variables.
  printEnv  :: m ()

{-
should parse:

- Expressions : `<expr>;`
- Sequentiation: `<expr> ; <expr>`
- variable assignment: `Var <varname> : <type> = <expr>`
- .q quit
- .p print environment variables
-}
pAction :: (MonadState EnvD m, MonadError IOException m, TypeCheck m, MonadIO m) => Parser m ()
pAction = pExpr' >>= (liftIO . putStrLn . show_dynamic)

pSeq :: (MonadState EnvD m, MonadError IOException m, TypeCheck m, MonadIO m) => Parser m ()
pSeq = undefined

pAssign :: (MonadState EnvD m, MonadError IOException m, TypeCheck m, MonadIO m) => Parser m ()
pAssign = undefined

pQuit :: MonadIO m => Parser m ()
pQuit = undefined

pPrint :: (MonadState EnvD m, MonadIO m) => Parser m ()
pPrint = undefined


fully :: Monad m => Parser m a -> Parser m a
fully p = spaces *> p <* eof 

parse' :: forall m. (ArithmeticSYM m, BooleanSYM m, IfSYM m, TernarySYM m, MonadError IOException m, MonadIO m)
  => EnvD -> [Char] ->  m EnvD
parse' env s  = rex 
  where


    rp :: StateT EnvD m (Either ParseError ())
    rp = runParserT (fully pAction) () "" s 

    rw :: m (Either ParseError (),EnvD)
    rw = runStateT  rp env

    rex :: m EnvD
    rex = rw >>= \case
      (Left e,env') -> liftIO (print e) $> env -- does it matter which env do we choose here? why? which is the correct one?
      (Right _, env') -> pure env'
        

repl :: forall m. (ArithmeticSYM m, BooleanSYM m, IfSYM m, TernarySYM m, MonadError IOException m, MonadIO m)
  => (m () -> IO ()) -> IO ()
repl run = do 
  hSetBuffering stdout NoBuffering
  let f env = liftIO (putStr ">> " >> getLine) >>= parse' env >>= f
  run $ f M.empty 


replE :: IO ()
replE = repl runner
  where 
  runner :: EvalRepr () -> IO ()
  runner = runEvalRepr

replS :: IO ()
replS = repl runner
  where
    runner :: NoParensPrinter () -> IO ()
    runner = (\(_,a) -> putStrLn a)  <=< flip runStateT "" .  flip runReaderT (Ctx $ -1) . runNoParensPrinter 

  