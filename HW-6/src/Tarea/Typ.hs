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

module Tarea.Typ where


import Data.Maybe
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Data.Map (Map)
import Control.Applicative

-- import Data.Kind (Constraint, Type)



data Err = Err 
  { errMsg    :: ErrMsg 
  , faultTerm :: Maybe Dynamic
  }
type ErrMsg = String
type Env a = Map String a
type EnvD  = Map String Dynamic
type TypeCheck repr = (ATypeCheck repr, BTypeCheck repr, ITypeCheck repr, TTypeCheck repr)
class ArithmeticSYM repr where
  asLit       :: Float -> repr Float
  asPlus      :: repr Float -> repr Float -> repr Float 
  asMinus     :: repr Float -> repr Float -> repr Float
  asMul       :: repr Float -> repr Float -> repr Float 
  asPower     :: repr Float -> repr Float -> repr Float 
  asPlusPlus  :: repr Float -> repr Float
  asUMinus    :: repr Float -> repr Float
  
  asFactorial :: repr Float -> repr Float

class BooleanSYM repr where
  boBool       :: Bool -> repr Bool
  boAnd        :: repr Bool -> repr Bool -> repr Bool 
  boOr         :: repr Bool -> repr Bool -> repr Bool 

class IfSYM repr where 
  ifIf :: repr Bool -> repr a -> repr a -> repr a

class TernarySYM repr where
  -- a <> b # c == (a + b) / c 
  teTern ::  repr Float -> repr Float -> repr Float  -> repr Float 


-- * The language of type representations: first-order and typed
-- It is quite like the language of int/neg/add we have seen before,
-- but it is now typed.
-- It is first order: the language of simple types is first order
class TASYM trepr where
  tFloat  :: trepr Float

class TBSYM trepr where
  tBool :: trepr Bool


newtype ShowT a = ShowT String

instance TASYM ShowT where
  tFloat = ShowT $ "Float"

instance TBSYM ShowT where
  tBool = ShowT $ "Bool"

view_t :: ShowT a -> String
view_t (ShowT s) = s

-- Can't be extended, close universe restricts us :(
-- is it a problem? Yes, a lot of duplicate code.
-- Can be solved? Probably, many ways of going here. 
-- 1. Using generics to define TSYMX to derive
--  the implementation in an automated fashion. 
--  Problem: inspect Rank2 type.
-- 2. Abstract TQ as a class and use a open family? That
newtype TQ t = TQ{ unTQ :: forall trepr. (TASYM trepr, TBSYM trepr) => trepr t }


instance TASYM TQ where
  tFloat = TQ tFloat

instance TBSYM TQ where
  tBool = TQ tBool

data ShowAs' t a = ShowAs (t a) (a -> String)

type ShowAs a = ShowAs' TQ a

instance (TASYM t) => TASYM (ShowAs' t) where
  tFloat = ShowAs tFloat show

-- Can be extended to any future type-interpreter! :)
instance (TBSYM t) => TBSYM (ShowAs' t) where
  tBool = ShowAs tBool show

-- Has to be defined for every tq.
show_as :: TQ a -> a -> String
show_as tr a = case unTQ tr of
  ShowAs @TQ t f -> f a


data TCOPY trep1 trep2 a = TCOPY (trep1 a) (trep2 a)

instance (TASYM trep1, TASYM trep2)  => TASYM (TCOPY trep1 trep2) where
  tFloat = TCOPY tFloat tFloat

-- Can be extended to any future type-interpreter! :)
instance (TBSYM trep1, TBSYM trep2)  => TBSYM (TCOPY trep1 trep2) where
  tBool = TCOPY tBool tBool

-- No need for extending. 
newtype EQU a b = EQU{ equ_cast :: forall c. c a -> c b }

-- * Leibniz equality is reflexive, symmetric and transitive
-- Here is the constructive proof
refl :: EQU a a
refl = EQU id


tran :: EQU a u -> EQU u b -> EQU a b
tran (EQU au) (EQU ub) = EQU $ ub . au

newtype FS b a = FS{unFS :: EQU a b}

symm :: forall a b. EQU a b -> EQU b a
symm equ = unFS . equ_cast equ . FS $ refl @a

-- * A constructive `deconstructor'
data AsFloat a = AsFloat (Maybe (EQU a Float))

instance TASYM AsFloat where
  tFloat = AsFloat . Just $ refl @Float

instance TBSYM AsFloat where
  tBool = AsFloat Nothing


data AsBool a = AsBool (Maybe (EQU a Bool))

instance TASYM AsBool where
  tFloat = AsBool Nothing

instance TBSYM AsBool where
  tBool =  AsBool . Just $ refl @Bool

-- can be extended, but boilerplaty
newtype SafeCast' t a = SafeCast (forall b. t b -> Maybe (EQU a b))

type SafeCast a = SafeCast' TQ a
  
instance TASYM (SafeCast' TQ) where
  tFloat = SafeCast $ \tb -> 
    case unTQ tb of AsFloat eq -> symm <$> eq

instance TBSYM (SafeCast' TQ) where
  tBool = SafeCast $ \tb -> 
    case unTQ tb of AsBool eq -> symm <$> eq


safe_gcast :: TQ a -> c a -> TQ b -> Maybe (c b)
safe_gcast (TQ ta) ca tb = cast ta
  where cast (SafeCast f) = maybe Nothing (\equ -> Just (equ_cast equ ca)) $ f tb

safe_cast :: TQ a -> a -> TQ b -> Maybe b
safe_cast ta a tb = runIdentity <$> safe_gcast ta (Identity a) tb


safe_gcast' :: MonadError Err c => Dynamic -> TQ a -> c a -> TQ b -> c b
safe_gcast' d (TQ ta) ca (TQ tn) = case (ta,tn) of
  (TCOPY (ShowT sta) (ta'), TCOPY (ShowT stn) (tn') ) ->
    let errMsg = concat ["Actual type: ", show sta, ". Expected type: ", show stn]
    in maybe (throwError $ Err errMsg (Just d)) id $ safe_gcast ta' ca tn'  


safe_cast' :: MonadError Err c => Dynamic -> TQ a -> a -> TQ b -> c b
safe_cast' d ta a tb = safe_gcast' d ta (pure a) tb

data Dynamic = forall t. Dynamic (TQ t) t


show_dynamic :: Dynamic -> String
show_dynamic (Dynamic t x) = show_as t x

class (ArithmeticSYM repr, MonadError Err repr) => ATypeCheck repr where
  tcLit :: Dynamic -> repr Float
  tcLit d@(Dynamic tr a) = safe_cast' d tr a tFloat >>= asLit
  tcPlus :: repr Float -> Dynamic -> repr Float
  tcPlus a d@(Dynamic trb b) =  asPlus a $
    safe_gcast' d trb (pure b) tFloat
  tcMul :: repr Float -> Dynamic -> repr Float
  tcMul a d@(Dynamic trb b) =  asMul a $
    safe_gcast' d trb (pure b) tFloat
  
  tcMinus :: repr Float -> Dynamic -> repr Float
  tcMinus = undefined

  tcPlusPlus  :: Dynamic -> repr Float
  tcPlusPlus  = undefined

  tcUMinus  :: Dynamic -> repr Float
  tcUMinus  = undefined

  tcFactorial  :: Dynamic -> repr Float
  tcFactorial  = undefined


class (BooleanSYM repr, MonadError Err repr) => BTypeCheck repr where
  tcBool :: Dynamic -> repr Bool
  tcBool d@(Dynamic tr a) = safe_cast' d tr a tBool >>= boBool
  tcAnd :: repr Bool -> Dynamic -> repr Bool
  tcAnd a d@(Dynamic trb b) =  boAnd a $
    safe_gcast' d trb (pure b) tBool

  tcOr :: repr Bool -> Dynamic -> repr Bool
  tcOr = undefined

-- Are these signatures enough? 
class (IfSYM repr, MonadError Err repr) => ITypeCheck repr where
  rcIf :: repr Bool -> repr a -> repr a -> repr a

class (TernarySYM repr, MonadError Err repr) => TTypeCheck repr where
  rcTern ::  repr Float -> repr Float -> repr Float  -> repr Float 

instance Monad m => ArithmeticSYM (StateT EnvD m) where
  asLit  = pure
  asPlus = liftA2 (+)
  asMul  = liftA2 (*)
  asPlusPlus  = undefined
  asUMinus    = undefined
  asFactorial = undefined
  asPower     = undefined

instance Monad m => BooleanSYM (StateT EnvD m) where
  boBool = pure
  boAnd = liftA2 (&&)
  boOr  = undefined

instance Monad m => IfSYM (StateT EnvD m) where
  ifIf = undefined

instance Monad m => TernarySYM (StateT EnvD m) where
  teTern = undefined

instance (MonadError Err m) =>  ATypeCheck  (StateT EnvD m)
instance (MonadError Err m) =>  BTypeCheck  (StateT EnvD m)
instance (MonadError Err m) =>  ITypeCheck  (StateT EnvD m)
instance (MonadError Err m) =>  TTypeCheck  (StateT EnvD m)