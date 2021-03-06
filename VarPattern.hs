{-# LANGUAGE TypeSynonymInstances #-}
module VarPattern where

import Data.Monoid

newtype Pattern a b = Pat { unPat:: (a -> b) -> (a -> b) }

runPat :: Pattern a b -> (a -> b) -> (a -> b)
runPat = unPat

instance Monoid (Pattern a b) where
    mempty = Pat id
    mappend (Pat g) (Pat f) = Pat $ g . f
         
pcase :: [Pattern a b] -> Pattern a b
pcase = mconcat

f = Pat $ \c a -> case a of
             [x] -> x
             _   -> c a

g = Pat $ \c a -> case a of 
              (1 : x : []) -> x
              _   -> c a

test = runPat (pcase [f,g]) head [1,3] 


{-
data Match a b = Match (a -> b)
               | NoMatch
             

type Case a b = Pattern a b
type Pattern a b = a -> Match a b

data List a = Nil
            | Cons a (List a)
            
pNil :: b -> Pattern (List a) b
pNil n a = case a of
              Nil ->  Match (\Nil -> n)
              _   ->  NoMatch

pCons :: (a -> List a -> b) -> Pattern (List a) b
pCons f a = case a of
              Cons _ _ -> Match (\(Cons s t) -> f s t)
              _ -> NoMatch
                            
f = \ a -> case a of
             [x] -> Match $ \ [x] -> x
             _   -> NoMatch
g = \ a -> case a of 
             (x : 1 : []) -> Match $ \ (x : 1 : []) -> x
             _   -> NoMatch
 

comp :: Pattern a b -> Pattern a b -> Pattern a b
comp f g = \a -> case f a of
                   Match f -> Match f
                   NoMatch -> g a 


pcase :: [Pattern a b] -> (a->b) -> a -> b
pcase cs d a = case (foldr comp (const NoMatch) cs) a of
                 Match f -> f a
                 NoMatch -> d a




-- h :: (a -/> b) -> Pattern a b
-- h = 
-}