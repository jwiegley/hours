{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall #-}

module Hours.Budget where

import Data.Foldable (Foldable(toList))
import Data.Semigroup (Semigroup((<>)), Sum(Sum, getSum))
import Data.These (These(..))
import Data.Typeable
import GHC.Generics

data Interval t a = Interval
    { begin :: t                -- ^ inclusive begin
    , end   :: t                -- ^ exclusive end
    , value :: a                -- ^ value associated with interval
    }
    deriving (Eq, Ord, Show,
              Functor, Foldable, Traversable,
              Generic, Typeable)

instance (Ord t, Semigroup a) => Semigroup (Interval t a) where
    x <> y = Interval
        (if begin x < begin y then begin x else begin y)
        (if end x > end y then end x else end y)
        (value x <> value y)

class HasDelta t u | t -> u where
    delta :: t -> t -> u

progress :: (HasDelta t u, Fractional u) => Interval t a -> t -> u
progress i t = delta t (begin i) / delta (end i) (begin i)

within :: (Ord t) => t -> Interval t a -> Bool
within t i = begin i <= t && t < end i

foldMapBounds :: (Foldable f, Monoid m)
              => (t -> t -> m) -> f (Interval t a) -> m
foldMapBounds f = foldMap (\i -> f (begin i) (end i))

boundsToValue :: HasDelta t u => Interval t a -> Interval t u
boundsToValue i = Interval { begin = begin i
                           , end = end i
                           , value = delta (end i) (begin i)
                           }

mapValues :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
mapValues f = fmap (fmap f)

sumValues :: (Foldable f, Foldable g) => Num a => f (g a) -> a
sumValues = getSum . foldMap (foldMap Sum)

distance :: (Foldable f, HasDelta t u, Num u) => f (Interval t a) -> u
distance = getSum . foldMapBounds (\b e -> Sum (delta b e))

splitIntervals :: (Foldable f, Ord t)
               => t -> f (Interval t a)
               -> ([Interval t a], Maybe (Interval t a), [Interval t a])
splitIntervals t xs =
    ( takeWhile (\i -> end i <= t) l
    , case filter (within t) l of
          []  -> Nothing
          [a] -> Just a
          _   -> error "overlapping intervals"
    , dropWhile (\i -> begin i <= t) l
    )
  where
    l = toList xs

current :: (Foldable f, Ord t)
        => t -> f (Interval t a) -> Maybe (Interval t a)
current t xs = case splitIntervals t xs of (_, cur, _) -> cur

mostRecent :: (Foldable f, Ord t)
           => t -> f (Interval t a) -> Maybe (Interval t a)
mostRecent t xs = case splitIntervals t xs of
    (_, Just x, _) -> Just x
    ([], _, _)     -> Nothing
    (ys, _, _)     -> Just (last ys)

activeIntervals :: (Foldable f, Ord t)
                => t -> f (Interval t a) -> ([Interval t a], [Interval t a])
activeIntervals t xs = case splitIntervals t (toList xs) of
    (before, mcur, after) ->
        case mcur of
            Nothing -> (before, after)
            Just c  -> (before ++ [c], after)

showIntervals :: (Foldable f, Show t, Show a) => f (Interval t a) -> String
showIntervals ints = go True (toList ints) ++ "\n]"
  where
    go _ [] = ""
    go first (x:xs) =
        (if first then "[ " else ", ") ++ show x ++ "\n" ++ go False xs

-- The code below is getting much more concrete, but lets you make more
-- value-aware splits of interval collections.

class Scalable t u where
    scale :: t -> u -> t

breakAt :: (Ord t, HasDelta t u, Scalable a u, Num u, Fractional u)
        => t -> Interval t a -> These (Interval t a) (Interval t a)
breakAt t i | within t i =
    let perc = progress i t in
    These (Interval (begin i) t (scale (value i) perc))
          (Interval t (end i) (scale (value i) (1 - perc)))
breakAt t i | t < begin i = This i
            | otherwise   = That i

divideIntervals :: (Foldable f, Ord t, HasDelta t u, Scalable a u, Num u,
                    Fractional u)
                => t -> f (Interval t a) -> ([Interval t a], [Interval t a])
divideIntervals t xs = case splitIntervals t (toList xs) of
    (before, mcur, after) -> case mcur of
        Nothing -> (before, after)
        Just c -> case breakAt t c of
            This x    -> (before ++ [x], after)
            That y    -> (before, y : after)
            These x y -> (before ++ [x], y : after)

valueProgress :: (Foldable f, Ord t, HasDelta t u, Scalable a u, Num u,
                  Fractional u)
              => t -> f (Interval t a) -> u
valueProgress t xs = distance before / distance xs
  where
    (before, _) = divideIntervals t xs
