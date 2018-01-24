{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Budget where

import Data.Foldable (Foldable(toList))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup((<>)), Sum(Sum, getSum))
import Data.These (These(..))

data Interval t a = Interval
    { intBeg :: t                -- ^ inclusive begin
    , intEnd :: t                -- ^ exclusive end
    , intVal :: a                -- ^ value associated with interval
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Ord t, Semigroup a) => Semigroup (Interval t a) where
    x <> y = Interval
        (if intBeg x < intBeg y then intBeg x else intBeg y)
        (if intEnd x > intEnd y then intEnd x else intEnd y)
        (intVal x <> intVal y)

foldMapBounds :: (Foldable f, Monoid m) => (t -> t -> m) -> f (Interval t a) -> m
foldMapBounds f = foldMap (\i -> f (intBeg i) (intEnd i))

foldMapValues :: (Foldable f, Monoid m) => (a -> m) -> f (Interval t a) -> m
foldMapValues f = foldMap (f . intVal)

boundsToValue :: (t -> t -> u) -> Interval t a -> Interval t u
boundsToValue delta i = i { intVal = delta (intEnd i) (intBeg i) }

sumRange :: Num a => [Interval t a] -> a
sumRange = getSum . foldMapValues Sum

-- | 'start' requires a non-empty container of intervals.
start :: Ord t => NonEmpty (Interval t a) -> t
start (x :| xs) = intBeg . head $ sortOn intBeg (x:xs)

end :: Ord t => NonEmpty (Interval t a) -> t
end (x :| xs) = intEnd . last $ sortOn intEnd (x:xs)

within :: Ord t => t -> Interval t a -> Bool
within t i = intBeg i <= t && t < intEnd i

intProgressAbs :: Fractional u => (t -> t -> u) -> t -> Interval t a -> u
intProgressAbs delta t i = delta t (intBeg i) / delta (intEnd i) (intBeg i)

progressAbs :: (Ord t, Fractional u)
            => (t -> t -> u) -> t -> NonEmpty (Interval t a) -> u
progressAbs delta t xs = delta t (start xs) / delta (end xs) (start xs)

progress :: (Foldable f, Ord t, Fractional u)
         => (t -> t -> u) -> t -> f (Interval t a) -> u
progress delta t xs = distance before / distance (toList xs)
  where
    distance = getSum . foldMapBounds (\b e -> Sum (delta b e))
    (before, _) = divideIntervals delta const t xs

splitIntervals :: (Foldable f, Ord t)
               => t -> f (Interval t a)
               -> ([Interval t a], Maybe (Interval t a), [Interval t a])
splitIntervals t xs =
    ( takeWhile (\i -> intEnd i <= t) l
    , case filter (within t) l of
          []  -> Nothing
          [a] -> Just a
          _   -> error "Overlapping intervals"
    , dropWhile (\i -> intBeg i <= t) l
    )
  where
    l = toList xs

current :: Ord t => t -> [Interval t a] -> Maybe (Interval t a)
current t xs = case splitIntervals t xs of (_, cur, _) -> cur

mostRecent :: (Foldable f, Ord t)
           => t -> f (Interval t a) -> Maybe (Interval t a)
mostRecent t xs = case splitIntervals t xs of
    (_, Just x, _) -> Just x
    ([], _, _)     -> Nothing
    (ys, _, _)     -> Just (last ys)

breakAt :: (Ord t, Fractional u)
        => (t -> t -> u) -> (a -> u -> a) -> t -> Interval t a
        -> These (Interval t a) (Interval t a)
breakAt delta mult t i | within t i =
    let perc = intProgressAbs delta t i in
    These (Interval (intBeg i) t (mult (intVal i) perc))
          (Interval t (intEnd i) (mult (intVal i) (1 - perc)))
breakAt _ _ t i | t < intBeg i = This i
                | otherwise    = That i

divideIntervals :: (Foldable f, Ord t, Fractional u)
               => (t -> t -> u) -> (a -> u -> a) -> t -> f (Interval t a)
               -> ([Interval t a], [Interval t a])
divideIntervals delta mult t xs = case splitIntervals t (toList xs) of
    (before, mcur, after) -> case mcur of
        Nothing -> (before, after)
        Just c -> case breakAt delta mult t c of
            This x    -> (before ++ [x], after)
            That y    -> (before, y : after)
            These x y -> (before ++ [x], y : after)

activeIntervals :: (Foldable f, Ord t)
               => t -> f (Interval t a)
               -> ([Interval t a], [Interval t a])
activeIntervals t xs = case splitIntervals t (toList xs) of
    (before, mcur, after) -> case mcur of
        Nothing -> (before, after)
        Just c  -> (before ++ [c], after)

valueProgress :: (Foldable f, Ord t, Monoid a, Fractional u, Fractional a)
              => (t -> t -> u) -> (a -> u -> a) -> t -> f (Interval t a)
              -> a
valueProgress delta mult t xs = foldMapValues id before / foldMapValues id xs
  where
    (before, _) = divideIntervals delta mult t xs

showIntervals :: (Show t, Show a) => [Interval t a] -> String
showIntervals ints = go True ints ++ "\n]"
  where
    go _ [] = ""
    go first (x:xs) =
        (if first then "[ " else ", ") ++ show x ++ "\n" ++ go False xs
