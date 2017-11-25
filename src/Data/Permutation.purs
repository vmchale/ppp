module Data.Permutation ( P (..)
                        , toArray
                        , identity
                        , pi
                        , multiply
                        , invert
                        ) where

import Prelude

import Data.Array (uncons, (:), index)
import Data.Maybe (Maybe(..))

data P n = Nil | Cons n (P n)

derive instance eqP :: Eq n => Eq (P n)

type Permutation = P Int

instance showP :: Show (P Int) where
    show Nil = "Nil"
    show (Cons x xs) = "(Cons " <> show x <> " " <> show xs <> ")"

identity :: Int -> Permutation
identity 0 = Nil
identity n = Cons 0 (identity $ n - 1)

insert :: Int -> Array Int -> Int -> Array Int
insert def l 0 = def : l
insert def [] _ = [def]
insert def l k = case uncons l of
    (Just { head: x, tail: xs }) -> x : insert def xs (k - 1)
    Nothing -> [] -- impossible

sigma :: Permutation -> Array Int -> Array Int
sigma Nil _ = []
sigma (Cons p ps) l = case uncons l of
    (Just { head: x, tail: xs }) -> insert x (sigma ps xs) p
    Nothing -> []

toArray :: Int -> Permutation -> Array Int
toArray n p = sigma p (sequential n)
    where sequential :: Int -> Array Int
          sequential 0 = []
          sequential m = 0 : map (add 1) (sequential $ m - 1)

delete :: Int -> Int -> Permutation -> Permutation
delete _ 0 (Cons j p) = p
delete 0 _ _ = Nil
delete n i (Cons 0 p) = Cons 0 (delete (n-1) (i-1) p)
delete n i (Cons j p) = Cons j (delete (n-1) i p)
delete _ _ _ = Nil -- shouldn't ever happen

multiply :: Int -> Permutation -> Permutation -> Permutation
multiply _ Nil p = p
multiply n (Cons i p) p' = case index (toArray n p') i of
    Just x -> Cons x (multiply (n-1) p (delete n i p'))
    Nothing -> Nil -- shouldn't ever happen

-- | Invert a permutation of a given size.
invert :: Int -> Permutation -> Permutation
invert _ Nil = Nil
invert n p@(Cons i is) = case index (toArray n p) j of
    Nothing -> Nil -- should never happen
    Just x -> Cons x (delete n j p)
        where j = case index (toArray n p) i of
                    Just x -> x
                    Nothing -> 0 -- shouldn't happen

fill :: Int -> Int -> Permutation
fill n 0 = identity n
fill n k = Cons 1 (fill (n-1) $ k - 1)

-- | Permutation from a swap.
pi :: Int -> Int -> Int -> Permutation
pi n 0 0 = identity n
pi n 0 k = Cons k (fill (n-1) $ k - 1)
pi n j 0 = Cons j (fill (n-1) $ j - 1)
pi n j k = Cons 0 (pi (n-1) (j-1) (k-1))
