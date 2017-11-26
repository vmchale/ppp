module Data.Permutation ( P
                        , toArray
                        , identity
                        , pi
                        , multiply
                        , invert
                        , size
                        , fixNat
                        ) where

import Prelude

import Data.Array (uncons, (:), index)
import Data.Maybe (Maybe(..))

data P n = Zero | Cons n (P n)

infixr 5 Cons as :~

derive instance eqP :: Eq n => Eq (P n)

type Permutation = P Int

instance showP :: Show (P Int) where
    show Zero = "Zero"
    show (x :~ xs) = "(" <> show x <> " :~ " <> show xs <> ")"

fixNat :: Int -> Permutation -> Maybe Int
fixNat i p = flip index i =<< (toArray p)

size :: Permutation -> Int
size Zero = 0
size (_:~xs) = 1 + size xs

identity :: Int -> Permutation
identity 0 = Zero
identity n = 0 :~ (identity $ n - 1)

insert :: Int -> Array Int -> Int -> Array Int
insert def l 0 = def : l
insert def l k = case uncons l of
    (Just { head: x, tail: xs }) -> x : insert def xs (k - 1)
    Nothing -> [def]

sigma :: Permutation -> Array Int -> Maybe (Array Int)
sigma Zero _ = Just []
sigma (p :~ ps) l = do
    { head: x, tail: xs } <- uncons l
    insert x <$> (sigma ps xs) <*> pure p

toArray :: Permutation -> Maybe (Array Int)
toArray p = sigma p (sequential $ size p)
    where sequential :: Int -> Array Int
          sequential 0 = []
          sequential m = 0 : map (add 1) (sequential $ m - 1)

delete :: Int -> Permutation -> Maybe Permutation
delete 0 (j :~ p) = Just p
delete _ p | size p == 1 = Just p
delete i pe@(0 :~ p) | size pe > 0 = Cons 0 <$> (delete (i-1) p)
delete i (j :~ p) | size p > 0 = Cons j <$> (delete i p)
delete _ _ = Nothing

multiply :: Int -> Permutation -> Permutation -> Maybe Permutation
multiply 0 Zero p = Just Zero
multiply _ Zero _ = Nothing
multiply _ (i :~ p) p' = do
    j <- flip index i =<< (toArray p')
    Cons j <$> (multiply (size p) p =<< (delete i p'))

-- | Invert a permutation of a given size.
invert :: Int -> Permutation -> Maybe Permutation
invert 0 Zero = Just Zero
invert _ Zero = Nothing
invert n p@(i :~ is) = do
    j <- flip index i =<< (toArray p)
    k <- flip index j =<< (toArray p)
    Cons k <$> (delete j p)

fill :: Int -> Int -> Permutation
fill n 0 = identity n
fill n k = 1 :~ (fill (n-1) $ k - 1)

-- | Permutation from a swap.
pi :: Int -> Int -> Int -> Permutation
pi n 0 0 = identity n
pi n 0 k = k :~ (fill (n-1) $ k - 1)
pi n j 0 = j :~ (fill (n-1) $ j - 1)
pi n j k = 0 :~ (pi (n-1) (j-1) (k-1))
