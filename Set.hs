module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)
import Data.List (nub, sort)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a

null :: Set a -> Bool

member :: Eq a => a -> Set a -> Bool

singleton :: a -> Set a

fromList :: [a] -> Set a

toList :: Set a -> [a]

toAscList :: Ord a => Set a -> [a]

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a

insert :: a -> Set a -> Set a

instance Ord a => Eq (Set a) where
    x == y = toAscList x == toAscList y

instance Semigroup (Set a) where
    x <> y = fromList (toList x ++ toList y)

instance Monoid (Set a) where
    mempty = empty

instance Show a => Show (Set a) where
    show x = show (toList x)

instance Functor Set where
    fmap _ Empty = Empty
    fmap f (Singleton x) = Singleton(f x)
    fmap f (Union l r) = Union (fmap f l) (fmap f r)

empty = Empty

null Empty = True
null _ = False

member _ Empty = False
member x (Singleton y) = x == y
member x (Union l r) = member x l || member x r

singleton = Singleton

fromList = foldr insert Empty

toList Empty = []
toList (Singleton x) = [x]
toList (Union l r) = toList l ++ toList r

toAscList s = sort (nub (toList s))

union s Empty = s
union Empty s = s
union l r = Union l r

insert x = Union (Singleton x)