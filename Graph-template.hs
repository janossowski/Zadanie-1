module Graph where
import Set(Set)
import qualified Set as Set
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

instance Graph Relation where
  empty = Relation Set.empty Set.empty
  vertex a = Relation (Set.singleton a) Set.empty
  union (Relation d1 r1) (Relation d2 r2) = Relation (d1 `Set.union` d2) (r1 `Set.union` r2)
  connect (Relation d1 r1) (Relation d2 r2) = Relation (d1 `Set.union` d2) (r1 `Set.union` r2 `Set.union` connectedEdges)
    where
      combinedVertices = [(x, y) | x <- Set.toList d1, y <- Set.toList d2]
      connectedEdges = Set.fromList combinedVertices

instance (Ord a, Num a) => Num (Relation a) where
  fromInteger n = Relation (Set.singleton (fromInteger n)) Set.empty
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id

instance Graph Basic where
  empty = Empty
  vertex a = Vertex a
  union g1 g2 = Union g1 g2
  connect g1 g2 = Connect g1 g2

instance Ord a => Eq (Basic a) where

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: Graph g => Basic a -> g a

instance (Ord a, Show a) => Show (Basic a) where

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot = undefined

instance Functor Basic where

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV = undefined

instance Applicative Basic where

instance Monad Basic where

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV = undefined

