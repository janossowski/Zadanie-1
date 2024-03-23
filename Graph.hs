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
  g1 == g2 = toRelation(fromBasic g1) == toRelation(fromBasic g2) where
    toRelation :: Relation a -> Relation a
    toRelation = id

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
fromBasic Empty = empty
fromBasic (Vertex a) = vertex a
fromBasic (Union g1 g2) = union (fromBasic g1) (fromBasic g2)
fromBasic (Connect g1 g2) = connect (fromBasic g1) (fromBasic g2)

instance (Ord a, Show a) => Show (Basic a) where
  show g = "edges " ++ show edgesStr ++ " + vertices " ++ show isolatedVertices
    where
      relationGraph = toRelation(fromBasic g) where
        toRelation :: Relation a -> Relation a
        toRelation = id
      edgesList = Set.toList $ relation relationGraph
      edgesStr = map (\(x, y) -> "(" ++ show x ++ "," ++ show y ++ ")") edgesList
      allVertices = Set.toList $ domain relationGraph
      isolatedVertices = filter (\v -> not (any (\(x, y) -> v == x || v == y) edgesList)) allVertices


-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g = "digraph {\n" ++ unlines (map showEdge edgesList) ++ unlines (map showIsolatedVertex isolatedVertices) ++ "}"
  where
    relationGraph = toRelation(fromBasic g) where
      toRelation :: Relation a -> Relation a
      toRelation = id
    edgesList = Set.toList $ relation relationGraph
    allVertices = Set.toList $ domain relationGraph
    isolatedVertices = filter (\v -> not (any (\(x, y) -> v == x || v == y) edgesList)) allVertices

    showEdge :: (Show a) => (a, a) -> String
    showEdge (x, y) = "  " ++ show x ++ " -> " ++ show y ++ ";"

    showIsolatedVertex :: (Show a) => a -> String
    showIsolatedVertex v = "  " ++ show v ++ ";"


instance Functor Basic where
  fmap _ Empty = Empty
  fmap f (Vertex a) = Vertex (f a)
  fmap f (Union g1 g2) = Union (fmap f g1) (fmap f g2)
  fmap f (Connect g1 g2) = Connect (fmap f g1) (fmap f g2)
  
-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV _ _ _ Empty = Empty
mergeV old1 old2 new (Vertex x)
  | x == old1 || x == old2 = Vertex new
  | otherwise = Vertex x
mergeV old1 old2 new (Union l r) = Union (mergeV old1 old2 new l) (mergeV old1 old2 new r)
mergeV old1 old2 new (Connect l r) = Connect (mergeV old1 old2 new l) (mergeV old1 old2 new r)

instance Applicative Basic where

instance Monad Basic where

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV = undefined
