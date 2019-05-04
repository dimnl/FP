-- Imports
import FPPrac.Trees
import Data.List
import Test.QuickCheck
import Data.Maybe

-- Ex 1
data Tree1a = Leaf1a Int
            | Node1a Int Tree1a Tree1a
            deriving (Show, Eq)

pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a n)     = RoseNode (show n) []
pp1a (Node1a n l r) = RoseNode (show n) [pp1a l,  pp1a r]

tree1a :: Tree1a
tree1a = Node1a 1 (Leaf1a 40)
                (Node1a 3 (Leaf1a 5)
                          (Leaf1a 2))

-- 2
data Tree1b = Leaf1b (Int, Int)
            | Node1b (Int, Int) Tree1b Tree1b
            deriving (Show, Eq)

pp1b :: Tree1b -> RoseTree
pp1b (Leaf1b n)     = RoseNode (show n) []
pp1b (Node1b n l r) = RoseNode (show n) [pp1b l, pp1b r]

tree1b :: Tree1b
tree1b = Node1b (1,2) (Leaf1b (40,3))
                (Node1b (3,4) (Leaf1b (5,5))
                              (Leaf1b (2,6)))
-- 3
data Tree1c = Leaf1c Int
            | Node1c Tree1c Tree1c
            deriving (Show, Eq)
pp1c :: Tree1c -> RoseTree
pp1c (Leaf1c n)   = RoseNode (show n) []
pp1c (Node1c l r) = RoseNode "" [pp1c l, pp1c r]

tree1c :: Tree1c
tree1c = Node1c (
                  Node1c (Leaf1c 3) (Leaf1c 4)
                )
                (
                  Node1c (Leaf1c 5) (Leaf1c 6)
                )
-- 4
data Tree1d = Leaf1d (Int, Int)
            | Node1d [Tree1d]
            deriving (Show, Eq)

pp1d :: Tree1d -> RoseTree
pp1d (Leaf1d n) = RoseNode (show n) []
pp1d (Node1d a) = RoseNode "" (map pp1d a)

tree1d :: Tree1d
tree1d = Node1d [
            Node1d [
              Leaf1d (4,5),
              Leaf1d (1,2),
              Node1d [
                  Leaf1d (9,0)
                 ]
              ],
                  Leaf1d (3,4),
                  Leaf1d (5,6)
        ]
-- 5
class PP a where
  pp :: a -> RoseTree

instance PP Tree1a where
  pp = pp1a

instance PP Tree1b where
  pp = pp1b

instance PP Tree1c where
  pp = pp1c

instance PP Tree1d where
  pp = pp1d

-- Ex 2
treeAdd :: Tree1a -> Tree1a
treeAdd (Leaf1a n)      = Leaf1a (n+1)
treeAdd (Node1a n l r)  = Node1a (n+1) (treeAdd l) (treeAdd r)

-- 2
treeSquare :: Tree1a -> Tree1a
treeSquare (Leaf1a n)     = Leaf1a (n*n)
treeSquare (Node1a n l r) = Node1a (n*n) (treeSquare l) (treeSquare r)

-- 3
mapTree :: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a n)      = Leaf1a $ f  n
mapTree f (Node1a n l r)  = Node1a (f n)  (mapTree f l) (mapTree f r)

treeAdd2:: Tree1a -> Tree1a
treeAdd2 = mapTree (+1)

treeSquare2 :: Tree1a -> Tree1a
treeSquare2 = mapTree (\x -> x*x)

-- 4
addNode :: Tree1b -> Tree1a
addNode (Leaf1b (x1,x2))      = Leaf1a (x1 + x2)
addNode (Node1b (x1, x2) l r) = Node1a (x1 + x2) (addNode l) (addNode r)

-- 5
mapTree1b :: ((Int,Int) -> Int) -> Tree1b -> Tree1a
mapTree1b f (Leaf1b n)      = Leaf1a $ f n
mapTree1b f (Node1b n l r ) = Node1a (f n) (mapTree1b  f l) (mapTree1b f r)

binAdd1b     = mapTree1b (\(x,y) -> x+y)
binSubXY1b   = mapTree1b (\(x,y) -> x-y)
binSubYX1b   = mapTree1b (\(x,y) -> y-x)
binMult1b    = mapTree1b (\(x,y) -> x*y)

showBin f = showRoseTree (pp1a $ f tree1b)

-- Ex 3
binMirror1a :: Tree1a -> Tree1a
binMirror1a (Leaf1a n)      = Leaf1a n
binMirror1a (Node1a n l r)  = Node1a n (binMirror1a r) (binMirror1a l)

checkBinMirror1a = showRoseTree (pp1a $ binMirror1a $ binMirror1a  tree1a  )

-- 2
class BinMirror a where
  binMirror :: a -> a

instance BinMirror Tree1a where
  binMirror = binMirror1a

-- 3
instance BinMirror Tree1d where
  binMirror = binMirror1d

binMirror1d :: Tree1d -> Tree1d
binMirror1d (Leaf1d (x1, x2)) = Leaf1d (x2, x1)
binMirror1d (Node1d a)        = Node1d  ( map binMirror1d (reverse a))

-- Ex 4
data TreeInt = LeafInt
            | NodeInt Int TreeInt TreeInt
            deriving (Show, Eq)

-- Below two functions used for testing
-- Use e.g. showIntTree $  makeTreeF [10,5,1,123,5,7,3,2]
showIntTree :: TreeInt -> IO ()
showIntTree = showRoseTree . transformIntTree

transformIntTree :: TreeInt -> RoseTree
transformIntTree LeafInt         = RoseNode "" []
transformIntTree (NodeInt n l r) = RoseNode (show n) [transformIntTree l,  transformIntTree r]

-- 1
insertTree :: Int -> TreeInt -> TreeInt
insertTree x LeafInt      = NodeInt x LeafInt LeafInt
insertTree x (NodeInt i l r)
  | x <= i      = NodeInt i (insertTree x l) r
  | otherwise   = NodeInt i l (insertTree x r)

-- 2
makeTreeR :: [Int] -> TreeInt
makeTreeR []      = LeafInt
makeTreeR (x:xs)  = insertTree x (makeTreeR xs)

makeTreeF :: [Int] -> TreeInt
makeTreeF = foldr insertTree LeafInt

-- 3
makeList :: TreeInt -> [Int]
makeList LeafInt          = []
makeList (NodeInt x l r)  = makeList l ++ [x] ++ makeList r

-- 4
treeSort :: [Int] -> [Int]
treeSort = makeList . makeTreeF

prop_treeSort :: [Int] -> Bool
prop_treeSort x = sort x == treeSort x

-- 5
sortTree :: TreeInt -> TreeInt
sortTree = makeTreeF . makeList

-- Ex 5
subtreeAt :: TreeInt -> Int -> Maybe TreeInt
subtreeAt LeafInt n = Nothing
subtreeAt (NodeInt i l r) n
  | n == i    = Just $ NodeInt n l r
  | n < i     = subtreeAt l n
  | otherwise = subtreeAt r n

-- Ex 6
-- Tested with: showIntTree $ cutOffAt ( makeTreeF [10,5,1,123,5,7,3,2]) 2
cutOffAt :: TreeInt -> Int -> TreeInt
cutOffAt LeafInt _  = LeafInt
cutOffAt(NodeInt i l r) d
  | d <= 0    = LeafInt
  | otherwise = NodeInt i (cutOffAt l (d-1)) (cutOffAt r (d-1))

-- Ex 7
data BinTree a  = Leaf
                | Node a (BinTree a) (BinTree a)
                deriving (Show, Eq)

-- 1
instance Show a => PP (BinTree a) where
  pp Leaf         = RoseNode "" []
  pp (Node n l r) = RoseNode (show n) [pp l,  pp r]
-- 2
instance BinMirror (BinTree a) where
  binMirror Leaf          = Leaf
  binMirror (Node n l r)  = Node n (binMirror r) (binMirror l)
-- 3
instance Functor BinTree where
  fmap f Leaf         = Leaf
  fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r)

-- Ex 8
data MyList a = Nil | Cons a (MyList a)
              deriving (Show, Eq)

mylst = Cons 1 $ Cons 2 $ Cons 3 $ Nil
-- 1
instance Functor MyList where
  fmap f Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)
-- 2
fromList :: [a] -> MyList a
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)
-- 3
-- Functor laws slide 120 /141
prop_firstFLaw :: Eq a=> [a] -> Bool
prop_firstFLaw x = id mylist == fmap id mylist
  where mylist = fromList x
-- 4
-- Only tested for two arbitrary functions
prop_secondFLaw :: [Int] -> Bool
prop_secondFLaw x = fmap (f . g) mylist == fmap f (fmap g mylist)
  where mylist = fromList x
        f = (+10)
        g = (*4)

-- Ex 9
