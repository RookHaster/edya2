module P4 where

data Btree a = BEm | BNode (Btree a) a (Btree a) deriving(Show)

data BST a = Em | Node (BST a) a (BST a) deriving(Eq, Ord, Show)

completo :: a -> Int -> Btree a
completo _ 0 = BEm
completo a n = BNode (completo a (n-1)) a (completo a (n-1))

balanceado :: a -> Int -> Btree a
balanceado _ 0 = BEm
balanceado a n = if even n then BNode (balanceado a (div n 2)) a (balanceado a (div (n-1) 2)) else BNode (balanceado a (div n 2)) a (balanceado a (div n 2))

maximo :: BST a -> a
maximo (Node _ a Em) = a
maximo (Node _ _ r) = maximo r

data Extended a = MinInf | Val a | MaxInf deriving(Eq, Ord)

checkBST :: Ord a => BST a -> Bool
checkBST Em = True
checkBST raiz = aux raiz MinInf MaxInf
    where
        aux :: Ord a => BST a -> Extended a -> Extended a -> Bool
        aux Em _ _ = True
        aux (Node l a r) inf sup = (Val a >= inf) && (Val a <= sup) && (aux l inf (Val a)) && (aux r (Val a) sup)


splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Em _ = (Em,Em)
splitBST (Node l y r) x | x == y = (Node l y Em, r)
                        | x < y = let (l', r') = splitBST l x in (l', Node r' y r)
                        | x > y = let (l', r') = splitBST r x in (Node l y l', r')

data Color = R | B deriving(Show)
data RBT a = E | T Color (RBT a) a (RBT a) deriving(Show)

lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c l a r = T c l a r

rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l a r = T c l a r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack(ins x t)
    where 
        ins x E = T R E x E
        ins x (T c l y r) | x < y = lbalance c (ins x l) y r 
                          | x > y = rbalance c l y (ins x r)
                          | otherwise = T c l y r
        makeBlack E = E
        makeBlack (T _ l x r) = T B l x r

rbt = T B (T B (T R E 3 E) 5 E) 10 (T R (T B E 12 E) 15 (T B (T R E 18 E) 20 E))

data T123 a = E123 | N2 a (T123 a) (T123 a) | N3 a a (T123 a) (T123 a) (T123 a) | N4 a a a (T123 a) (T123 a) (T123 a) (T123 a) deriving(Show)

rbt123 :: RBT a -> T123 a
rbt123 E = E123
rbt123 (T B (T R l1 y r1) x (T R l2 z r2)) = N4 x y z (rbt123 l1) (rbt123 r1) (rbt123 l2) (rbt123 r2)
rbt123 (T B l@(T R l1 y r1) x r@(T B _ _ _)) = N3 x y (rbt123 l1) (rbt123 r1) (rbt123 r)
rbt123 (T B l@(T B _ _ _) x r@(T R l1 y r1)) = N3 x y (rbt123 l) (rbt123 l1) (rbt123 r1)
rbt123 (T B l@(T R l1 y r1) x E) = N3 x y (rbt123 l1) (rbt123 r1) E123
rbt123 (T B E x r@(T R l1 y r1)) = N3 x y E123 (rbt123 l1) (rbt123 r1)
rbt123 (T B l x r) = N2 x (rbt123 l) (rbt123 r)
