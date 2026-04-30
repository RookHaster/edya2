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
rbt123 (T B (T R l1 y r1) x r) = N3 x y (rbt123 l1) (rbt123 r1) (rbt123 r)
rbt123 (T B l x (T R l1 y r1)) = N3 x y (rbt123 l) (rbt123 l1) (rbt123 r1)
rbt123 (T B l x r) = N2 x (rbt123 l) (rbt123 r)

data Heap a = Eh | Nh Int a (Heap a) (Heap a) deriving(Show)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Eh = h1
merge Eh h2 = h2
merge h1@(Nh _ x a1 b1) h2@(Nh _ y a2 b2) = if x <= y then makeH x a1 (merge b1 h2) else makeH y a2 (merge h1 b2)

rank :: Heap a -> Int
rank Eh = 0
rank (Nh r _ _ _) = r

makeH x a b = if rank a >= rank b then Nh (rank b + 1) x a b else Nh (rank a + 1) x b a

fromList :: Ord a => [a] -> Heap a
fromList [] = Eh
fromList list = aux list Eh
    where
        aux :: Ord a => [a] -> Heap a -> Heap a
        aux [] heap = heap
        aux (x:xs) heap = aux xs (merge heap (Nh 0 x Eh Eh))

data PHeaps a = Empty | Root a [PHeaps a] deriving(Show)

isPHeap :: Ord a => PHeaps a -> Bool
isPHeap Empty = True
isPHeap (Root x ys) = aux x ys
    where
        aux x [] = True
        aux x (Empty:xs) = aux x xs
        aux x ((Root y ys):xs) = x <= y && aux x xs && aux y ys

mergeH :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
mergeH h1 Empty = h1
mergeH Empty h2 = h2
mergeH h1@(Root x xs) h2@(Root y ys) = if x <= y then Root x (h2:xs) else Root y (h1:ys)

insertH :: Ord a => PHeaps a -> a -> PHeaps a
insertH raiz x = mergeH raiz (Root x [])

concatH :: Ord a => [PHeaps a] -> PHeaps a
concatH [] = Empty
concatH (x:xs) = mergeH x (concatH xs)

delMin :: Ord a => PHeaps a -> Maybe (a, PHeaps a)
delMin Empty = Nothing
delMin (Root x xs) = Just (x, concatH xs)

ejemploHeap1 = Root 1 [Root 3 [Root 4 [], Root 5 []], Root 8 [Root 9 [], Root 10 [], Root 11 []], Root 2 [Root 6 [], Root 7 [Root 12 []]]]
ejemploHeap2 = Root 2 [Root 5 [Root 10 [Root 20 [Root 40 []], Root 25 []], Root 15 [Root 30 []]], Root 8 [Root 12 []]]