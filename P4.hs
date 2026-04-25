module P4 where
import Distribution.Simple.Command (OptDescr(BoolOpt))

data Btree a = BEm | BNode (Btree a) a (Btree a) deriving(Show)

data BST a = Em | Node (BST a) a (BST a) deriving(Eq, Ord, Show)

completo :: a -> Int -> Btree a
completo _ 0 = BEm
completo a n = BNode (completo a (n-1)) a (completo a (n-1))

balanceado :: a -> Int -> Btree a
balanceado _ 0 = BEm
balanceado a n = if (mod n 2) == 0 then BNode (balanceado a (div n 2)) a (balanceado a (div (n-1) 2)) else BNode (balanceado a (div n 2)) a (balanceado a (div n 2))

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
