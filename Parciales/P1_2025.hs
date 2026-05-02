module P1_2025 where

data Scapegoat a = E | N a Int (Scapegoat a) (Scapegoat a) deriving(Eq, Show)

-- a
size :: Scapegoat a -> Int
size E = 0
size (N _ x _ _) = x

-- b
data Ext a = MinInf | Val a | MaxInf deriving(Eq, Ord)

isBst :: Ord a => Scapegoat a -> Bool
isBst E = True
isBst raiz = aux raiz MinInf MaxInf
    where
        aux :: Ord a => Scapegoat a -> Ext a -> Ext a -> Bool
        aux E _ _ = True
        aux (N x _ l r) inf sup = (Val x >= inf) && (Val x <= sup) && (aux l inf (Val x)) && (aux r (Val x) sup)

-- c
isScapegoat :: Ord a => Scapegoat a -> Bool
isScapegoat E = True
isScapegoat raiz = isBst raiz && aux raiz
    where 
        aux E = True
        aux (N _ x l r) = (fromIntegral (size l) / fromIntegral x) <= 2/3 && (fromIntegral (size r) / fromIntegral x) <= 2/3 && aux l && aux r

-- d
member :: Ord a => a -> Scapegoat a -> Bool
member _ E = False
member x (N y _ l r) | x > y = member x r
                     | x < y = member x l
                     | otherwise = True


-- e 
rebuild :: Scapegoat a -> Scapegoat a
rebuild E = E
rebuild raiz = fromList (inorder raiz)
    where 
        inorder :: Scapegoat a -> [a]
        inorder E = []
        inorder (N x _ l r) = (inorder l) ++ [x] ++ (inorder r)

        fromList :: [a] -> Scapegoat a
        fromList [] = E
        fromList lista = let l = length lista 
                             (izq, der) = splitAt (div l 2) lista
                             in (N (lista !! (div l 2)) l (fromList izq) (fromList (drop 1 der)))

-- f
insert :: Ord a => a -> Scapegoat a -> Scapegoat a
insert x E = N x 1 E E
insert x raiz = (insertaux x raiz)
    where
        insertaux :: Ord a => a -> Scapegoat a -> Scapegoat a
        insertaux x E = (N x 1 E E)
        insertaux x (N y c l r) | x > y = if (fromIntegral (size l) / fromIntegral (c+1)) <= 2/3 && (fromIntegral ((size r)+1) / fromIntegral (c+1)) <= 2/3 
                                                then (N y (c+1) l (insertaux x r)) else rebuild (N y (c+1) l (insertaux x r))
                                | otherwise = if (fromIntegral ((size l)+1) / fromIntegral (c+1)) <= 2/3 && (fromIntegral (size r) / fromIntegral (c+1)) <= 2/3 
                                                then (N y (c+1) (insertaux x l) r) else rebuild (N y (c+1) (insertaux x l) r)