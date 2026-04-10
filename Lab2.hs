
module Lab02 where

{-
   Laboratorio 2
   EDyAII 2022
-}

import Data.List
import GHC.Internal.Base (VecElem(Int16ElemRep))
import Distribution.Simple.Command (OptDescr(BoolOpt))

-- 1) Dada la siguiente definición para representar árboles binarios:

data BTree a = E | Leaf a | Node (BTree a) (BTree a)

-- Definir las siguientes funciones:

-- a) altura, devuelve la altura de un árbol binario.

altura :: BTree a -> Int
altura E = 0
altura (Leaf a) = 0
altura (Node l r) = max (altura r) (altura l) + 1

-- b) perfecto, determina si un árbol binario es perfecto (un árbol binario es perfecto si cada nodo tiene 0 o 2 hijos
-- y todas las hojas están a la misma distancia desde la raı́z).

perfecto :: BTree a -> Bool
perfecto E = False
perfecto (Leaf a) = True
perfecto (Node l r) = (perfecto l == perfecto r) && (altura l == altura r)

-- c) inorder, dado un árbol binario, construye una lista con el recorrido inorder del mismo.

inorder :: BTree a -> [a]
inorder E = []
inorder (Leaf a) = [a]
inorder (Node l r) = inorder l ++ inorder r


-- 2) Dada las siguientes representaciones de árboles generales y de árboles binarios (con información en los nodos):

data GTree a = EG | NodeG a [GTree a]

garbol :: GTree Char
garbol = 
  NodeG 'A' 
    [ NodeG 'B' 
        [ NodeG 'F' 
            [ NodeG 'K' [], NodeG 'L' []]
        , NodeG 'G' []
        , NodeG 'H' []
        ]
    , NodeG 'C' []
    , NodeG 'D' []
    , NodeG 'E' 
        [ NodeG 'I' 
            [ NodeG 'M' []]
        , NodeG 'J' []
        ]
    ]

data BinTree a = EB | NodeB (BinTree a) a (BinTree a)

inorderB :: BinTree a -> [a]
inorderB EB = []
inorderB (NodeB l x r) = inorderB l ++ [x] ++ inorderB r

{- Definir una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
   la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
   el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
   de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).
   
   
   Por ejemplo, sea t: 
       
                    A 
                 / | | \
                B  C D  E
               /|\     / \
              F G H   I   J
             /\       |
            K  L      M    
   
   g2bt t =
         
                  A
                 / 
                B 
               / \
              F   C 
             / \   \
            K   G   D
             \   \   \
              L   H   E
                     /
                    I
                   / \
                  M   J  
-}

g2bt :: GTree a -> BinTree a
g2bt EG = EB
g2bt (NodeG a hijos) = NodeB (bros hijos) a EB
    where 
        bros :: [GTree a] -> BinTree a
        bros [] = EB -- depende de como definas la lista de hijos
        bros [EG] = EB -- depende de como definas la lista de hijos
        bros ((NodeG a x):xs) = NodeB (bros x) a (bros xs)

-- 3) Utilizando el tipo de árboles binarios definido en el ejercicio anterior, definir las siguientes funciones: 
{-
   a) dcn, que dado un árbol devuelva la lista de los elementos que se encuentran en el nivel más profundo 
      que contenga la máxima cantidad de elementos posibles. Por ejemplo, sea t:
            1
          /   \
         2     3
          \   / \
           4 5   6
                             
      dcn t = [2, 3], ya que en el primer nivel hay un elemento, en el segundo 2 siendo este número la máxima
      cantidad de elementos posibles para este nivel y en el nivel tercer hay 3 elementos siendo la cantidad máxima 4.
   -}

arbol :: BinTree Int
arbol = NodeB 
                  (NodeB EB 2 (NodeB EB 4 EB)) 
                  1 
                  (NodeB (NodeB EB 5 EB) 3 (NodeB EB 6 EB))

pow :: Int -> Int -> Int
pow _ 0 = 1
pow n m = n * (pow n (m-1))                  

bfs :: BinTree a -> [[a]]
bfs EB = []
bfs raiz = aux [raiz]
    where 
        aux :: [BinTree a] -> [[a]]
        aux [] = []
        aux cola = let vals = [x | (NodeB _ x _) <- cola]
                       hijos = concat[[l,r] | NodeB l _ r <- cola]
                       in vals : aux (filter noteb hijos)
        noteb :: BinTree a -> Bool
        noteb EB = False
        noteb _ = True


dcn :: BinTree a -> [a]
dcn EB = []
dcn raiz = let lista = (bfs raiz) in lista !! (aux lista 0)
    where 
        aux :: [[a]] -> Int -> Int
        aux [] n = n-1
        aux (x:xs) n = if (length x == (pow 2 n)) then (aux xs (n+1)) else n-1  --AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

{- b) maxn, que dado un árbol devuelva la profundidad del nivel completo
      más profundo. Por ejemplo, maxn t = 2   -}

maxn :: BinTree a -> Int
maxn EB = 0
maxn (NodeB l _ r) = min (1+ maxn l) (1+ maxn r)

{- c) podar, que elimine todas las ramas necesarias para transformar
      el árbol en un árbol completo con la máxima altura posible. 
      Por ejemplo,
         podar t = NodeB (NodeB EB 2 EB) 1 (NodeB EB 3 EB)
-}

podar :: BinTree a -> BinTree a
podar EB = EB
podar raiz = aux raiz (maxn raiz)
    where 
        aux :: BinTree a -> Int -> BinTree a
        aux _ 0 = EB
        aux (NodeB l x r) n = (NodeB (aux l (n-1)) x (aux r (n-1)))