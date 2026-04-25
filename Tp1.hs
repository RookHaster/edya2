module Tp1 where

import Data.List (sortBy)
import Data.Ord (comparing)
import Distribution.Simple.Setup (emptyHaddockProjectFlags)

data NdTree p = Node (NdTree p) -- subarbol izquierdo
                    p           -- punto
                    (NdTree p)  -- subarbol derecho
                    Int         -- eje
                    | Empty
    deriving(Eq, Ord, Show)

newtype Punto2d = P2d (Double, Double)
            deriving (Eq, Ord, Show)
newtype Punto3d = P3d (Double, Double, Double)

class Punto p where
    dimension :: p -> Int           -- devuelve el numero de coordenadas de un punto
    coord :: Int -> p -> Double     -- devuelve la coordenada k-esima de un punto (comenzando de 0)
    dist :: p -> p -> Double        -- calcula la distancia entre dos puntos

    dist p q = aux p q 0 ((dimension p)-1)
        where
            aux :: Punto p => p -> p -> Int -> Int -> Double
            aux p q i n = if (i == n) then ((coord i p) - (coord i q)) * ((coord i p) - (coord i q)) 
                                      else ((coord i p) - (coord i q)) * ((coord i p) - (coord i q)) + (aux p q (i+1) n)

instance Punto Punto2d where
    dimension p = 2
    coord 0 (P2d(x,_)) = x
    coord 1 (P2d(_,y)) = y 

instance Punto Punto3d where
    dimension p= 3
    coord 0 (P3d(x,_,_)) = x
    coord 1 (P3d(_,y,_)) = y
    coord 2 (P3d(_,_,z)) = z

fromList :: Punto p => [p] -> NdTree p
fromList l = fromListAux l 0
fromListAux:: Punto p => [p] -> Int -> NdTree p
fromListAux [] i = Empty
fromListAux l i = (Node (fromListAux a (mod (i+1) (dimension (l !! 0)))) b (fromListAux c (mod (i+1) (dimension (l !! 0)))) i)
                where 
                    eje = mod i (dimension (l !! 0))
                    l1 = ordenarPor eje l
                    size = length l1
                    b = l1 !! (div size 2)
                    l2menores = take ((div size 2)) l1 
                    l2mayores = drop (1+(div size 2)) l1
                    [a,c] = partir l2menores l2mayores b eje

ordenarPor :: Punto p => Int -> [p] -> [p]
ordenarPor k l = sortBy (comparing (coord k)) l

partir::Punto p => [p] -> [p] -> p -> Int -> [[p]]
partir lmenores []  point eje = [lmenores, []]
partir lmenores (y:ys) point eje = if (coord eje y) == (coord eje point) then partir (lmenores ++ [y]) ys point eje else [lmenores,(y:ys)] 

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar x Empty = (Node Empty x Empty 0)
insertar x tree = insertarAux x tree 0

insertarAux:: Punto p => p -> NdTree p -> Int -> NdTree p
insertarAux x Empty eje = (Node Empty x Empty eje)
insertarAux x (Node l p r axis) eje = if (coord axis p) >= (coord axis x) then (Node (insertarAux x l (mod (eje+1) (dimension x))) p r axis) else
                                                                            (Node  l p (insertarAux x r (mod (eje+1) (dimension x))) axis)

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar point Empty = Empty

eliminar point (Node Empty p Empty eje) = if p == point then Empty else (Node Empty p Empty eje)

eliminar point (Node l p Empty eje) = if p == point then (Node (eliminar b l) b Empty eje) else  (Node (eliminar point l) p  Empty eje)
                                        where b = buscarMasgrande l eje

eliminar point (Node Empty p r eje) = if p == point then (Node Empty b (eliminar b r) eje) else  (Node Empty p (eliminar point r) eje)
                                        where b = buscarMaschico r eje
                                        
eliminar point (Node l p r eje) = if p==point then (Node l b (eliminar b r) eje) else 
                                    if (coord eje point) > (coord eje p) then (Node l p (eliminar point r) eje) else (Node (eliminar point l) p r eje)
                                        where 
                                            b = buscarMaschico r eje

buscarMasgrande::(Eq p,Punto p) => NdTree p -> Int -> p
buscarMasgrande (Node Empty cabeza Empty axis) _ = cabeza
buscarMasgrande (Node l cabeza Empty axis)  eje = if axis == eje then cabeza else coordMax cabeza (buscarMasgrande l  eje) eje --comparar con la cabeza
buscarMasgrande (Node Empty cabeza r axis) eje = if axis ==eje then buscarMasgrande r eje  else coordMax cabeza (buscarMasgrande r  eje) eje--comparar con la cabeza
buscarMasgrande (Node l cabeza r axis )  eje = if axis == eje then buscarMasgrande r eje else 
                                                    if (coord eje max1) > (coord eje max2) then max1 else max2
                                                    where 
                                                        max1 = (buscarMasgrande l  eje)
                                                        max2 = (buscarMasgrande r eje)

buscarMaschico :: (Eq p, Punto p) => NdTree p -> Int -> p
buscarMaschico (Node Empty cabeza Empty axis) _ = cabeza
buscarMaschico (Node Empty cabeza r axis) eje = if eje == axis then cabeza else coordMin cabeza (buscarMaschico r eje) eje
buscarMaschico (Node l cabeza Empty axis) eje = if eje == axis then  buscarMaschico l eje else coordMin cabeza (buscarMaschico l eje) eje
buscarMaschico (Node l cabeza r axis) eje = if axis == eje then buscarMaschico l eje else 
                                                if (coord eje min1) < (coord eje min2) then min1 else min2
                                                where 
                                                    min1 = (buscarMaschico l eje)
                                                    min2 = (buscarMaschico r eje)

coordMax :: Punto p =>p -> p -> Int -> p
coordMax p q axis = if (coord axis p) > (coord axis q) then p else q

coordMin :: Punto p =>p -> p -> Int -> p
coordMin p q axis = if (coord axis p) < (coord axis q) then p else q

type Rect = (Punto2d, Punto2d)

inRegion :: Punto2d -> Rect -> Bool
inRegion p (x,y) = ((coord 0 p) >= (coord 0 x) && (coord 0 p) <= (coord 0 y) && (coord 1 p) <= (coord 1 x) && (coord 1 p) >= (coord 1 y)) -- x arriba e izquierda de y
                    || ((coord 0 p) <= (coord 0 x) && (coord 0 p) >= (coord 0 y) && (coord 1 p) <= (coord 1 x) && (coord 1 p) >= (coord 1 y)) -- x arriba y derecha de y
                    || ((coord 0 p) >= (coord 0 y) && (coord 0 p) <= (coord 0 x) && (coord 1 p) <= (coord 1 y) && (coord 1 p) >= (coord 1 x)) -- x abajo y derecha de y
                    || ((coord 0 p) <= (coord 0 y) && (coord 0 p) >= (coord 0 x) && (coord 1 p) <= (coord 1 y) && (coord 1 p) >= (coord 1 x)) -- x abajo e izquierda de y

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty rect = []
ortogonalSearch (Node l p r  eje) (p1,p2) = if (coord eje (coordMax p1 p2 eje)) < (coord eje p) then ortogonalSearch l (p1,p2) else
                                            if (coord eje (coordMin p1 p2 eje)) > (coord eje p) then ortogonalSearch r (p1,p2) else
                                            if inRegion p (p1,p2) then [p] ++ ortogonalSearch l (p1,p2) ++ ortogonalSearch r (p1,p2) else
                                                ortogonalSearch l (p1,p2) ++ ortogonalSearch r (p1,p2)


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Node (Node (Node Empty (P2d (2.0,3.0)) Empty 2) (P2d (5.0,4.0)) (Node Empty (P2d (4.0,7.0)) Empty 2) 1) (P2d (7.0,2.0)) (Node (Node Empty (P2d (8.0,1.0)) Empty 2) (P2d (9.0,6.0)) Empty 1) 0

p1 = P2d (2,3)
p2 = P2d (5,4)
p3 = P2d (9,6)
p4 = P2d (4,7)
p5 = P2d (8,1)
p6 = P2d (7,2)

p7 = P2d (0,0)
p8 = P2d (5,5)

rec1 :: Rect
rec1 = (p7,p8) 

listatest = [p1,p2,p3,p4,p5,p6]
listatest2 = [p5,p6,p7]

tree = fromList listatest

{-
Node (NdTree p) -- subarbol izquierdo
                    p           -- punto
                    (NdTree p)  -- subarbol derecho
                    Int  
-}









