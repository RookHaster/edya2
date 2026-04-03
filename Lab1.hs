module Lab01 where

import Data.List

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
non :: Bool -> Bool
non b = case b of
   True -> False
   False -> True

-- b)
ultimo :: [int] -> [int]
ultimo [x]         =  []
ultimo (x:xs)      =  x : ultimo xs
ultimo []          =  error "empty list"

-- c)
len :: [a] -> Int
len []        =  0
len (_:l)     =  1 + len l

-- d)
list123 = 1 : (2 : (3 : ([])))

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail _ [] = []
addToTail x (_:ys) = map (+ x) ys

-- g) este no se que onda
-- listmin xs = head . sort xs

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs


--2. Definir las siguientes funciones y determinar su tipo:

--a) five, que dado cualquier valor, devuelve 5
five :: a -> Int
five _ = 5

--b) apply, que toma una función y un valor, y devuelve el resultado de aplicar la función al valor dado
apply :: (a -> b) -> a -> b
apply f x = f x

--c) ident, la función identidad
ident :: a -> a
ident x = x

--d) first, que toma un par ordenado, y devuelve su primera componente
first :: [a] -> a
first (x:xs) = x

--e) derive, que aproxima la derivada de una función dada en un punto dado

--f) sign, la función signo
sign :: Int -> Int
sign x
  | x<0 = -1
  | x == 0 = 0
  | otherwise = 1

--g) vabs, la función valor absoluto (usando sign y sin usarla)
vabss :: Int -> Int
vabss x = x * (sign x)

vabs :: Int -> Int
vabs x = if x<0 then x*(-1) else x


--h) pot, que toma un entero y un número, y devuelve el resultado de elevar el segundo a la potencia dada por el primero
pot :: Num a => a -> Int -> a
pot x 1 = x
pot x y = x * pot x (y-1)

--i) xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool
xor x y = if (x || y) && not (x&&y) then True else False

--j) max3, que toma tres números enteros y devuelve el máximo entre ellos
max3 :: [Int] -> Int
max3 [x,y,z]
  | x>y && x>z = x
  | y>x && y>z = y
  | otherwise = z

--k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

{- 
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}
bisiesto :: Int -> Bool
bisiesto x = ((mod x 4) == 0 && (mod x 100) /= 0) || ((mod x 400) == 0)

{-
4)

Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:

-}
(*$) :: [Int] -> Int -> [Int]
[] *$ _ = []
(y:ys) *$ x = (y*x:ys *$ x)


v = [1, 2, 3] *$ 2 *$ 4


--5) Definir las siguientes funciones usando listas por comprensión:

--a) 'divisors', que dado un entero positivo 'x' devuelve la lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], mod n x == 0]

--b) 'matches', que dados un entero 'x' y una lista de enteros descarta de la lista los elementos distintos a 'x'
matches :: Int -> [Int] -> [Int]
matches x ys = [y | y <- ys, y == x]

--c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2, donde 0 <= a, b, c, d <= 'n'
cuadrupla :: Int -> [(Int,Int,Int,Int)]
cuadrupla n = [(x,y,z,w) | x <- [0..n], y <- [0..n], z <- [0..n], w <- [0..n], x*x + y*y == z*z + w*w]


--d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista 'xs' sin elementos repetidos

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

{- 
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}
scalarProduct :: [Int] -> [Int] -> [Int]
scalarProduct [] [] = []
scalarProduct (x:xs) (y:ys) = (x*y : scalarProduct xs ys)


--7) Definir mediante recursión explícita las siguientes funciones y escribir su tipo más general:

--a) 'suma', que suma todos los elementos de una lista de números
suma :: [Int] -> Int
suma [] = 0
suma [x] = x
suma (y:ys) = y + suma ys

--b) 'alguno', que devuelve True si algún elemento de una lista de valores booleanos es True, y False en caso contrario
alguno :: [Bool] -> Bool
alguno [] = False
alguno [x] = x
alguno (x:xs) = x || alguno xs

--c) 'todos', que devuelve True si todos los elementos de una lista de valores booleanos son True, y False en caso contrario
todos :: [Bool] -> Bool
todos [] = True
todos [x] = x
todos (x:xs) = x && todos xs

--d) 'codes', que dada una lista de caracteres, devuelve la lista de sus ordinales -------------------------------------------------------------------------------

--e) 'restos', que calcula la lista de los restos de la división de los elementos de una lista de números dada por otro número dado
restos :: [Int] -> Int -> [Int]
restos [] _ = []
restos (x:xs) y = (rem x y : restos xs y)

--f) 'cuadrados', que dada una lista de números, devuelva la lista de sus cuadrados
cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = (x*x : cuadrados xs)

--g) 'longitudes', que dada una lista de listas, devuelve la lista de sus longitudes
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = ((len x) : longitudes xs)

--h) 'orden', que dada una lista de pares de números, devuelve la lista de aquellos pares en los que la primera componente es menor que el triple de la segunda
orden :: [(Int, Int)] -> [(Int, Int)]
orden [] = []
orden ((x,y):xs) = if x<(3*y) then ((x,y): orden xs) else orden xs

--i) 'pares', que dada una lista de enteros, devuelve la lista de los elementos pares
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = if (rem x 2) == 0 then (x: pares xs) else pares xs

--j) 'letras', que dada una lista de caracteres, devuelve la lista de aquellos que son letras (minúsculas o mayúsculas) ----------------------------------------

--k) 'masDe', que dada una lista de listas 'xss' y un número 'n', devuelve la lista de aquellas listas de 'xss' con longitud mayor que 'n' 
masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (x:xs) n = if (len x) > n then (x : masDe xs n) else (masDe xs n)