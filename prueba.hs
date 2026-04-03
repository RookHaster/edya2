non :: Bool -> Bool
non b = case b of
   True -> False
   False -> True

ultimo :: [int] -> [int]
ultimo [x]         =  []
ultimo (x:xs)      =  x : ultimo xs
ultimo []          =  error "empty list"

len :: [Int] -> Int
len []        =  0
len (_:l)     =  1 + len l

list123 = 1 : (2 : (3 : ([])))

[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

addToTail _ [] = []
addToTail x (_:ys) = map (+ x) ys

smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

poli :: Int -> Int -> Int
poli x y = (x+y)*2