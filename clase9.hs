data Arbol = Hoja Integer | Rama Arbol Integer Arbol deriving (Show , Eq)

esHoja :: Arbol -> Bool
esHoja (Hoja a) = True
esHoja (Rama _ a _) = False


sumaNodos :: Arbol -> Integer
sumaNodos (Hoja a) = a
sumaNodos (Rama a1 n a2) = n + sumaNodos a1 + sumaNodos a2

altura :: Arbol -> Integer
altura (Hoja _) = 1
altura (Rama a1 n a2) = 1 + max (altura a1) (altura a2)

--(Rama (Rama (Hoja 3) 1 (Hoja 4)) 11 (Rama (Hoja 5) 2 (Rama (Hoja 7) 6 (Hoja 8))))   
 

data Arbol' t = Hoja' t | Rama' (Arbol' t) t (Arbol' t) deriving (Show , Eq)


esHoja':: Arbol' t -> Bool
esHoja' (Hoja' t ) = True
esHoja' (Rama' _ t _) = False

maximo :: Ord a => Arbol' a -> a
maximo (Hoja' a) = a
maximo (Rama' a1 a a2) = max a ( max (maximo a1) (maximo a2) )