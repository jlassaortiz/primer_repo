iniciales :: String -> [ Char ] -> [ Char ]
iniciales nombre apellido = [n,'.', a,'.']
                             where (n: _) = nombre 
                                   (a: _) = apellido
longitud :: [a] -> Integer
longitud [] = 0
longitud (x:[]) = 1
longitud (_:xs) = 1 + longitud xs


--Ejercicios parcial hechos con Pattern Matching

esMul :: Integer -> Integer -> Bool
esMul a 0 = True
esMul 0 n = False
esMul a n | a < n = False
          | a > n = esMul (a-n) n
          | a == n = True


intercalar :: [a] -> [a] -> [a]
intercalar [] [] = []
intercalar l1 [] = l1
intercalar [] l2 = l2
intercalar (a:as) (b:bs) = a : b : intercalar as bs

quitar :: Integer -> [Integer] -> [Integer]
quitar a [] = []
quitar a (x:xs) | a == x = quitar a xs
                | otherwise = x : quitar a xs


comprimir :: [Integer] -> [(Integer, Integer)]
comprimir xs = comprimir' (poneUnos xs)


poneUnos :: [Integer] -> [(Integer, Integer)]
poneUnos [] = []
poneUnos (a:as) = (a,1) : poneUnos as

comprimir' :: [(Integer, Integer)] -> [(Integer, Integer)]
comprimir' [] = []
comprimir' [a] = [a]
comprimir' ((p,ps):(s,ss):xs) | p == s = comprimir' ((p, ps + ss) : xs)
                              | otherwise =  (p,ps) : comprimir' ((s, ss) : xs)


descomprimir :: [(Integer, Integer)] -> [Integer]
descomprimir [] = []
descomprimir ((a,1): xs) = a : descomprimir xs 
descomprimir ((a,n): xs) = a : descomprimir ((a,n-1):xs)




data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Eq

esFinde :: Dia -> Bool
esFinde d | d == Sabado || d == Domingo = True
          | otherwise = False

diaHabil :: Dia -> Bool
diaHabil Sabado = False
diaHabil Domingo = False
diaHabil x = True

tuplar :: [a] -> [a] -> [(a,a)]
tuplar [] [] = []
tuplar (x:xs) [] = []
tuplar [] (y:ys) = []
tuplar (x:xs) (y:ys) = (x,y) : tuplar xs ys

data Racional = (Integer , Integer)

potencia :: Racional -> Integer -> Racional
potencia a n | n == 1 = a
