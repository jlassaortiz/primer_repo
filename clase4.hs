potencia :: Float -> Integer -> Float
potencia a n | n == 0 = 1
             | otherwise = a * potencia a (n-1)

--  {-    | n == 1 = a    NO ES NECESARIO! ESTA DEF COMO a * potencia (a 0) -}


division :: Integer -> Integer -> (Integer,Integer)
division a d | a < d = (0,a)
             | a == d = (1,0)
             | a > d =  ( 1 + fst(division (a-d) d) , snd(division (a-d) d) )

division' :: Integer -> Integer -> (Integer,Integer)
division' a d | a < d = (0,a)
              | otherwise = ( 1 + fst(division' (a-d) d) , snd(division' (a-d) d) )

division'' :: Integer -> Integer -> (Integer,Integer)
division'' a d | a < d = (0,a)
               | otherwise = ( 1 + fst qr , snd qr )
               where qr = division (a-d) d  -- mucho mas rápido! 


prod :: [Integer] -> Integer
prod lista | length lista == 0 = 1
           | otherwise = head lista * prod(tail lista)


rev ::  [Integer] -> [Integer]
rev lista | length lista == 0 = lista
          | otherwise = rev (tail lista) ++ [head lista]

sumaL :: [Integer] -> [Integer] -> [Integer]
sumaL l1 l2 | not(length l1 == length l2) = undefined
            | length l1 == 0 = []
            | otherwise = [head l1 + head l2] ++ sumaL (tail l1) (tail l2)

prodInterno:: [Float] -> [Float] -> Float
prodInterno l1 l2 | not(length l1 == length l2) = undefined
                  | length l1 == 0 = 0
                  | otherwise = head l1 * head l2 + prodInterno (tail l1) (tail l2)


