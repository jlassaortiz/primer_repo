pertenece :: Integer -> [Integer] -> Bool
pertenece x l | length l == 0 	= False
              | x == (head l) 	= True 
              | otherwise 		= pertenece x (tail l) 


hayRepetidos :: [Integer] -> Bool
hayRepetidos l | length l == 0 					= False
			   | pertenece (head l) (tail l) 	= True
			   | otherwise 						= hayRepetidos (tail l)


--------- 0 ----------

--propia:

menores :: Integer -> [Integer] -> [Integer]
menores x l = aux x l []

aux :: Integer -> [Integer] -> [Integer] -> [Integer]
aux x l r | length l == 0 	= r ++ []
          | x > (head l) 	= aux x (tail l) (r ++ [head l])
          | otherwise 		= aux x (tail l) r


--------- 0 -----------


menores2 :: Integer -> [Integer] -> [Integer]
menores2 x [] = []
menores2 x l  | (head l) < x 	= [head l] ++ menores2 x (tail l)
              | otherwise 		= menores2 x (tail l)


quitar :: Integer -> [Integer] -> [Integer]
quitar x [] = []
quitar x l  | x == head l  = tail l
            | otherwise    = [head l] ++ quitar x (tail l)

maximo :: [Integer] -> Integer
maximo [] = undefined 
maximo l  | length l == 1 			     = head l
          | head l > head (tail l) 	 = maximo ( (head l) : tail (tail l))
          | otherwise 				       = maximo (tail l)



-- CAMBIO DE BASE --

enBase :: Integer -> Integer -> [Integer]
enBase x b | x < b 	  	= [x]
           | otherwise 	= enBase (div x b) b ++ [mod x b]

deBase :: Integer -> [Integer] -> Integer
deBase b n | length n == 0      = 0
           | length n == 1 	    = head n
           | otherwise 	        = (head n)*b^((length n) - 1 ) + deBase b (tail n) 


  --- CAPICUA ---

  capicuaPara :: [Integer] -> [Integer]
  