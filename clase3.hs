factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = n * factorial(n-1)


fib :: Integer -> Integer
fib n | n == 0 = 1
      | n == 1 = 1
      | n > 1 = fib (n - 1) + fib (n - 2)

par :: Integer -> Bool  
par n | n == 2 = True
      | n == 1 = False
      | n > 2 = par (n - 2)

sumaImpares :: Integer -> Integer 
sumaImpares n | n == 1 = 1
              | n > 1 = (2*n -1) + sumaImpares (n-1)


mult3 :: Integer -> Bool
mult3 n | n == 3 = True 
        | n < 3 = False
        | n > 3 = mult3 (n-3)

doblefact :: Integer -> Integer
doblefact n | n == 2 = 2
            | n < 2 = undefined
            | n > 2 = n * doblefact(n - 2)

-- comb :: Integer -> Integer -> Integer 
-- comb n m | n==2 && m == 1 = 2


neg :: Integer -> Integer
neg n | n == 0 = 0
	  | n /= 0 = neg (n-1)


-- Escribir una funcion que dado n ∈ N sume los numeros impares positivos 
--cuyo cuadrado sea menor que n

sumaImpares3 :: Integer -> Integer
sumaImpares3 n | n <= 0 = 0
               | n ==1 = 1
               | par n == False = n + sumaImpares3 (n-2)
               | par n == True = undefined

sI2 :: Integer -> Integer -> Integer
sI2 n x | n > x*x = sI2 (n) (x+2) -- Solo funciona si x == 1 al principio 
        | n <= x*x = sumaImpares3 (x-2)
