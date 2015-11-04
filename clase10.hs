data Polinomio = Mono Float Integer | Suma Polinomio Polinomio | Producto Polinomio Polinomio

evaluar :: Polinomio -> Float -> Float
evaluar (Mono a n) z = (a*z)^n
evaluar (Suma p q) z = (evaluar p z) + (evaluar q z)
evaluar (Producto p q) z = (evaluar p z) * (evaluar q z)



---CALCULAMOS CONEFICIENTES DE POLINOMIOS

coeficientes :: Polinomio -> [Float]
coeficientes (Mono a 0) = [a]
coeficientes (Mono a n) = 0 : coeficientes (Mono a (n-1)) 
coeficientes (Suma p q) = sumaListas (coeficientes p) (coeficientes q)
coeficientes (Producto p q) = prodListas (coeficientes p) (coeficientes q)


sumaListas :: [Float] -> [Float] -> [Float]
sumaListas [] [] = []
sumaListas a [] = a
sumaListas [] b = b
sumaListas (a:as) (b:bs) = [a+b] ++ sumaListas as bs

prodListas :: [Float] -> [Float] -> [Float]
prodListas [] l = []
prodListas l [] = []
prodListas [x] (l:ls) = (x*l) : prodListas [x] ls 
prodListas (x:xs) l =  sumaListas (prodListas [x] l) ( 0: prodListas xs l )

---DERIVEMOS ( reveeeer)

deriv :: Polinomio -> Polinomio
deriv (Mono a n) = Mono (a * (fromInteger n)) (n-1)
deriv (Suma p q) = Suma (deriv p) (deriv q)
deriv (Producto p q) = Suma (Producto (deriv p) q) (Producto p (deriv q))


--Para agregar los Polinomios a la clase Num

instance Num Polinomio where
	(+) p q = Suma p q
	(*) p q = Producto p q
	negate p = Producto (Mono (-1) 0 ) p
	fromInteger n = Mono (fromInteger n) 0
	abs p = undefined
	signum p = undefined

instance Show Polinomio where
	show p = mostrarLista (coeficientes p) 0


mostrarLista :: [Float] -> Integer -> String
mostrarLista [] 0 = "0"
mostrarLista [a] n = show a ++ " x^" ++ show n
mostrarLista (a:as) n | a == 0 = mostrarLista as (n+1) 
mostrarLista (a:as) 0 = show a ++ " + " ++ mostrarLista as 1
mostrarLista (a:as) 1 = show a ++ " x" ++ " + " ++ mostrarLista as 2
mostrarLista (a:as) n = show a ++ " x^" ++ show n ++  " + " ++ mostrarLista as (n+1)
 


p1 = Mono 5 1
p2 = Mono 3 2
p3 = Mono 7 3
p4 = Suma p1 p2
p5 = Suma p3 p4
p6 = Producto p1 p2
p7 = Producto p4 p5
p8 = Producto p6 p4


----