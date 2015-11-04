data RT t = Rose t [RT t] deriving Show


r1 = Rose 42 []
r2 = Rose 42 [Rose 30 [], Rose 100 [], Rose 300 []]
r3 = Rose 1 [ Rose 2 [], Rose 3 [Rose 5 [], Rose 6 [], Rose 7 []],
              Rose 4 [], Rose 5 [Rose 68 []]]
r4 = Rose 3 [r3, r2]
r7 = Rose 4 [Rose 5 [Rose 6 [Rose 7 [Rose 100 [], Rose 70 [], Rose 5 
                                                             [Rose 80 [Rose 5 []]]]]]]


raiz :: RT t -> t
raiz (Rose n _) = n

hijos :: RT t -> [RT t]
hijos (Rose n rs) = rs



sumarTodo :: Num t => RT t -> t
sumarTodo (Rose n []) = n
sumarTodo (Rose n rs) = n + sumarAux rs

sumarAux :: Num t => [RT t] -> t
sumarAux [] = 0
sumarAux rs = sumarTodo (head rs) + sumarAux (tail rs)



hojas :: RT t -> [t]
hojas (Rose n []) = [n]
hojas (Rose n rs) = hojasAux rs

hojasAux :: [RT t] -> [t]
hojasAux [] = []
hojasAux rs = hojas (head rs) ++ hojasAux (tail rs)



altura :: RT t -> Integer
altura (Rose n []) = 1
altura (Rose n rs) = 1 + alturaAux rs

alturaAux :: [RT t] -> Integer
alturaAux [] = 0
alturaAux rs = max (altura (head rs)) (alturaAux (tail rs)) 
