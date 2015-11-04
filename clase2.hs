crearPar :: a -> b -> (a,b)
crearPar x y = (x ,y)

invertir :: (a,b) -> (b,a)
invertir (x,y) = (y,x)

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia (x,y) (z,w) = sqrt((x-z)^2 + (y-w)^2)

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = crearPar (((-b) + sqrt(b^2 -4*a*c)) / (2*a)) (((-b) - sqrt(b^2 -4*a*c)) / (2*a))  

listar :: a -> a -> a -> [a]
listar x1 x2 x3 = [x1,x2,x3]

maximo :: Integer -> Integer -> Integer
maximo x y | x > y = x
           |otherwise = y

minimo :: Integer -> Integer -> Integer
minimo x y | x < y = x
           |otherwise = y


rangoDePaso :: Integer->Integer-> Integer->[Integer]
rangoDePaso x1 x2 x3 = [(minimo x1 (minimo x2 x3))..(maximo x1 (maximo x2 x3))]

pendiente :: (Float,Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) = (y1-y2)/(x1-x2)
