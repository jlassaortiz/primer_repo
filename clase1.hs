doble x = 2 * x
suma x y = x + y
normaVectorial v1 v2 = sqrt((v1)**2 + (v2)**2)
funcionConstante8 x = 8
respuestaATodo = 42
signo x | x > 0 = 1
        | x == 0 = 0
        | x < 0 = (-1) 

abso x | x >= 0 = x
       | x < 0 = (-x)

abso2 x = x * signo x -- reusar codigo es mejor|


maximo a b | a > b = a
           | otherwise = b

maximo3 a b c = maximo (maximo a b) c

yLogico x y |x == True && y == True = True
            |otherwise = False              

--lo mejor es usar " x && y " ya funciona como yLogico por defecto|

--TAREA:|

funcion1 n1 n2 n3 | n2 < 10 = n1
                  | n2 >= 10 = n1 + n3



-- ¿Como construyo tabla de verdad para nand?


-- forma predeterminada por Haskell para nand " not ( x && y) " |
nand True True = False
nand True False = True
nand False True = True
nand False False = True

-- ¿Como construyo tabla de verdad para nor? |
 

-- forma predeterminada por Haskell para nor " not ( x || y) " |

rPol2p a b c = ( (-b) + sqrt(b*b -4 *a*c) ) / (2 * a)
rPol2n a b c = ( (-b) - sqrt(b*b -4 *a*c) ) / (2 * a)

esPitagorica a b c | c*c == a*a + b*b = True
                   | otherwise = False












