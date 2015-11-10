-- DATOS Y SHOW
type Pixel = (Integer, Integer, Integer)
type PixelDelta = (Integer, Integer, Integer)
type Frame = [[Pixel]]

data Video = Iniciar Frame | Agregar Frame Video deriving Eq
instance Show Video
   where show (Iniciar f) = mostrarFrame f
         show (Agregar f v) = (mostrarFrame f) ++ "\n" ++ (show v)

type FrameComprimido = [(Integer, Integer, PixelDelta)]
data VideoComprimido = IniciarComp Frame | AgregarNormal Frame VideoComprimido | AgregarComprimido FrameComprimido VideoComprimido
instance Show VideoComprimido
   where show (IniciarComp f) = "INICIAL \n" ++ mostrarFrame f
         show (AgregarNormal f v) = "NO COMPRIMIDO \n" ++ (mostrarFrame f) ++ "\n" ++ (show v)
         show (AgregarComprimido f v) = "COMPRIMIDO \n" ++ (mostrarFrameComprimido f) ++ "\n" ++ (show v)

mostrarFrame :: Frame -> String
mostrarFrame [] = ""
mostrarFrame (x:xs) = (show x) ++ "\n" ++ (mostrarFrame xs)




mostrarFrameComprimido :: FrameComprimido -> String
mostrarFrameComprimido [] = ""
mostrarFrameComprimido (x:xs) = "\t" ++ (show x) ++ "\n" ++ (mostrarFrameComprimido xs)




-- Ejercicio 1/5
ultimoFrame :: Video -> Frame
ultimoFrame (Iniciar f) = f
ultimoFrame (Agregar f vs) = f





-- Ejercicio 2/5
norma :: (Integer, Integer, Integer) -> Float
norma (x1, x2, x3) = sqrt ( (fromInteger x1)^2 + (fromInteger x2)^2 +(fromInteger x3)^2 )





-- Ejercicio 3/5
pixelsDiferentesEnFrame :: Frame -> Frame -> Float -> FrameComprimido
pixelsDiferentesEnFrame fr1 fr2 u = col fr1 fr2 u 0 0


dist :: [Pixel] -> [Pixel] -> Float -> Integer -> Integer -> [(Integer, Integer, PixelDelta)]
dist [] pxs u f c = []
dist (p:px) (q:qx) u f c| norma (resta p q) > u = (f, c, resta p q) : dist px qx u f (c+1)
                        | norma (resta p q) <= u = dist px qx u f (c+1)

 -- La función dist toma una fila de cada frame y compara pixel a pixel. Si la diferencia
 -- entre pixeles es significativa (la norma de la resta de los pixeles es mayor a cierto
 -- umbral u) devuelve una tupla de tres elementos (fila, columna, (diferencia entre los pixeles))
 -- Luego llama a la recurción pero como cambiamos de pixel entonces
 -- el parametro c cambia de c -> c+1
 -- Si la diferencia entre pixeles no es significativa, solamente llama a la recurción
 -- también cambiando el parametro c -> c+1


col :: Frame -> Frame -> Float -> Integer -> Integer -> [(Integer, Integer, PixelDelta)]
col [] mss u f c = []
col (l:ls) (m:ms) u f c = (dist l m u f c) ++ (col ls ms u (f+1) c)

-- La función col toma dos frames y les aplica la función dist entre las primeras filas de
-- cada frame y luego llama a la recurición (cambiando el parametro f -> f+1
-- en cada iteracion) recorriendo de esta forma cada frame en su totalidad


-- *Main> pixelsDiferentesEnFrame v1f1 v2f2 1
-- [(0,0,(3,3,3)),(0,1,(3,3,3)),(1,0,(3,3,3)),(1,2,(-3,-3,-3)),(2,1,(-3,-3,-3)),(2,2,(-3,-3,-3))]





-- Ejercicio 4/5
comprimir :: Video -> Float -> Integer -> VideoComprimido
comprimir (Iniciar fx) u n                 = (IniciarComp fx)
comprimir (Agregar fs (Iniciar fx)) u n    | longitud (pixelsDiferentesEnFrame fs fx u) <= n = AgregarComprimido (pixelsDiferentesEnFrame fs fx u)
                                                                                                                 ( IniciarComp fx )
                                           | longitud (pixelsDiferentesEnFrame fs fx u) > n = AgregarNormal fs ( IniciarComp fx )

comprimir (Agregar fs (Agregar fx vs)) u n | longitud (pixelsDiferentesEnFrame fs fx u) <= n = AgregarComprimido (pixelsDiferentesEnFrame fs fx u)
                                                                                                              ( AgregarNormal fx (comprimir vs u n))
                                           | longitud (pixelsDiferentesEnFrame fs fx u) > n = AgregarNormal fs  (comprimir (Agregar fx vs) u n)



longitud :: FrameComprimido -> Integer
longitud [] = 0
longitud (x:[]) = 1
longitud (_:xs) = 1 + longitud xs



-- Ejercicio 5/5
descomprimir :: VideoComprimido -> Video
descomprimir (AgregarComprimido fc (IniciarComp fx))       = Agregar (aplicarCambio fx fc) (Iniciar fx)
descomprimir (IniciarComp fx)                              = Iniciar fx
descomprimir (AgregarNormal fs vc)                         = Agregar fs (descomprimir vc)
descomprimir (AgregarComprimido fc (AgregarNormal fx vc))  = Agregar (aplicarCambio fx fc) (Agregar fx (descomprimir vc))




-- Funciones provistas por la cátedra
sumarCambios :: FrameComprimido -> FrameComprimido -> FrameComprimido
sumarCambios fc1 fc2 = [(i, j, sumar deltas (busqueda i j fc2)) | (i, j, deltas) <- fc1] ++
                       [(i, j, deltas) | (i, j, deltas) <- fc2, busqueda i j fc1 == (0,0,0)]
-- *Main> sumarCambios [(1,1,(2,2,2)),(2,2,(0,0,-1))] [(1,1,(-3,-3,-3)), (1,2,(1,1,1))]
-- [(1,1,(-1,-1,-1)),(2,2,(0,0,-1)),(1,2,(1,1,1))]

aplicarCambio :: Frame -> FrameComprimido -> Frame
aplicarCambio f fc = [ [nuevoVal f i j fc| j <- [0..length (f !! i) - 1]] | i <- [0..length f - 1]]
  where nuevoVal f i j fc = sumar ((f !! i) !! j) (busqueda (fromIntegral i) (fromIntegral j) fc)
--  *Main> aplicarCambio [[(1,1,1),(2,2,2)],[(3,3,3),(4,4,4)]] [(0, 1, (1,2,3))]
--  [[(1,1,1),(3,4,5)],[(3,3,3),(4,4,4)]]

busqueda :: Integer -> Integer -> FrameComprimido -> PixelDelta
busqueda i j [] = (0, 0, 0)
busqueda i j ((x, y, c) : cs) | x == i && j == y = c
                            | otherwise = busqueda i j cs

sumar :: PixelDelta -> PixelDelta -> PixelDelta
sumar (x,y,z) (x2,y2,z2) =  (x+x2,y+y2,z+z2)

resta :: PixelDelta -> PixelDelta -> PixelDelta
resta (x,y,z) (x2,y2,z2) = (x-x2,y-y2,z-z2)

-- PRUEBAS
p3 :: Pixel
p3 = (3,3,3)

p0 :: Pixel
p0 = (0,0,0)

p1 :: Pixel
p1 = (1,1,1)

p2 :: Pixel
p2 = (2,2,2)

p4 :: Pixel
p4 = (1,2,3)

-- Video 0:
f0 = [[p0, p0, p0], [p3, p3, p3]]
f1 = [[p3, p3, p3], [p3, p3, p3]]
video0 = Agregar f1 (Agregar f0 (Iniciar f0))

-- Video 1:  En la versión comprimida, todos los frames son comprimidos (salvo el inicial)

v1f1 :: Frame
v1f1 = [[p3, p3, p0, p0, p0],
	  [p3, p3, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v1f2 :: Frame
v1f2 = [[p0, p0, p0, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v1f3 :: Frame
v1f3 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p3, p3, p0],
	  [p0, p0, p3, p3, p0],
	  [p0, p0, p0, p0, p0]]

v1f4 :: Frame
v1f4 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p3, p3],
	  [p0, p0, p0, p3, p3]]


v1 :: Video
v1 = Agregar v1f4 (Agregar v1f3 (Agregar v1f2 (Iniciar v1f1)))

v1Comp :: VideoComprimido
v1Comp = comprimir v1 1 6


-- Video 2:  En la versión comprimida, sólo los frames 2 y 4 son comprimidos

v2f1 :: Frame
v2f1 = [[p3, p3, p0, p0, p0],
	  [p3, p3, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v2f2 :: Frame
v2f2 = [[p0, p0, p0, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v2f3 :: Frame
v2f3 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p3, p3, p3],
	  [p0, p0, p3, p3, p0],
	  [p0, p0, p0, p0, p0]]

v2f4 :: Frame
v2f4 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p3],
	  [p0, p0, p0, p3, p3],
	  [p0, p0, p0, p3, p3]]


v2 :: Video
v2 = Agregar v2f4 (Agregar v2f3 (Agregar v2f2 (Iniciar v2f1)))

v2Comp :: VideoComprimido
v2Comp = comprimir v2 1 6

v3 :: Video
v3 = Iniciar f1
