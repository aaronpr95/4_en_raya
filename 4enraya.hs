{-
  PRÁCTICA HASKELL. 4 en raya

  (c) - Aarón Portales - 2016

-}


import System.Environment
import Data.Bits
import Data.List 
import System.IO

-- NOTAS:
-- N = Columnas   y   M = Filas

-- declaracion de la funcuion atoi
atoi::String->Int
atoi=read


-- declara un tablero inicial con todo a 0
-- con las dimensiones n=columnas y m=filas ----------------------------------------------
tablero n m l t = if length t < m then tablero n m l (inser_fila n l t) else t

inser_fila n l t = t++[columnas n l]

columnas n l = if length l < n then columnas n (inser_cero l) else l


inser_cero [] = [0]
inser_cero [x] = [x,0]
inser_cero (x:xs) = (x:xs)++[0]
-- FIN INICIAR TABLERO ------------------------------------------------------------------



-- funcion que muestra cada elemento de una lista
-- en una linea distinta -------------------------------------------------------------
pintar [x] = show x
pintar (x:xs) = show x ++"\n"++ pintar xs

-- en t pasamos [1]
lista_colum t n = if last t == (n)
                     then t
                     else t++(lista_colum [q+1] n)
  where q = primero t



-- FUNCION DE INSERTAR FICHA SEGUN LA COLUMNA QUE ELIJA EL JUGADOR ---------------------
-- le pasamos la columna(c) y el tablero(t) y el jugador (1 o 2)
--insertar x t c m =
--insertar x t c m = inser x c (t!!f) 
insertar x t c m = inserlist (inser x c (t!!f)) f t
  where f = pos_columna t m c --es la fila en la que queda la ficha
        
-- insertar x t c m = inserlist (inser x c (t!!f)) f t
  --where f = pos_columna t n c --es la fila en la que queda la ficha
    --    n = m-1


-- nos devuelve en que fila de la columna cae la ficha
-- le pasamos tablero t m(numero de la ultima fila, sabiendo que la primera es 0)
-- y numero de columna (c)
-- devuelve el numero de la primera fila vacia de la columna c:
-- 	columna 0
-- 	columna 1
-- 	columna 2 etc
pos_columna t m c = if t!!m!!c /= 0 then pos_columna t (m-1) c else m


-- inserta x en la posicion n de la lista l
--ins_lista x n l =
--inser x 0 l = x:l
inser x n [] = [x]
inser x n (y:ys) = q++[x]++i
  where q = take n (y:ys)
        i = resto n (y:ys)

-- insertar lista i en lista de listas l en la pos c(que es columna) 
--inserlist i 0 (y:ys) = i++g
inserlist i c [[]] = [i]
inserlist i c (y:ys) = q++[i]++h
  where q = take c (y:ys)
        h = resto c (y:ys)
        
-- resto nos devuelve la lista quitando los n primeros elementos
resto n [] = []
resto 0 (x:xs) = xs
resto n (x:xs) = if n/=0 then resto (n-1) (tail (x:xs)) else xs

-- funcion que comprieba si es posible insertar una ficha donde le dice un jugador
--es_posible t m n x 
-- se le pasa el tablero , fila, columna y x=columna donde desea introducir el jugador la ficha
es_posible t m n x = if limit n x == True
                        then llena t m x
                        else False 


-- comprobar que la x se sale de los limites
limit n x = if x>n then False else if x<0 then False else True

-- comprobar que la columna ya esté llena
llena t m x = if t!!0!!x == 0 then True else False



-- FIN FUNCION INSERTAR -------------------------------------------------------------------



-- FUNCION COMPROBAR LINEA -------------------------------------------------------------


-- COMPROBACION HORIZONTAL --
-- comprueba todas las listas horizontales
-- le pasamos el tablero y las filas que hay
comph t m = if m<0 then False else comph1 t m
comph1 t m = if horizontal q == True then True else comph t (m-1)
  where q = t!!m


-- comprueba que hay linea en una lista horizontal
horizontal l = if igual12 q 
                  then if length q >= 4 then True else False
                  else False
  where q = mayoritario l

-- para comprobar que la lista es de 1 o 2 no de "0" que sería que no hay ficha
igual12 l = if (primero l) == 1 || (primero l) == 2 then True else False

-- devuelve la lista seguida mas larga
-- si la lista más larga es mayor que 4 la devuelve
mayoritario [] = []
mayoritario [x] = [x]
mayoritario (x:xs) = if length j >= length q then j else q
  where j = lista_seguida (x:xs)
        q = mayoritario (resto_seguida (x:xs))
        

-- devuelve la lista de los primeros x seguidos
lista_seguida [] = []
lista_seguida [x] = [x]
lista_seguida (x:xs) = if x==q then [x]++(lista_seguida xs) else [x]
  where q = primero xs

-- devuelve la lista quitando los primeros x numeros iguales
resto_seguida [] = []
resto_seguida [x] = []
resto_seguida (x:xs) = if x==q then resto_seguida xs else xs 
  where q = primero xs

-- COMPROBACION VERTICAL --
{-
  Vamos a hacer una lista de cada columna y despues comprobar como el proceso de 
  la comprobacion horizontal
-}
-- pasamos el tablero y el numero de filas(m -> el número de la ultima fila)
-- n numero de la columna empezando desde cero
-- comlumnalista t n 0 = [t!!0!!n]
-- devuelve una lista de la columna N
columnalista t m n = if m>0 then [t!!m!!n]++columnalista t (m-1) n else [t!!0!!n]

-- FUNCION PARA COMPROBAR TODAS las lineas en vertical
compv t m n = if n>0 
                then if compv1 t m n == False then compv t m (n-1) else True
                else compv1 t m 0

-- devuelve verdadero si hay linea en la UNA sola columna N debiendo pasar
-- las M filas que hay (poner una menos porque empieza por 0)
compv1 t m n = vertical q
  where q = columnalista t m n 

-- comprueba que hay linea en la lista
vertical l = if igual12 q 
                  then if length q >= 4 then True else False
                  else False
  where q = mayoritario l


-- comprobación DIAGONAL
{-
  Crear una lista de una diagonal y comprobar como en casos anteriores
-}
{-Diagonal_1
*
  *
    * 
      *   -}
-- creacion de la lista de una diagonal como la que se muestra arriba
-- tabler(t) filas(m) columnas(n) en t []
list_diagonal1 t m n j = if length j < 4 
                            then list_diagonal1 t (m-1) (n-1) (j++[t!!m!!n]) 
                            else j
-- probar: list_diagonal1 [[1,2,3,4],[5,6,7,8],[9,10,11,13],[14,15,16,17]] 3 3 []

-- comprueba si en esa diagonal hay linea
list_diagonal1Com l = if igual12 q 
                        then if length q >= 4 then True else False
                        else False
  where q = mayoritario l


-- recorrer todas las diagonales
--rec_diagonal1 t m n j =

{--
Diagonal_1
*
  *
    * 
      *   --}
-- HACE LA COMPROBACION DE LA DIAGONAL QUE SE MUESTRA ARRIBA
-- INCLUYENDO TODAS LAS POSIBLES POSICIONES
diagonal1 t m n j = if m>(m-(m-3)) 
                            then if rec_horizontal t m n j == True
                                    then True
                                    else diagonal1 t (m-1) n j                              
                            else rec_horizontal t m 3 j



-- recorre horizontalmente las diagonales de la fila m y devuelve True o False si ha habido linea
rec_horizontal t m n j = if n>(n-(n-3)) 
                            then if list_diagonal1Com q == True 
                                    then True 
                                    else rec_horizontal t m (n-1) j
                            else list_diagonal1Com h
  where q = list_diagonal1 t m n j
        h = list_diagonal1 t m 3 j
-- rec_horizontal [[1,2,3,4],[5,6,,8],[9,10,2,13],[14,15,16,2]] 3 3 []


{- Diagonal_2
        *
      *
    *
  *          -}

-- hacemos funcion que devuelve una lista a partir de la diagonal
list_diagonal2 t m n j = a++b++c++d
  where a = [t!!(m-3)!!(n)]
        b = [t!!(m-2)!!(n-1)]
        c = [t!!(m-1)!!(n-2)]
        d = [t!!(m)!!(n-3)]


-- comprueba si en esa diagonal hay linea
list_diagonal2Com l = if igual12 q 
                        then if length q >= 4 then True else False
                        else False
  where q = mayoritario l


-- recorre horizontalmente las diagonales de la fila m y devuelve True o False si ha habido linea
rec_horizontal2 t m n j = if n>(n-(n-3)) 
                            then if list_diagonal2Com q == True 
                                    then True 
                                    else rec_horizontal2 t m (n-1) j
                            else list_diagonal2Com h
  where q = list_diagonal2 t m n j
        h = list_diagonal2 t m 3 j


diagonal2 t m n j = if m>(m-(m-3)) 
                            then if rec_horizontal2 t m n j == True
                                    then True
                                    else diagonal2 t (m-1) n j                              
                            else rec_horizontal2 t m 3 j






-- VERT Y HORIZ y las diagnales
comprob t m n j = if q==True || p==True || h==True || f==True then True else False
  where q = compv t m n
        p = comph t m
        h = diagonal1 t m n j
        f = diagonal2 t m n j

-- comprobar EMPATE
empate t 0 = fila_ocupada (t!!0)
empate t m = if fila_ocupada (t!!m) == True then empate t (m-1) else False

-- devuelve verdadero si todas las posiciones de l son distintas de 0
fila_ocupada [] = True
fila_ocupada [x] = if x==0 then False else True
fila_ocupada (x:xs) = if x==0 then False else fila_ocupada xs
 

-- FIN FUNCION COMPROBAR ---------------------------------------------------------------

-- devuelve el primer elemento de una lista
primero [] = error "lista vacia1"
primero [x] = x
primero (x:xs) = x



-- FUNCION PRINCIPAL
main :: IO ()
main = do
  args <- getArgs
  let n = atoi(args!!0)
  let m = atoi(args!!1)
  if n<7 || m<6  
     then ayuda
     else do
       putStrLn "Cuatro en linea"
       let t = tablero n m [] []
       if length args == 3 then jugador1' t (n-1) (m-1) else jugador1 t (n-1) (m-1)


-- modulo jugar dos humanos
jugador1 t n m = do
  print "Numero de columna:"
  let j = lista_colum [1] (n+1)
  print j
  print "-----------------"
  putStrLn (pintar t)
  print "-----------------" 
  print j
  if comprob t m n [] == True
     then print "Gana jugador2"
     else do
        if empate t m == True
           then print "Empate"
           else do
               putStrLn "Jugador 1: introduce columna:"
               c <- getLine
               let d = ((atoi c)-1)
               if es_posible t m n d == True 
                  then do 
                    let z = (insertar 1 t d m)
                    jugador2 z n m    --ordenador z n m
                  else do 
                    print "Columna incorrecta"
                    jugador1 t n m



jugador2 t n m = do
  print "Numero de columna:"
  let j = lista_colum [1] (n+1)
  print j
  print "-----------------"
  putStrLn (pintar t)
  print "-----------------" 
  print j
  if comprob t m n [] == True
          then print "Gana jugador1"
          else do
            if empate t m == True
               then print "Empate"
               else do
                 putStrLn "Jugador 2: introduce columna:"
                 c <- getLine
                 let d = ((atoi c)-1)
                 if es_posible t m n d == True 
                    then do 
                      let z = (insertar 2 t d m)
                      jugador1 z n m
                    else do
                      print "Columna incorrecta"
                      jugador2 t n m
-- imprime la ayuda del juego
ayuda = print "Se deben pasar como argumento: < numero de columnas >= 7 >   < numero de filas >= 6 >"



jugador1' t n m = do
  print "Numero de columna:"
  let j = lista_colum [1] (n+1)
  print j
  print "-----------------"
  putStrLn (pintar t)
  print "-----------------" 
  print j
  if comprob t m n [] == True
     then print "Gana el Ordenador"
     else do
       if empate t m == True
          then print "Empate"
          else do
               putStrLn "Jugador 1: introduce columna:"
               c <- getLine
               let d = ((atoi c)-1)
               if es_posible t m n d == True 
                  then do 
                    let z = (insertar 1 t d m)
                    ordenador z n m
                  else do 
                    print "Columna incorrecta"
                    jugador1' t n m
  

--ordenador :: IO ()
ordenador t n m = do
  print "Numero de columna:"
  let j = lista_colum [1] (n+1)
  print j
  print "-----------------"
  putStrLn (pintar t)
  print "-----------------" 
  print j
  if comprob t m n [] == True
     then print "Gana jugador1"
     else do
          if empate t m == True
             then print "Empate"
             else do
               putStrLn "Ordenador: introduce columna:" 
               let z = insertarIA t m n
               jugador1' z n m


-- ###################################    IA   ##########################################
{-
En el segundo caso, la estrategia del ordenador 
consistira en elegir la fila mas al centro
posible en la que al colocar una ficha,
el contrario no hace 4 en raya en la siguiente jugada,
siempre que sea posible, claro.
-}
{-
PROCEDIMIENTO: 
  Guardamos en una lista las longitudes de cada columna(las fichas que tiene cada una)
  Y escogemos la que se acerque más a la fila más al centro en la que colocar una ficha
-}



insertarIA t m 0 = if q==0 
                      then insertar 2 t 0 m
                      else insreverse t m 0
  where q = t!!0!!0
insertarIA t m n = if q==0 
                      then insIA t m n --si q=0 entonces fila vacia
                      else insertarIA t m (n-1)               
  where q = t!!0!!n -- la más alta


-- comprobar que pasa si insertamos en una casilla
--insIA t m 0 = insertarIA t m 0
insIA t m n = if comprob q m n [] == True
                 then insertar 2 t n m
                 else insertarIA t m (n-1)
  where q = insertar 1 t n m


insreverse t m n = if q==0 then insertar 2 t n m else insreverse t m (n+1)
  where q = t!!0!!n

