-- | Este es el codigo fuente del TP para la materia Progamación Avanza - 2023 - UNRC
-- El texto tiene varias funciones y tipos de datos ya implementados, el trabajo consiste en completar aquellas funciones 
-- que no se encuentran implementadas (es decir, estan definidas con 'undefined').  

module Piedras where 

-- | El siguiente tipo define los jugadores del juego, C por Computadora
--   y H por Humano
data Jugador = C | H deriving (Eq,Show)


-- | Definimos los estados, un estado es un jugador mas las cantidad de piedras disponibles
type Estado = (Jugador, Int) 

-- | Definimos los posibles estados del juegos, el resultado del juego, puede ser que la computadora pierda, o gane
data Resultado = CPerdio | CGano deriving (Eq,Ord,Show) 
-- | Definimos las posible jugadas, sacar 1 piedra, 3 piedras o 4 piedras
jugadas = [1,3,4]

-- | La funcion otro Jugador, dado un jugador, devuelve el otro jugador, por ejemplo: otroJugador C = H
otroJugador :: Jugador -> Jugador
otroJugador C = H
otroJugador H = C 

-- | Dada una jugada (cantidad de piedras que se retiran) y un estado retorna el estado resultante, se deben controlar los casos de jugadas no posibles
hacerJugada :: Int -> Estado -> Estado
hacerJugada jugada (jugador, piedras) | (jugada == 1 || jugada == 3 || jugada == 4) && piedras - jugada >= 0 = (otroJugador jugador, piedras - jugada) 
                                      | otherwise = error "jugada no valida"
--en la primer guarda nos aseguramos que solo pueda ingresar como jugada 1,3 y 4,
--agregamos la conjuncion piedras-jugada >= 0 para contemplar que no quiera sacar mas piedras de las que hay, 
--sino por ejemplo si poniamos hacerJugada 4 (H,3) devolvia (C,-1) y eso no es correcto. 

-- |  evalEstado toma un estado como parametro, y dice si el estado es ganador o perdedor considerando
--  las mejores jugadas del oponente. Por ejemplo, evalEstado (H,2) = CGano, porque H solo puede
--  retirar 1 y luego la computadora retira 1 y gana
evalEstado :: Estado -> Resultado
evalEstado  (j, k)  | (k == 0) = if j == C then CPerdio else CGano
                  |  k>0 && j == C   = foldl max CPerdio $ map evalEstado posibleJugs
                  |  k>0 && j == H   = foldl min CGano $ map evalEstado posibleJugs   
                  |  otherwise = error "jugada no valida"
                  where posibleJugs = [(otroJugador j, k - i) | i<- jugadas, i<=k]    

-- | Calcula la mejor jugada para un estado dado, para el jugador dado.
-- Por ejemplo, mejorJug (H,3) debería devolver 3, ya que la mejor jugada para H cuando hay 3 piedras es retirar 3.
-- Ayuda: Tener en cuenta que el tipo Resultado implementa la clase Ord, es decir, tenemos
-- CPerdio < CGano. Es decir:
--
-- 	Para el caso mejorJug(C, k) tenemos que devolver la jugada que nos da el resultado maximo con respecto a C (es 
-- 	decir, la mejor jugada para la computadora).
-- 	
-- 	En el caso mejorJug (H, k) tenemos que devolver la jugada que nos da el valor minimo (es decir, consideramos 
-- 	la mejor jugada para H, que seria la peor para C).
mejorJug :: Estado -> Int
mejorJug (C, piedras) = maximum (1:[j | j <- jugadas,j <= piedras, (evalEstado (H, piedras - j) == CGano) || (j == piedras)])
 --Si hay 1,3 o 4 piedras la mejor jugada para la computadora es sacar esa cantidad de pidras.
 --Si no hay esas piedras hicimos 
 --una lista con las mejores jugadas para la computadora y que saque las piedras maximas de esa lista, 
 --le concatenamos el 1 ya que aveces no hay jugadas ganadoras en ese caso  por defecto hacemos que saque 1 piedra.
mejorJug (H, piedras) = maximum (1:[j | j <- jugadas,j <= piedras, (evalEstado (C, piedras - j) == CPerdio) || (j == piedras)]) 
 --Si hay 1,3 o 4 piedras la mejor jugada para el humano es sacar esa cantidad de pidras.
 --Si no hay esas piedras hicimos 
 --una lista con las peores jugadas para la computadora y que saque las piedras maximas de esa lista, 
 --le concatenamos el 1 ya que a veces no hay jugadas donde la computadora pierde en ese caso  por defecto hacemos que saque 1 piedra.


-- | Las siguientes funciones implementan una pequeña interface para poder jugar interactivamente,.
jugar :: Estado -> IO()
jugar (j,k) = do
	        putStrLn ("Hay "++ (show k) ++ " piedras, cuantas saca?:")	  
	        jugada <-  getLine
	        let s  = read jugada 
	            (j', k') = hacerJugada s (j,k) 
	        if k'==0 then (putStrLn "Gano!")
	        else do 
	     	     let mj = mejorJug (j',k')    
	     	     putStrLn ("mi jugada: "++(show mj))
	             if k' - mj ==0 
	             then putStrLn "Perdio!"
	             else do 
	             	  jugar (H, k' - mj) 
	          
-- | Comienza el juego con una cantidad de piedras dada el Humano  
comenzarJuego :: Int -> IO()
comenzarJuego cant | cant <= 0 = error "La cantidad de piedras debe ser mayor que 0."
                   | otherwise = jugar (H, cant)

-- juegosGanadores k, calcula todos los comienzos ganadores para la computadora hasta con k piedras
-- por ejemplo, juegosGanadores 10 = [2,7,9]
juegosGanadores :: Int -> [Int]
juegosGanadores i = [x | x <-[1..i] ,evalEstado(H,x) == CGano]
--hacemos una lista por comprension con todos los x tal que en evalEstado(H,x) sea igual a CGano

{-TEST:
Estos son los test que realizamos:

>otroJugador C

H

>otroJugador H

C

>hacerJugada 2 (H,4)

jugada no valida

>hacerJugada 5 (H,8)

jugada no valida

>hacerJugada 0 (H,3)

jugada no valida

>hacerJugada 4 (H,3)

jugada no valida, ya que no podria sacar 4 piedras cuando tenes 3 

>hacerJugada 1 (H,5)

(C,4)

>hacerJugada 3 (H,5)

(C,2)

>hacerJugada 4 (C,4)

(H,0) Y este caso ganaria la computadora ya que el humano en este caso se queda sin piedras por agarrar 


*Piedras> juegosGanadores 20
[2,7,9,14,16]

En este caso son los estados en el cual si el humano tiene el estado
2,7,9,14,16 ya pierde porque la computadora tiene un algoritmo que siempre te gana
por lo tanto
si el estado de H es en algun momento (H,14) basicamente ya perdio por lo tanto CGana 


*Piedras> juegosGanadores 1
[]

Devuelve una lista vacia puesto que el estado de el humano es (H,1)
lo cual ya basicamente gano porque retira una piedra y el estado de C seria (C,0)
por lo tanto ya CPierde 

> mejorJug (C,1)
1
> mejorJug (C,2)
1
> mejorJug (C,3)
3
> mejorJug (C,4)
4
> mejorJug (C,5)
3
> mejorJug (C,7)
1
Estas son las mejores jugadas que puede hacer la computadora cuando tiene esas cantidades de piedras, y cuando cualquiera de los
jugadores tiene 7 devuelve 1 ya que como cualquier jugada lo lleva a perder el juego


> mejorJug (H,1)
1
> mejorJug (H,2)
1
> mejorJug (H,3)
3
> mejorJug (H,4)
4
> mejorJug (H,5)
3
-}