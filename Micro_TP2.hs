module Micro_TP2 where
import Text.Show.Functions

data Microprocesador = Microprocesador { acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, etiquetaError :: String, memoriaDeDatos :: [Int], programa :: Programa } deriving Show

microMemoriaInfinita = Microprocesador { acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiquetaError = [], memoriaDeDatos = [0,0..], programa = [add, lodv 22, swap, lodv 10] }

fp20 = Microprocesador { acumuladorA = 7, acumuladorB = 24, programCounter = 0, etiquetaError = [], memoriaDeDatos = [], programa = [] }

at8086 = Microprocesador { acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiquetaError = [], memoriaDeDatos = [1..20], programa = []  }

xt8088 = Microprocesador { 
 acumuladorA = 0,
 acumuladorB = 0,
 programCounter = 0,
 etiquetaError = [],
 memoriaDeDatos = [],
 programa = [divide, lod 1, swap, lod 2, str 2 0, str 1 2]}

--unProgramaLoco = [add, lodv 22, swap, lodv 10]

type Instruccion = Microprocesador -> Microprocesador
type Programa = [Instruccion]

------------------------  FUNCIONES TP 2  -----------------------------

-- 3.2 Punto 2: Ejecución de un programa
ejecutarProgramaEnMicro :: Microprocesador -> Microprocesador
ejecutarProgramaEnMicro micro = (ifnz.programa) micro $ micro
-- a = ejecutarProgramaEnMicro 1 xt8088

-- 3.3 Punto 3: IFNZ
ifnz :: Programa -> Microprocesador -> Microprocesador
ifnz instrucciones micro 
 | acumuladorA micro /= 0 = foldr ejecutarInstruccionSinError micro instrucciones
 | otherwise = micro
-- ifnz [swap, lodv 3] fp20

--ejecutarInstrucion :: Instruccion -> Microprocesador -> Microprocesador
--ejecutarPrograma programa micro = 

ejecutarInstruccionSinError :: Instruccion -> Microprocesador -> Microprocesador
ejecutarInstruccionSinError funcion micro
 | etiquetaError micro == "" = (nop.funcion) micro
 | otherwise = micro

-- 3.4 Punto 4: Depuración de un programa
depurarPrograma :: Microprocesador -> Programa -> Programa
depurarPrograma micro unPrograma = filter (condicionDepuracion micro) unPrograma
-- depurarPrograma [swap, nop, lodv 133, lodv 0, str 1 3, str 2 0]

condicionDepuracion :: Microprocesador -> Instruccion -> Bool
condicionDepuracion micro unaInstruccion = (verificarTodoEnCero.ejecutarInstruccionSinError unaInstruccion) micro

verificarTodoEnCero :: Microprocesador -> Bool
verificarTodoEnCero micro = not ((acumuladorA micro == 0) && (acumuladorB micro == 0) && all (==0) (memoriaDeDatos micro))

-- 3.5 Punto 5: Memoria ordenada
ordenadaMemoriaDelMicro :: Microprocesador -> Bool
ordenadaMemoriaDelMicro = estaOrdenadaLista.memoriaDeDatos
-- ordenadaMemoriaDelMicro at8086

estaOrdenadaLista :: (Ord a) => [a] -> Bool
estaOrdenadaLista []       = True
estaOrdenadaLista [_]      = True
estaOrdenadaLista (x:y:xs) = x <= y && estaOrdenadaLista (y:xs)

-- 3.6:

-- 	¿Qué sucede al querer cargar y ejecutar el programa que suma 10 y 22 en el procesador con memoria infinita?
-- 		El programa se ejecuta correctamente, el acumulador A con 32 y el acumulador B con 0.

-- 	¿Y si queremos saber si la memoria está ordenada?
-- 		El programa no se rompe, pero no se termina de ejecutar porque nunca va a encontrar el ultimo elemento para
--		saber si esta ordenada la memoria o no.

-- 	Relacione lo que pasa con el concepto y justifique.
--		Haskell es suficientemente inteligente como para ejecutar instrucciones en una lista infinita, tal asi qu
--		si se pide alguna operacion con una cantidad limitada de elementos de una lista infinita, haskell puede resolverlo.
--		Pero en caso que se pida una operacion con todos los elementos, no habra ninguna respuesta porque no se puede operar con
--		elementos infinitos por ende nunca va a responer.
-------------------------------- FUNCIONES TP 1 --------------------------------

---- Typeo la funcion NOP: aumenta en 1 el Program Counter (PC)
nop :: Instruccion
nop micro = micro {programCounter = programCounter micro + 1} 

---- Typeo la funcion LODV: Carga en el acumulador A el valor 'valor'
lodv :: Int -> Instruccion
lodv valor micro = micro { acumuladorA = valor }

---- Typeo la funcion SWAP: Intercambia los valores de los acumuladores (el del A va al B y viceversa).
swap :: Instruccion
swap micro = micro {acumuladorA = acumuladorB micro , acumuladorB = acumuladorA micro}

---- Typeo la funcion ADD: Suma los valores de los dos acumuladores, el resultado queda en el acumulador A, el acumulador B debe quedar en 0
sumarAcumuladores :: Instruccion
sumarAcumuladores micro = micro {acumuladorA = acumuladorA micro + acumuladorB micro}

insertarCeroEnB :: Instruccion
insertarCeroEnB micro = micro {acumuladorB = 0}

add :: Instruccion
add = insertarCeroEnB.sumarAcumuladores

---- Typeo la funcion DIVIDE (DIV): Divide el valor del acumulador A por el valor del acumulador B, el resultado queda en el acumulador A, el acumulador B debe quedar en 0. Si la division es por cero, el mensaje de error debe ser: DIVISION BY CERO
divide :: Instruccion
divide micro  | (acumuladorB micro) == 0 = micro { etiquetaError = "DIVISION BY ZERO" }
              | otherwise = (insertarCeroEnB.dividirAcumuladores) micro

dividirAcumuladores :: Instruccion
dividirAcumuladores micro = micro {acumuladorA = (div)(acumuladorA micro)(acumuladorB micro)}

---- Typeo la funcion LOD: Carga el acumulador A con el contenido de la memoria de datos en la posición 'posicion'
lod :: Int -> Instruccion
lod posicion micro = micro {acumuladorA = (traerDeLista (memoriaDeDatos micro) (posicion-1))}

traerDeLista = (!!)

---- Typeo la funcion STR: Guarda el valor 'dato' en la posición 'posicion' de la memoria de datos
str :: Int -> Int -> Instruccion
str posicion valor micro = micro { memoriaDeDatos = (dividirListaTraerElementosIzquierda (posicion-1) (memoriaDeDatos micro)) ++ (valor : (dividirListaTraerElementosDerecha (posicion) (memoriaDeDatos micro)))}

dividirListaTraerElementosIzquierda = take

dividirListaTraerElementosDerecha = drop
