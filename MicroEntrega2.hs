module MicroEntrega1 where
import Text.Show.Functions

---------- 3.1 ----------
data Procesador = Procesador {
  memoria :: [Int],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String,
  listaDeFunciones :: [Procesador -> Procesador] -- Se agrego para la entrega 2
} deriving (Show)

mem = replicate 1024 0

xt8088 = Procesador {memoria = mem, acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = [], listaDeFunciones = []}

-- Funciones auxiliares --
operar funcion numero1 numero2 = funcion numero1 numero2
incrementarContador procesador = procesador {programCounter = programCounter procesador +1}
procesarInstruccion instruccion procesador = incrementarContador $ instruccion (procesador)

---------- 3.2 ----------
nop = incrementarContador
aumentarTresPosiciones = nop.nop.nop

---------- 3.3 ----------
lodv valor procesador = procesador {acumuladorA = valor}
swap procesador = procesador {acumuladorA = acumuladorB procesador, acumuladorB = acumuladorA procesador}
add procesador = procesador {acumuladorA = operar (+) (acumuladorA procesador) (acumuladorB procesador), acumuladorB = 0}

sumar10y22 = procesarInstruccion(add).procesarInstruccion(lodv 22).procesarInstruccion(swap).procesarInstruccion(lodv 10)

---------- 3.4 ----------
divide (Procesador ememoria eacumuladorA 0 eprogramCounter _ eListaDeFunciones) = Procesador {memoria = ememoria, acumuladorA = eacumuladorA, acumuladorB = 0, programCounter = eprogramCounter, mensajeError = "DIVISION BY ZERO", listaDeFunciones = eListaDeFunciones}
divide (Procesador ememoria eacumuladorA eacumuladorB eprogramCounter emensajeError eListaDeFunciones) = Procesador {memoria = ememoria, acumuladorA = operar div eacumuladorA eacumuladorB, acumuladorB = 0, programCounter = eprogramCounter, mensajeError = emensajeError, listaDeFunciones = eListaDeFunciones}
-- se uso la funcion round, pero se puede usar truncate
str adress valor procesador = procesador { memoria = take (adress -1) (memoria procesador) ++ valor : drop  adress (memoria procesador)}

lod adress procesador = procesador {acumuladorA = (memoria procesador) !! (adress-1)}

---------- 4.2 ----------
fp20 = Procesador {memoria = mem, acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = [], listaDeFunciones = []}

---------- 4.3 ----------
-- Prueba 1
memoriaAT8086 = take 20 (iterate (+1) 1)
at8086 = Procesador {memoria = memoriaAT8086, acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = [], listaDeFunciones = []}
-- Prueba 3
prueba433 = procesarInstruccion(divide).procesarInstruccion(lod 1).procesarInstruccion(swap).procesarInstruccion(lod 2).procesarInstruccion(str 2 0).procesarInstruccion(str 1 2)
-- Prueba 4
prueba434 = procesarInstruccion(divide).procesarInstruccion(lodv 12).procesarInstruccion(swap).procesarInstruccion(lodv 4)

---------- ENTREGA 2 ----------

cargarFuncion funcion procesador = procesador {listaDeFunciones = listaDeFunciones(procesador) ++ [funcion]}
selecccionarFuncion numeroDeFuncion procesador =  (!!) (listaDeFunciones procesador) (numeroDeFuncion - 1)
ejecutarFuncion procesador funcion = funcion procesador
