module MicroEntrega1 where

---------- 3.1 ----------
data Procesador = Procesador {
  memoria :: [Int],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String
} deriving (Show)

mem = replicate 1024 0

xt8088 = Procesador {memoria = mem, acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = [] }

-- Funciones auxiliares
operar funcion numero1 numero2 = funcion numero1 numero2
incrementarContador procesador = procesador {programCounter = programCounter procesador +1}


---------- 3.2 ----------
nop = incrementarContador
aumentarTresPosiciones = nop.nop.nop

---------- 3.3 ----------
lodv valor procesador = incrementarContador $ procesador {acumuladorA = valor}
swap procesador = incrementarContador $ procesador {acumuladorA = acumuladorB procesador, acumuladorB = acumuladorA procesador}
add procesador = incrementarContador $ procesador {acumuladorA = operar (+) (acumuladorA procesador) (acumuladorB procesador), acumuladorB = 0}

sumar10y22 = add.(lodv 22).swap.(lodv 10)

---------- 3.4 ----------

divide (Procesador ememoria eacumuladorA 0 eprogramCounter _) = Procesador {memoria = ememoria, acumuladorA = eacumuladorA, acumuladorB = 0, programCounter = eprogramCounter + 1, mensajeError = "DIVISION BY ZERO"}
divide (Procesador ememoria eacumuladorA eacumuladorB eprogramCounter emensajeError) = Procesador {memoria = ememoria, acumuladorA = operar div eacumuladorA eacumuladorB, acumuladorB = 0, programCounter = eprogramCounter + 1, mensajeError = emensajeError}
-- se uso la funcion round, pero se puede usar truncate
str adress valor procesador = incrementarContador $ procesador { memoria = take (adress -1) (memoria procesador) ++ valor : drop  adress (memoria procesador)}

lod adress procesador = incrementarContador $ procesador {acumuladorA = (memoria procesador) !! (adress-1)}

---------- 4.2 ----------
fp20 = Procesador {memoria = mem, acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = [] }

---------- 4.3 ----------
-- Prueba 1
memoriaAT8086 = take 20 (iterate (+1) 1)
at8086 = Procesador {memoria = memoriaAT8086, acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = [] }
-- Prueba 3
prueba433 = divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)
-- Prueba 4
prueba434 = divide.(lodv 12).swap.(lodv 4)

---------- 5.1 ----------
