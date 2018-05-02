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

---------- 3.2 ----------
nop procesador = procesador {programCounter = programCounter procesador +1}
aumentarTresPosiciones = nop.nop.nop

---------- 3.3 ----------
lodv valor procesador = procesador {acumuladorA = valor, programCounter = programCounter procesador +1}
swap procesador = procesador {acumuladorA = acumuladorB procesador, acumuladorB = acumuladorA procesador, programCounter = programCounter procesador +1}
add procesador = procesador {acumuladorA = acumuladorA procesador + acumuladorB procesador, acumuladorB = 0, programCounter = programCounter procesador +1}

sumar10y22 = add.(lodv 22).swap.(lodv 10)

---------- 3.4 ----------

divide (Procesador ememoria eacumuladorA 0 eprogramCounter _) = Procesador {memoria = ememoria, acumuladorA = eacumuladorA, acumuladorB = 0, programCounter = eprogramCounter + 1, mensajeError = "DIVISION BY ZERO"}
divide (Procesador ememoria eacumuladorA eacumuladorB eprogramCounter emensajeError) = Procesador {memoria = ememoria, acumuladorA = round (fromIntegral eacumuladorA / fromIntegral eacumuladorB), acumuladorB = 0, programCounter = eprogramCounter + 1, mensajeError = emensajeError}
-- se uso la funcion round, pero se puede usar truncate
str adress valor procesador = procesador { memoria = take (adress -1) (memoria procesador) ++ [valor] ++ drop  adress (memoria procesador), programCounter = programCounter procesador + 1}

lod adress procesador = procesador {acumuladorA = (!!) (memoria procesador) (adress-1), programCounter = programCounter procesador + 1}

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
