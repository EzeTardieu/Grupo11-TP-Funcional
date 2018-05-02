---------- 3.1 ----------
data Procesador = Procesador {memoria :: [Int], acumuladorA :: Int, acumuladorB :: Int, contador :: Int, failure :: String} deriving (Show)
mem = replicate 256 0

xt8088 = Procesador {memoria = mem, acumuladorA = 0, acumuladorB = 0, contador = 0, failure = "No hay error" }

---------- 3.2 ----------
nop procesador = procesador {contador = contador procesador +1}
aumentarTresPosiciones = nop.nop.nop

---------- 3.3 ----------
lodv valor procesador = procesador {acumuladorA = valor, contador = contador procesador +1}
swap procesador = procesador {acumuladorA = acumuladorB procesador, acumuladorB = acumuladorA procesador, contador = contador procesador +1}
add procesador = procesador {acumuladorA = acumuladorA procesador + acumuladorB procesador, acumuladorB = 0, contador = contador procesador +1}

sumar10y22 = add.(lodv 22).swap.(lodv 10)

---------- 3.4 ----------

divide (Procesador ememoria eacumuladorA 0 econtador _) = Procesador {memoria = ememoria, acumuladorA = eacumuladorA, acumuladorB = 0, contador = econtador + 1, failure = "DIVISION BY ZERO"}
divide (Procesador ememoria eacumuladorA eacumuladorB econtador efailure) = Procesador {memoria = ememoria, acumuladorA = round (fromIntegral eacumuladorA / fromIntegral eacumuladorB), acumuladorB = 0, contador = econtador + 1, failure = efailure}
-- se uso la funcion round, pero se puede usar truncate
str adress valor procesador = procesador { memoria = take (adress -1) (memoria procesador) ++ [valor] ++ drop  adress (memoria procesador)}

lod adress procesador = procesador {acumuladorA = (!!) (memoria procesador) (adress-1)}
