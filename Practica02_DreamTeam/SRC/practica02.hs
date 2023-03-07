module CalculadoraRaid where
    --NOTA: Funciona solo para teras
    
    --Función para separar una lista en lista de n elementos
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l
        | n > 0 = (take n l) : (group n (drop n l))
        | otherwise = error "Negative or zero n"
        
    -- Función aux que convertiria de gb a teras, pero no fue necesaria dada la NOTA del pricipio
    -- No lo borramos para que vean que lo tomamos en cuenta.
    gbAT :: [Float] -> [Float]
    gbAT xs = map convierte xs
        where 
            convierte n
                | n > 100 = n/1000
                | otherwise = n  
    -- Función que saca el disco mas pequeño
    discoMP :: [Float] -> Float
    discoMP discos =  foldr1 min discos 
    -- Función que agarra la diferencia de espacios entre el disco mas pequeño y los demas        
    sinUso :: [Float] -> Float            
    sinUso discos = foldr (\x y -> (y-(mp)) + x) 0 discos
        where
            mp = discoMP discos

    -- Función que resuelve raid 0
    raid0 :: Float -> [Float] -> [Float]
    raid0 n discos = [((discoMP discos)*n), 0, (sinUso discos)]

    -- Función que resuelve raid 1
    raid1 :: Float -> [Float] -> [Float]
    raid1 n discos 
        | mod (round n) 2 == 0 = [(((discoMP discos)*n)/2), (((discoMP discos)*n)/2), (sinUso discos)]
        | otherwise = error "No se puede hacer raid1 con un numero impar de discos."

    -- Función que resuelve raid 5
    raid5 :: Float -> [Float] -> [Float]
    raid5 n discos
        | n >= 3 = [(((discoMP discos)*(n-1))), (discoMP discos), (sinUso discos)]
        | otherwise = error "No se puede hacer raid5 con menos de 3 discos."

    -- Función que resuelve raid 6
    raid6 :: Float -> [Float] -> [Float]
    raid6 n discos
        | n >= 4 = [(((discoMP discos)*(n-2))), ((discoMP discos)*2), (sinUso discos)]
        | otherwise = error "No se puede hacer raid6 con menos de 4 discos."

    -- Función que resuelve raid 0+1
    raid01 :: Float -> [Float] -> [Float]
    raid01 n discos 
        | mod (round n) 2 == 0 = [(raid0'!!0),(raid0'!!0),(sinUso discos)]
        | otherwise = zipWith (+) (raid10 (n-1) (init discos)) [0,0,discos!!(round n-1)]
        where
            discosN = map (\x -> if x > discoMP discos then discoMP discos else x) discos
            grupos = group (2) discosN
            raid0' = raid0 2 (grupos!!0)

    -- Función que resuelve raid 1+0
    raid10 :: Float -> [Float] -> [Float]
    raid10 n discos
        | mod (round n) 2 == 0 = [(raid1'!!0)*(n/2),(raid1'!!1)*(n/2),(sinUso discos)]
        | otherwise = zipWith (+) (raid10 (n-1) (init discos)) [0,0,discos!!(round n-1)]
        where
            discosN = map (\x -> if x > discoMP discos then discoMP discos else x) discos
            grupos = group (2) discosN
            raid1' = raid1 2 (grupos!!0)

    -- Función que resuelve raid 5+0
    raid50 :: Float -> [Float] -> [Float]
    raid50 n discos
        | n >= 6 =[((discoMP discos)*(n-2)), ((discoMP discos)*2), (sinUso discos)]
        | otherwise = error "No se puede hacer un raid50 con menos de 6 discos"
    -- Función que resuelve raid 6+0

    raid60 :: Float -> [Float] -> [Float]
    raid60 n discos
        | n >= 8 =[((discoMP discos)*(n-4)), ((discoMP discos)*4), (sinUso discos)]
        | otherwise = error "No se puede hacer un raid60 con menos de 8 discos"   

    -- Función que regresa una cadena del raid hecho
    calculaRaid :: String -> Float -> [Float] -> String
    calculaRaid str n ns = "Almacenamiento: " ++ show (resultado!!0) ++ " Protección: " ++ show (resultado!!1) ++ " Desperdiciado: " ++ show (resultado!!2)
                where
                    resultado
                        | str == "0" = raid0 n ns
                        | str == "1" = raid1 n ns
                        | str == "5" = raid5 n ns
                        | str == "6" = raid6 n ns
                        | str == "1+0" = raid10 n ns
                        | str == "0+1" = raid01 n ns
                        | str == "5+0" = raid50 n ns
                        | str == "6+0" = raid60 n ns
                        | otherwise = error "Raid no reconocido."
            
