module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Ciudad = UnaCiudad{
    nombre :: String,
    anioFundacion :: Number, 
    atraccionesPrincipales :: [String],
    costoDeVida :: Number
}   deriving(Show)

baradero :: Ciudad
baradero = UnaCiudad{
    nombre = "Baradero",
    anioFundacion = 1615,
    atraccionesPrincipales = ["Parque del Este", "Museo Alejandro Barbich"],
    costoDeVida = 150
}

nullish :: Ciudad
nullish = UnaCiudad{
    nombre = "Nullish",
    anioFundacion = 1800,
    atraccionesPrincipales = [],
    costoDeVida = 140
}

caletaOlivia :: Ciudad
caletaOlivia = UnaCiudad{
    nombre = "Caleta Olivia",
    anioFundacion = 1901,
    atraccionesPrincipales = ["El Gorosito", "Faro Costanera"],
    costoDeVida = 120
}

maipu :: Ciudad
maipu = UnaCiudad{
    nombre = "Maipu",
    anioFundacion = 1878,
    atraccionesPrincipales = ["Fortin Kakel"],
    costoDeVida = 115
}

azul :: Ciudad
azul = UnaCiudad{
    nombre = "Azul",
    anioFundacion = 1832,
    atraccionesPrincipales = ["Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"],
    costoDeVida = 190
}


-- Punto 1 

menor1800 :: Number -> Bool
menor1800 numero = numero < 1800

valorDeCiudad :: Ciudad -> Number
valorDeCiudad ciudad | (menor1800 . anioFundacion) ciudad  = (1800 - anioFundacion ciudad) * 5
                     | (null . atraccionesPrincipales) ciudad = costoDeVida ciudad * 2
                     | otherwise = 3 * costoDeVida ciudad

-- > valorDeCiudad baradero
-- 925
-- > valorDeCiudad nullish
-- 280
-- > valorDeCiudad caletaOlivia 
-- 360


-- Punto 2 

isVowel :: Char -> Bool
isVowel character = character `elem` "aeiouAEIOU"

esAtraccionCopada :: Ciudad -> Bool
esAtraccionCopada ciudad = (not . null . atraccionesPrincipales) ciudad && any (isVowel . head) (atraccionesPrincipales ciudad)

-- > esAtraccionCopada baradero
-- False
-- > esAtraccionCopada nullish 
-- False
-- > esAtraccionCopada caletaOlivia 
-- True

esSobria :: Ciudad -> Number -> Bool
esSobria ciudad cantidadLetras = (not . null . atraccionesPrincipales) ciudad && all ((cantidadLetras <) . length) (atraccionesPrincipales ciudad)

-- > esSobria baradero 14
-- True
-- > esSobria baradero 15
-- False
-- > esSobria nullish 5  
-- False


tieneNombreRaro :: Ciudad -> Bool
tieneNombreRaro = (<5). length . nombre 

-- > tieneNombreRaro maipu
-- False
-- > tieneNombreRaro azul
-- True


-- Parte 3

agregarAtraccion :: Ciudad -> String -> Ciudad
agregarAtraccion ciudad atraccion = ciudad{
                                            atraccionesPrincipales = atraccion : atraccionesPrincipales ciudad,
                                            costoDeVida = costoDeVida ciudad + costoDeVida ciudad * 0.2
                                        }


-- > agregarAtraccion azul "Balneario Municipal Alte. Guillermo Brown"
-- UnaCiudad
--    { nombre = "Azul"
--    , anioFundacion = 1832
--    , atraccionesPrincipales =
--       [ "Balneario Municipal Alte. Guillermo Brown"
--       , "Teatro Español"
--       , "Parque Municipal Sarmiento"
--       , "Costanera Cacique Catriel"
--       ]
--    , costoDeVida = 228
--    }

crisis:: Ciudad -> Ciudad
crisis ciudad | (null . atraccionesPrincipales) ciudad = ciudad{costoDeVida = costoDeVida ciudad - costoDeVida ciudad * 0.1}
              | otherwise = ciudad{
                            atraccionesPrincipales = init (atraccionesPrincipales ciudad),
                            costoDeVida = costoDeVida ciudad - costoDeVida ciudad * 0.1
                            }
-- > crisis azul
-- UnaCiudad
--    { nombre = "Azul"
--    , anioFundacion = 1832
--    , atraccionesPrincipales =
--        [ "Teatro Español"
--        , "Parque Municipal Sarmiento"
--        ]
--    , costoDeVida = 171
--    }
-- > crisis nullish
-- UnaCiudad
--    { nombre = "Nullish"
--    , anioFundacion = 1800
--    , atraccionesPrincipales = []
--    , costoDeVida = 126
--    }


remodelacion:: Ciudad -> Number -> Ciudad
remodelacion ciudad porcentaje = ciudad{
    nombre = "New " ++ nombre ciudad,
    costoDeVida = costoDeVida ciudad + costoDeVida ciudad * porcentaje/100
}

-- > remodelacion azul 50
-- UnaCiudad
--    { nombre = "New Azul"
--   , anioFundacion = 1832
--    , atraccionesPrincipales =
--        [ "Teatro Español"
--        , "Parque Municipal Sarmiento"
--        , "Costanera Cacique Catriel"
--        ]
--    , costoDeVida = 285
--    }

reevaluacion :: Ciudad -> Number -> Ciudad
reevaluacion ciudad numero_letras | esSobria ciudad numero_letras = ciudad{
                                                                    costoDeVida = costoDeVida ciudad + costoDeVida ciudad * 0.1
                                                                }
                                  | otherwise = ciudad{
                                                       costoDeVida = costoDeVida ciudad - 3
                                                       }

-- > reevaluacion azul 14
-- UnaCiudad
--    { nombre = "Azul"
--    , anioFundacion = 1832
--    , atraccionesPrincipales =
--       [ "Teatro Español"
--       , "Parque Municipal Sarmiento"
--        , "Costanera Cacique Catriel"
--        ]
--    , costoDeVida = 187
--    }

-- > reevaluacion azul 13
-- UnaCiudad
--    { nombre = "Azul"
--    , anioFundacion = 1832
--    , atraccionesPrincipales =
--        [ "Teatro Español"
--       , "Parque Municipal Sarmiento"
--        , "Costanera Cacique Catriel"
--        ]
--    , costoDeVida = 209
--    }

-- Punto 4
moreno = UnaCiudad{
    nombre = "Moreno",
    anioFundacion = 1879,
    atraccionesPrincipales = ["Nine","El Fogonazo"],
    costoDeVida = 200
}
-- Moreno cumple con todas las condiciones si se lo evalua con 4 letras

transformacion :: Ciudad -> String -> Number -> Number-> Ciudad
transformacion ciudad atraccion cantidad_letras porcentaje = reevaluacion (crisis (remodelacion (agregarAtraccion ciudad atraccion) porcentaje)) cantidad_letras

-- > transformacion moreno "Josephina" 7 40
-- UnaCiudad
--    { nombre = "New Moreno"
--    , anioFundacion = 1879
--    , atraccionesPrincipales =
--        [ "Josephina"
--        , "Nine"
--        ]
--    , costoDeVida = 299.4
--    }

-- Parte 2

-- Punto 4.1

data Anio = UnAnio{
    numero :: Number, 
    eventos :: [Ciudad -> Ciudad]
}   deriving(Show)

dosMilVeintidos :: Anio
dosMilVeintidos = UnAnio{
    numero = 2022, 
    eventos = [crisis, (`remodelacion` 5), (`reevaluacion` 7)]
}

dosMilQuince :: Anio
dosMilQuince = UnAnio{
    numero = 2015, 
    eventos = []
}

dosMilVeintiuno :: Anio
dosMilVeintiuno = UnAnio{
    numero = 2021,
    eventos = [crisis,(`agregarAtraccion` "Playa")]
}

veinteVeintiTres :: Anio
veinteVeintiTres = UnAnio{
    numero = 2023,
    eventos = [crisis, (`agregarAtraccion` "Parque"), (`remodelacion` 10), (`remodelacion` 20)]
}


aplicarCiudadFuncion :: Ciudad -> (Ciudad -> Ciudad) -> Ciudad
aplicarCiudadFuncion ciudad funcion = funcion ciudad

reflejarPasoAnio :: Ciudad -> Anio -> Ciudad
reflejarPasoAnio ciudad anio = foldl aplicarCiudadFuncion ciudad (eventos anio)

-- > reflejarPasoAnio azul dosMilVeintidos 
-- UnaCiudad
--     { nombre = "New Azul"     
--     , anioFundacion = 1832    
--     , atraccionesPrincipales =
--         [ "Teatro Español"    
--         , "Parque Municipal Sarmiento"
--         ]
--     , costoDeVida = 197.505
--     }

-- > reflejarPasoAnio azul dosMilQuince   
-- UnaCiudad
--     { nombre = "Azul"
--     , anioFundacion = 1832
--     , atraccionesPrincipales =
--         [ "Teatro Español"
--         , "Parque Municipal Sarmiento"
--         , "Costanera Cacique Catriel"
--         ]
--     , costoDeVida = 190
--     }

-- Punto 4.2

criterioCostoDeVida :: Ciudad -> Ciudad -> Bool
criterioCostoDeVida ciudadConEvento ciudadSinEvento = costoDeVida ciudadConEvento > costoDeVida ciudadSinEvento

criterioCantAtracciones :: Ciudad -> Ciudad -> Bool
criterioCantAtracciones ciudadConEvento ciudadSinEvento = (length . atraccionesPrincipales) ciudadConEvento > (length . atraccionesPrincipales) ciudadSinEvento

ciudadRespectoCriterio :: Ciudad -> (Ciudad -> Ciudad -> Bool) -> (Ciudad -> Ciudad) -> Bool
ciudadRespectoCriterio ciudad criterio evento = criterio (evento ciudad) ciudad

-- > ciudadRespectoCriterio azul criterioCostoDeVida crisis
-- False

-- > ciudadRespectoCriterio azul criterioCostoDeVida (`agregarAtraccion` "Monasterio Trapense") 
-- True

-- ciudadRespectoCriterio azul criterioCantAtracciones (`agregarAtraccion` "Monasterio Trapense")
-- True

-- Punto 4.3

modificarEventosAnioSuba :: [Ciudad -> Ciudad] -> Ciudad -> [Ciudad -> Ciudad]
modificarEventosAnioSuba eventos ciudad = [x | x <- eventos, criterioCostoDeVida (x ciudad) ciudad ]

subirPorCriterio :: Ciudad -> Anio -> ([Ciudad -> Ciudad] -> Ciudad -> [Ciudad -> Ciudad]) -> Ciudad
subirPorCriterio ciudad anio criterio = foldl aplicarCiudadFuncion ciudad (criterio (eventos anio) ciudad) 
-- Funcion que se utiliza en los puntos 4.3, 4.4 y 4.5 para evitar repetir codigo
-- Se le debe pasar el parametro del criterio (modificarEventosAnioSuba, modificarEventosAnioBaje, modificarEventosValorSuba)

-- > subirPorCriterio azul dosMilVeintidos modificarEventosAnioSuba
-- UnaCiudad
--     { nombre = "New Azul"
--     , anioFundacion = 1832
--     , atraccionesPrincipales =
--         [ "Teatro Español"
--         , "Parque Municipal Sarmiento"
--         , "Costanera Cacique Catriel"
--         ]
--     , costoDeVida = 219.45
--     }

-- Punto 4.4

modificarEventosAnioBaje :: [Ciudad -> Ciudad] -> Ciudad -> [Ciudad -> Ciudad]
modificarEventosAnioBaje eventos ciudad = [x | x <- eventos, not (criterioCostoDeVida (x ciudad) ciudad)] 

-- > subirPorCriterio azul dosMilVeintidos modificarEventosAnioBaje    
-- UnaCiudad
--     { nombre = "Azul"
--     , anioFundacion = 1832
--     , atraccionesPrincipales =
--         [ "Teatro Español"
--         , "Parque Municipal Sarmiento"
--         ]
--     , costoDeVida = 171
--     }

-- Punto 4.5
modificarEventosValorSuba :: [Ciudad -> Ciudad] -> Ciudad -> [Ciudad -> Ciudad]
modificarEventosValorSuba eventos ciudad = [x | x <- eventos, valorDeCiudad (x ciudad) > valorDeCiudad ciudad]

-- > subirPorCriterio nullish dosMilVeintidos modificarEventosValorSuba 
-- UnaCiudad
--     { nombre = "New Nullish"
--     , anioFundacion = 1800
--     , atraccionesPrincipales = []
--     , costoDeVida = 147
--     }

-- El enunciado dice 161.7, pero en realidad solo se aplica la funcion de remodelacion 5. 

-- Punto 5

-- parte 5.1

compararCostoDeVida :: Ciudad -> Ciudad-> (Ciudad -> Ciudad) -> (Ciudad -> Ciudad) -> Bool
compararCostoDeVida ciudad1 ciudad2 evento1 evento2 = (costoDeVida . evento1) ciudad1 <= (costoDeVida . evento2) ciudad2
-- Utilizada para no repetir codigo en el punto 5.1 y 5.2

eventoOrdenado :: Anio ->  Ciudad -> Bool 
eventoOrdenado UnAnio{eventos = []} _ = False
eventoOrdenado UnAnio{eventos = [evento]} _ = True
eventoOrdenado UnAnio{eventos = (primerEvento:segundoEvento:xs)} ciudad = compararCostoDeVida ciudad ciudad primerEvento segundoEvento && eventoOrdenado UnAnio{eventos = segundoEvento:xs} ciudad

{-
> eventoOrdenado dosMilVeintidos azul
True
> eventoOrdenado veinteVeintiTres azul
False
-}

-- parte 5.2

ciudadesOrdenadas :: (Ciudad -> Ciudad) -> [Ciudad] -> Bool
ciudadesOrdenadas _ [] = False
ciudadesOrdenadas _ [ciudad] = True
ciudadesOrdenadas evento (primerCiudad:segundaCiudad:xs) = compararCostoDeVida primerCiudad segundaCiudad evento evento && ciudadesOrdenadas evento (segundaCiudad:xs)

{-
> ciudadesOrdenadas (`remodelacion` 10) [caletaOlivia, nullish, baradero, azul] 
True
> ciudadesOrdenadas (`remodelacion` 10) [caletaOlivia, azul , baradero]        
False
-}

-- parte 5.3

compararAnios :: Anio -> Anio -> Ciudad -> Bool
compararAnios primerAnio segundoAnio ciudad = costoDeVida (reflejarPasoAnio ciudad primerAnio) <= costoDeVida (reflejarPasoAnio ciudad segundoAnio)

aniosOrdenados :: [Anio] -> Ciudad  -> Bool
aniosOrdenados [] _ = False
aniosOrdenados [anio] _ = True
aniosOrdenados (primerAnio:segundoAnio:xs) ciudad = compararAnios primerAnio segundoAnio ciudad && aniosOrdenados (segundoAnio:xs) ciudad

{-
> aniosOrdenados [dosMilVeintiuno,dosMilVeintidos,veinteVeintiTres] baradero
False
> aniosOrdenados [dosMilVeintidos,dosMilVeintiuno,veinteVeintiTres] baradero
True
-}

-- Punto 6

dosMilVeinticuatro :: Anio
dosMilVeinticuatro = UnAnio{
    numero = 2024,
    eventos = [crisis, (`reevaluacion` 7), (`remodelacion` 1), (`remodelacion` 2), (`remodelacion` 3)] --Sigue infinitamente (`remodelacion` 3++)
}

{-  -- Parte 6.1 eventos Ordenados

    La funcion eventosOrdenados puede tener un resultado si al aplicar la reevaluacion, el costo de vida queda siendo mayor que al aplicar la primera remodelacion. 
    En caso de que esto no se cumpla, la funcion no terminaria nunca, por ende no se obtendria nunca un resultado.


    -- Parte 6.2 anios ordenados

    Hay diferentes resultados dependiendo de los elementos pasados en la lista:
    - En el caso que se pase solo el anio 2024, el resultado sera True ya que solo tiene un elemento en la lista.
        Esto se logra gracias a que en la definicion de la funcion utilizamos pattern matching, lo que permite evaluar distintos casos de invocacion, segun los parametros
        pasados a la funcion.

    - En el caso que se pase una lista de anios en el que los dos primeros anios a comparar resulte en que no estan ordenados, la funcion terminaria resultando en False. 
        Esto ocurre por las caracteristicas que brinda haskell de evaluacion diferida, en este caso con el "&&": False && (cualquier valor booleano) = False.

    - En el caso que se pase una lista de anios en el que los anios previos al 2024 esten ordenados, la funcion no terminaria de aplicar los eventos.
        Esto ocurre debido a que utilizamos la funcion de orden superior "foldl", la cual es recursiva y no cuenta con una condicion de fin y por ende no se puede utilizar
        la caracteristica de evaluacion diferida en listas infinitas que provee haskell.

    - En el caso que se pase una lista y el primer o segundo anio sea el 2024, tampoco tendra resultado por la misma razon anterior.
-}
