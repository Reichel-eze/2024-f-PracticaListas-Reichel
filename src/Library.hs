module Library where
import PdePreludat

data Carta = UnaCarta {
    nombre :: String,
    velocidad :: Number,
    altura :: Number,
    peso :: Number,
    fuerza :: Number,
    peleas :: Number,
    tags :: [String]    -- tiene una lista de string, de tags
}deriving(Show, Eq)

-- Instaciamos algunos superheroes

batman :: Carta
batman = UnaCarta {
    nombre = "batman",
    velocidad = 100,
    altura = 100,
    peso = 80,
    fuerza = 100,
    peleas = 100,
    tags = ["batigenial"]
}

superman :: Carta
superman = UnaCarta {
    nombre = "superman",
    velocidad = 100,
    altura = 100,
    peso = 80,
    fuerza = 100,
    peleas = 100,
    tags = ["superfantastico"]
}

ganadoraSegun :: (Carta -> Number) -> Carta -> Carta -> Carta
ganadoraSegun criterio carta1 carta2
    | criterio carta1 > criterio carta2 = carta1
    | otherwise                         = carta2

ganadoraSegunVelocidad :: Carta -> Carta -> Carta
ganadoraSegunVelocidad = ganadoraSegun velocidad    

ganadoraSegunAltura :: Carta -> Carta -> Carta
ganadoraSegunAltura = ganadoraSegun altura

-- con lambda

ganadoraSegunIMC :: Carta -> Carta -> Carta
ganadoraSegunIMC = ganadoraSegun (\carta -> peso carta `div` altura carta ^ 2)

-- Quiero el nombre de cada carta
nombres :: [Carta] -> [String]
nombres = map nombre

-- Quiero la fuerza de cada carta
fuerzas :: [Carta] -> [Number]
fuerzas = map fuerza

-- Quiero la longitud del nombre de cada carta
longitudNombres :: [Carta] -> [Number]
longitudNombres = map (length . nombre) -- 1ero obtengo el nombre y luego pido su tamaño

-- Quiero las cartas de superheroes nuevos
superheroesNuevos :: [Carta] -> [Carta]
superheroesNuevos = filter ( (== 0) . peleas)

-- Quiero las cartas cuyos nombres tiene 'X'
superheroesConX :: [Carta] -> [Carta]
superheroesConX = filter (elem 'X' . nombre) 

-- Quiero las cartas con mas peso que altura
pesadas :: [Carta] -> [Carta]
pesadas = filter (\carta -> peso carta > altura carta) 

-- Quiero saber si hay cartas de superheroes nuevo (OJO, NO es igual)
hayNuevos :: [Carta] -> Bool
hayNuevos = any ((== 0) . peleas)

-- Quiero saber si todos los nombres tienen 'X' (OJO, NO es igual)
todosConX :: [Carta] -> Bool
todosConX = all (elem 'X' . nombre)

-- Quiero saber si existen cartas con mas peso que altura
hayPesadas :: [Carta] -> Bool
hayPesadas = any (\carta -> peso carta > altura carta)

-- Quiero saber el total de peleas ganadas
peleasTotales :: [Carta] -> Number
peleasTotales cartas = (sum . map peleas) cartas

-- Quiero todos los nombres en un string intercalados con ";"
nombresSeparados :: [Carta] -> String
nombresSeparados cartas = concatMap ((++";") . nombre) cartas

-- Quiero la carta que tenga la mayor fuerza (OJO quiero la carta)
masFuerte :: [Carta] -> Carta
masFuerte cartas = foldl1 (ganadoraSegun fuerza) cartas

-- 1. Extender las cartas para incluir tags y definir funciones para cambiarlos

-- agrego un elemento a una lista con los ":"
ponerTag :: String -> Carta -> Carta
ponerTag tag carta = carta {tags = tag : tags carta}

-- hago un filtro de aquellos tags que no sean el que paso (es decir una nueva lista sin el tag que pase por parametro)
quitarTag :: String -> Carta -> Carta
quitarTag tag carta = carta {tags = filter (/= tag) (tags carta)}

-- 2. Dado un mazo de cartas
-- a. Obtener los nombres de las cartas que empiecen con "bat"

-- 1ero. Obtengo los nombres de las cartas (hago un map)
-- 2dos. Realizo un filtro y me quedo con los que son bati (hago un filter)

-- PARA PENSAR y poder entender el caminito
-- nombre        :: (Carta -> String)
-- empiezaConBat :: (String -> Bool)

batiNombres :: [Carta] -> [String]
batiNombres cartas = (filter (empiezaConBat) . map (nombre)) cartas

batiNombres' :: [Carta] -> [String]
batiNombres' cartas = (filter ((== "bat") . take 3) . map (nombre)) cartas

empiezaConBat :: String -> Bool
empiezaConBat =  (== "bat") . take 3

-- b. Averiguar si hay cartas con todos los tags demasiado largos
-- tag largo --> aquel que tiene mas de 10 caracteres

-- 1ero. Obtengo 

-- PARA PENSAR y poder entender el caminito
-- tags             :: (Carta -> [String])
-- all              :: ((String -> Bool) -> [String] -> Bool)
-- length           :: (String -> Number)

--hayCartasConTodosLosTagsMuyLargos :: [Carta] -> Bool
--hayCartasConTodosLosTagsMuyLargos cartas = (any (\tag -> length tag > 10) . tags) cartas

hayCartasConTodosLosTagsMuyLargos' :: [Carta] -> Bool
hayCartasConTodosLosTagsMuyLargos' cartas = (any (all ((>10) . length) . tags)) cartas 


-- c. Corregir las cartas a las que le pusieron el tag #alguien en lugar de #alien