
main :: IO()
main = do
    let animal1 = Animal 29 "Elefante" [ "Volar"]
    print (noTanCuerdo animal1)


data Animal = Animal {coeficiente :: Float , especie :: String , capacidades :: [String]} deriving(Show,Eq)

inteligenciaSuperior :: Animal -> Float -> Animal
inteligenciaSuperior animal n = animal {coeficiente = coeficiente animal + n}

pinkificar :: Animal -> Animal
pinkificar (Animal c e cap) = Animal c e []

superpoderes :: Animal -> [String] -> Animal
superpoderes (Animal iq e c) habilidades
    | e == "elefante" = Animal iq e (c++habilidades++["no tener miedo a los ratones"])
    | (e=="raton") && (iq>100) = Animal iq e (c++habilidades++["Hablar"])
    | otherwise  = Animal iq e (c++habilidades)


noTanCuerdo :: Animal -> Bool
noTanCuerdo (Animal _ _ c) = length ([x| x <- c, esPinkiesco x ]) >= 2

esPinkiesco :: String -> Bool
esPinkiesco x = x `elem` ["Hablar", "Saltar", "Volar"]

data Experimento = Experimento {transformacion :: [a -> Animal], criterioExito :: Animal -> Bool}

experimentoExitoso :: Experimento -> Animal ->Bool
experimentoExitoso (Experimento t c) a = c (tranformacionF t a)

transformacionF :: [Animal -> Animal] -> Animal -> Animal
transformacionF [] a = a
transformacionF (x:xs) a = transformacion xs (x a)
