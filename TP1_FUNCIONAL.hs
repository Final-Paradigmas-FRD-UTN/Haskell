{- EJERCICIOS TP 1 FUNCIONAL > https://docs.google.com/document/d/1qPONe3Ow8gKfrkX9E2Atd1q78KQE29WK_EZZIgKm2tE/edit#heading=h.h1gxkxzf5qan -}


{-  Definir la función fahrToCelsius, que a partir de una cantidad de grados en 
    escala Fahrenheit, devuelve el equivalente en escala Celsius.-}

fahrToCelsius :: Float -> Float
fahrToCelsius f = (f - 32) * 5 / 9

{-  Definir, usando composición, la función haceFrioF, que indica si una temperatura 
    expresada en grados Fahrenheit es fría. Decimos que hace frío si 
    la temperatura es menor a 8 grados Celsius. -}

haceFrioF :: Float -> Bool
haceFrioF = (< 8) . fahrToCelsius

{-  Trabajamos con tres enteros que representan el nivel de un río en tres días consecutivos. 
    Por ejemplo: medí los días 1, 2 y 3, las mediciones son: 22 cm, 283 cm, y 294 cm.
    Definir la función dispersión, que toma los tres valores y devuelve la diferencia entre el más alto 
    y el más bajo.-}

dispersion :: Int -> Int -> Int -> Int
dispersion x y z = maximum [x, y, z] - minimum [x, y, z]

{-  En una plantación de pinos, de cada árbol se conoce la altura expresada en metros. 
    El peso de un pino se puede calcular a partir de la altura así:
    3 kg x cm hasta 3 metros,
    2 kg x cm arriba de los 3 metros.
    Por ejemplo: 2 metros -> 600 kg, 5 metros -> 1300 kg 
    (porque los primeros 3 metros pesan 900 kg y los siguientes 2 pesan los 400 restantes).
    Los pinos se usan para llevarlos a una fábrica de muebles, 
    a la que le sirven árboles de entre 400 y 1000 kilos, 
    un pino fuera de este rango no le sirve a la fábrica. -}

{- Definir la función pesoPino, recibe la altura de un pino y devuelve su peso. -}

pesoPino :: Float -> Int
pesoPino altura
    | altura <= 3 = round $ altura * 300
    | otherwise   = 900 + round ((altura - 3) * 200)

{- Definir la función esPesoUtil, recibe un peso en kg y responde si un pino 
de ese peso le sirve a la fábrica -}

esPesoUtil :: Int -> Bool
esPesoUtil peso = peso >= 400 && peso <= 1000

{- Definir la función sirvePino, recibe la altura de un pino y responde si un pino 
de ese peso le sirve a la fábrica. Definir sirvePino usando composición -}

sirvePino :: Float -> Bool
sirvePino = esPesoUtil . pesoPino
