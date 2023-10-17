{-
    Ejercicios adicionales
    
    Estos son partes de la entrega de la tarea 2 (HW-2) el vienes 20 a la medianoche

-}

-- 1.
-- Box-Muller transform!
-- Ya saben: https://en.wikipedia.org/wiki/Box-Muller_transform

box'muller :: Double -> Double -> (Double, Double)
box'muller u1 u2 = (0.0, 0.0) -- replace with your magic here


-- 2.
-- Logaritmos!

{-
    Notación ... porque no hay LaTeX aqui: logaritmo[b]x se lee "logaritmo de x en base b"

    (a) Implementar 'logz' (logaritmo generalizado) usando la función predefinida 'log'.
    La función logz computa logaritmos en una base arbitraria, pasada como argumento.
    Si no saben cómo hacer un cambio de base, vean Wikipedia

    Noten que la firma de la función se presta a curryficación.
    Y los argumentos no son intercambiables logaritmo[10]2 != logaritmo[2]10
    El orden importa. Tenemos dos posibilidades:

    logz x b = ...
    logz b x = ...

    Implementen una de las dos posibilidades. Escojan la que consideren mas útil.

    (b) Justifiquen su selección, aqui mismo:

    (c) Agreguen 2 a 4 ejemplos demostrando aplicaciones parciales de logz.
    Un ejemplo debe instanciar una variable v con la aplicación parcial y usarla.
    Otro debe pasar la aplicación parcial como argumento en una llamada.

-}

logz :: Double -> Double -> Double
logz _ _ = (0, 0)


-- 3.
-- Conversión de coordenadas

{-

    Ver https://en.wikipedia.org/wiki/Polar_coordinate_system
    La sección relevante es "Converting between polar and Cartesian coordinates"

    Implemente estas dos funciones, indicando su firma explicitamente

    -- polarFromCartesian: convierte coordenadas cartesianas a polares
    polarFromCartesian :: ____

    -- cartesianFromPolar: convierte coordenadas polares a cartesianas
    cartesianFromPolar :: ____

-}

-- your magic here ...
