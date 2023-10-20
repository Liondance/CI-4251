{-
  generador de numbres :D
-}

nombres = [ "Alicia", "Betty", "Carlos", "David" ]

apellidos = [ "Pimentel", "Rodriguez", "Santana" ]

lista = [ n ++ " " ++ a | n <- nombres, a <- apellidos ]
