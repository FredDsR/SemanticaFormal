-- Exercicio 1
palindromo :: String -> Bool
palindromo s = s == reverse s

-- Exercicio 2
verificaTriangulo :: Int -> Int -> Int -> Bool
verificaTriangulo a b c
    | a + b <= c = False
    | a + c <= b = False
    | b + c <= a = False
    | otherwise  = True

-- Exercicio 3
sinal :: Int -> Int
sinal x
    | x > 0     = 1
    | x == 0    = 0
    | otherwise = -1

-- Exercicio 4
menorTres :: Int -> Int -> Int -> Int
menorTres x y z
    | x < y && x < z = x
    | y < x && y < z = y
    | otherwise      = z

-- Exercico 5
potencia :: Int -> Int -> Int
potencia b e
    | e == 0 = 1
    | otherwise = b * potencia b (e - 1)