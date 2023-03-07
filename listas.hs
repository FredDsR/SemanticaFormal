-- Exercicio 1
multLista :: Int -> [Int] -> [Int]
multLista x (head:body)
    | body == [] = x * head : []
    | otherwise  = x * head : multLista x body

-- Exercicio 2
elemento :: Int -> [Int] -> Bool
elemento x (head:body)
    | x == head  = True
    | body == [] = False
    | otherwise  = elemento x body

-- Exercicio 3
conta :: Int -> [Int] -> Int
conta x (head:body)
    | body == [] && x == head = 1
    | body == [] && x /= head = 0
    | x /= head  = conta x body
    | otherwise  = 1 + conta x body

-- Exercicio 4
contaMaiores :: Int -> [Int] -> Int
contaMaiores x (head:body)
    | body == [] && head > x  = 1
    | body == [] && head <= x = 0
    | head > x                = 1 + contaMaiores x body
    | otherwise               = contaMaiores x body

-- Exercicio 5
maiores :: Int -> [Int] -> [Int]
maiores x (head:body)
    | body == [] && head > x  = head : []
    | body == [] && head <= x = []
    | head > x                = head : maiores x body
    | otherwise               = maiores x body

-- Exercicio 6
geraLista :: Int -> Int -> [Int]
geraLista x n
    | x == 0    = []
    | otherwise = n : geraLista (x - 1) n 

-- Exercicio 7
addFim :: Int -> [Int] -> [Int]
addFim n (head:body)
    | body == [] = head : n : []
    | otherwise  = head : addFim n body