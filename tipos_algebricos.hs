-- Tipos Algébricos Haskell

data Temperatura = Frio | Calor
    deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno | Primavera
    deriving(Eq,Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

data Forma = Circulo Float | Retangulo Float Float
    deriving(Eq,Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a

data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq,Show)

arv :: Arvore
arv = Nodo 3 (Folha 1) (Nodo 2 (Folha 4) (Folha 5))

somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha (n*2)
multDoisArvore (Nodo n a1 a2) = Nodo (n*2) (multDoisArvore a1) (multDoisArvore a2)

-- Exercicios Tipos Algébricos

-- Execicio 1
multArvore :: Int -> Arvore -> Arvore
multArvore x (Folha n) = Folha (n*x)
multArvore x (Nodo n a1 a2) = Nodo (n*x) (multArvore x a1) (multArvore x a2)

-- Exercicio 2
contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) = (contaFolhas a1) + (contaFolhas a2)

-- Exercicio 3
contaNodos :: Arvore -> Int
contaNodos (Folha n) = 0
contaNodos (Nodo n a1 a2) = 1 + (contaNodos a1) + (contaNodos a2)

-- Exercicio 4
quantasVezes :: Int -> Arvore -> Int
quantasVezes x (Folha n)
    | n == x = 1
    | otherwise = 0
quantasVezes x (Nodo n a1 a2)
    | n == x = 1 + quantasVezes x a1 + quantasVezes x a2
    | otherwise = quantasVezes x a1 + quantasVezes x a2

-- Exercicio 5
maxArvore :: Arvore -> Int
maxArvore (Folha n) = n
maxArvore (Nodo n a1 a2) = max n (max (maxArvore a1) (maxArvore a2))

-- Exercicio 6
refleteArvore :: Arvore -> Arvore
refleteArvore (Folha n) = Folha n
refleteArvore (Nodo n a1 a2) = Nodo n (refleteArvore a2) (refleteArvore a1)

-- Exercicio 7
geraLista :: Arvore -> [Int]
geraLista (Folha n) = n : []
geraLista (Nodo n a1 a2) = n : geraLista a1 ++ geraLista a2