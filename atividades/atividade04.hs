atividade = "04"
nome = "Kauan Pablo de Sousa Silva"
matricula = "556027"

--  1

-- Substitue na steing "texi" todas as aparições de "from" pr "to" . Retorna a string de 
-- entrada com as devidas  substituições.

-- Exemplo
-- $ replace "a casa e o casarão foram reservados para o casamento do Acasio" "cas" "vel"
-- $ a vela e o velarão foram reservados para o velamento do Avelio

replace :: [Char] -> [Char] -> [Char] -> [Char]
replace [] _ _ = []
replace (x : xs) from to
    | take (length from) (x : xs) == from = to ++ replace (drop (length from) (x : xs)) from to
    | otherwise = x : replace xs from to
    
-- 2

-- identifica o maior valor n de uma lista de inteiros L e retorna a tupla (A,n,B) onde A 
-- é a sublista de L dos valores que precedem n e B a sublista de L que sucedem n.

-- Exemplo
-- $ lsSplit [1, 7, 11, 5, -2]
-- $ ([1,7], 11, [5, -2])

lsSplit :: [Int] -> ([Int], Int, [Int])
lsSplit [] = ([], 0, [])
lsSplit xs = let n = maximum xs in (takeWhile (/= n) xs, n, tail (dropWhile (/= n) xs))

-- 3
-- Ordena do maior para o menor  uma lista usando o algoritmo de ordenação por seleção.

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = let x = maximum xs in x : selectionSort (remove x xs)
    where
        remove _ [] = []
        remove x (y : ys)
            | x == y = ys
            | otherwise = y : remove x ys