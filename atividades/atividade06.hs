atividade = "06"
nome = "Kauan Pablo de Sousa Silva"
matricula = "556027"

-- 01
-- crie uma função que determine se uma string é anagrama de outra
-- Verifica se duas strings são anagramas uma da outra
anagrama :: [Char] -> [Char] -> Bool
anagrama s1 s2 = sortString s1 == sortString s2

-- Função para ordenar uma string ignorando espaços usando selectionSort
sortString :: [Char] -> [Char]
sortString xs = selectionSort [x | x <- xs, x /= ' ']

-- Função de ordenação por seleção (Selection Sort)
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = let x = maximum xs in x : selectionSort (remove x xs)
    where
        remove _ [] = []
        remove x (y : ys)
            | x == y = ys
            | otherwise = y : remove x ys

-- 02
-- construa função que elimine repetções de uma dada string s
--  sem alterar a sequência original 
-- dos caracteres de s.
unique :: [Char] -> [Char]
unique [] = ""
unique (x:xs) = x : unique (filter (/= x) xs)

-- teste
-- $ unique "aabbxa" 
-- $ "abx"

-- 03
-- implemente uma função que determine a string formada pelos 
-- caracteres comuns a duas strins de entrada a e b. A saida não 
-- deve ter duplicadas.
intersec :: [Char] -> [Char] -> [Char] 
intersec a b = unique [x | x <- a, y <- b, x == y]

-- teste
-- $ intersec "abcd" "cdef"
-- $ "cd"

-- 04
-- dado três listas zipálas numa lista de triplas de forma 
-- semelhante ao comando zip. 
zip'linha :: [a] -> [b] -> [c] -> [(a,b,c)]
zip'linha [] _ _ = []
zip'linha _ [] _ = []
zip'linha _ _ [] = []
zip'linha (x:xs) (y:ys) (z:zs) = (x, y, z) : zip'linha xs ys zs

-- teste 01
-- zip'linha
-- $ [1,2,3] "abc" [TRUE,FALSE,TRUE] 
-- $ [(1,"a", TRUE), (2, "b", FALSE), (3, "c", TRUE)] 
-- teste 02
-- $ zip'linha [1,2,3,4] "abc" [TRUE] 
-- $ [(1,"a",TRUE)]
