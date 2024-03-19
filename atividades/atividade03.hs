atividade = "3"
nome = "Kauan Pablo de Sousa Silva"
matricula = "556027"

-- Função genérica que recebe uma lista de elementos e retorna uma lista de tuplas onde
-- o primeiro elemento da tupla é um elemento da lista e o segundo é a quantidade de
-- vezes que ele aparece na lista (Usada para resolver a 1ª e 2ª questão)

countOccurrences :: (Eq a) => [a] -> [(a, Int)]
countOccurrences [] = []
countOccurrences (x : xs) = (x, length (filter (== x) (x : xs))) : countOccurrences (filter (/= x) xs)

-- 1ª QUESTÃO
-- Recebe uma string s e retorna uma lista de tuplas (c, f) onde c é um caractere em s e
-- f é o total de vezes que c ocorre em s. Exemplo,
-- >> tls "aabcbaccc"
-- [('a', 3), ('b', 2), ('c', 4)]
-- OBS: a ordem das tuplas na lista de saída não é importante.

-- Função que recebe uma String e retorna a frequência  em que cada caractere aparece
tls :: String -> [(Char, Int)]
tls = countOccurrences


-- 2ª QUESTÃO
-- Recebe uma string s de entrada e retorna uma tupla do tipo (p,f) onde p é a palavra
-- mais frequente em s e f o valor de quantas vezes ela ocorre. Exemplo,
-- >> sfq "a casa. Ela casa. casa!
-- ("casa", 3)
-- OBS:  Note que tokens como ponto e exclamação adjacentes as palavras não devem
-- interferir na contagem.

-- Função que recebe uma string e retorna outra string sem caracteres especiais
removeSpecialChars :: String -> String
removeSpecialChars = filter (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " ")

-- Função que recebe uma lista de tuplas (String, Int) e retorna a tupla com o maior valor
-- (Parecido com a função maximumBy da biblioteca Data.List)
maximumBy_ :: [(String, Int)] -> (String, Int)
maximumBy_ [] = error ""
maximumBy_ (x : xs) = foldl (\acc@(a, b) (c, d) -> if b > d then acc else (c, d)) x xs

-- Função que recebe uma string e retorna uma tupla com a palavra mais frequente e a
-- quantidade de vezes que ela aparece na string
sfq :: String -> (String, Int)
sfq [] = ("", 0)
sfq str = maximumBy_ (countOccurrences (words (removeSpecialChars str)))