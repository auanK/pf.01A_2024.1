-- Verifica se um número é par
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- Gera a sequência de Collatz para um número
collatzSequence :: Int -> [Int]
collatzSequence 1 = [1]
collatzSequence x
  | isEven x = x : collatzSequence (x `div` 2)
  | otherwise = x : collatzSequence (x * 3 + 1)

-- Encontra a maior sequência de Collatz de uma lista de tuplas
longestCollatzSequenceLength :: Int
longestCollatzSequenceLength = snd (findLongestTuple collatzLengths)

-- Encontra a tupla com o maior segundo elemento
findLongestTuple :: [(Int, Int)] -> (Int, Int)
findLongestTuple [] = error "Lista vazia"
findLongestTuple (x : xs) = foldl (\acc@(a, b) (c, d) -> if b > d then acc else (c, d)) x xs

-- Lista de tuplas (número, comprimento da sequência de Collatz)
collatzLengths :: [(Int, Int)]
collatzLengths = [collatzTuple x | x <- [1..1000]]

-- Cria uma tupla (número, comprimento da sequência de Collatz)
collatzTuple :: Int -> (Int, Int)
collatzTuple x = (x, length (collatzSequence x))
