atividade = "1"

-- Substitua seus dados
nome = "Kauan Pablo"

matricula = "556027"

-- 1
-- Recebe uma string e
-- retorna-a sem as vogais.
noVog :: String -> String
noVog s = [x | x <- s, x `notElem` "aeiouAEIOU"]

-- 2
-- retorna quantas vezes x é divisível por n
num'divs :: Int -> Int -> Int
num'divs x n | x `mod` n == 0 = 1 + num'divs (x `div` n) n | otherwise = 0

-- 3
-- Dado um inteiro n. determinar se
-- ele é ou não um número primo (use o numero de divisores).
is'prime :: Int -> Bool
is'prime n = null ([x | x <- [x | x <- [2 .. n], x * x <= n], n `mod` x == 0])

-- 4
-- inverte um inteiro, por exemplo
-- o inverso de 251 é 152.
int'inv :: Int -> Int
int'inv x = read (reverse (show x)) :: Int