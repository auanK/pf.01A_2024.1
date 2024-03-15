atividade = "2"

-- Substitua seus dados
nome = "Kauan Pablo"

matricula = "556027"

-- 1 
-- Sejam as tuplas u e v de inteiros
-- tal que exista um inteiro k onde
-- u = kv ou v = ku  
-- então u e v são mútiplos. Construa 
-- função que determine se duas 
-- tuplas de inteiros  são múltiplas.
isMult :: (Int,Int) -> (Int, Int) -> Bool
isMult (u1, u2) (v1, v2) = (u1 `mod` v1 == 0 && u2 `mod` v2 == 0) || (v1 `mod` u1 == 0 && v2 `mod` u2 == 0)

-- 2
-- Sejam todos os triângulos retângulos
-- de perímetro p e de lados inteiros.
--   representados por tuplas (a,b,c)
-- com  a>=b>=c. Criar
--  função que determine
-- o total destes triângulos dado p .

tot'tri :: Int -> Int
tot'tri p = length [(a, b, c) | a <- [1 .. p], b <- [1 .. a], c <- [1 .. b], a ^ 2 == b ^ 2 + c ^ 2, a + b + c == p]
