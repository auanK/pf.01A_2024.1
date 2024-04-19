atividade = "05"

nome = "Kauan Pablo de Sousa Silva"

matricula = "556027"

-- 1
-- Determinar se um ano é ou não bissexto
bis :: Int -> Bool
bis n = (mod n 4 == 0 && mod n 100 /= 0) || mod n 400 == 0

-- 2
-- Converter de uma escala de temperatura para outra. Há três argumentos de entrada.
-- O primeiro é o valor numérico da temperatura, o segundo representa a escala desse
-- valor e o terceiro a escala do valor oriundo da conversão . Tais unidades são
-- representadas por caracteres onde 'C' simboliza Célcios, 'F'   Fahrenheit e 'K'
-- Kelvin.
temp :: Float -> Char -> Char -> Float
temp t fr to
  | fr == 'C' && to == 'F' = t * 1.8 + 32
  | fr == 'C' && to == 'K' = t + 273.15
  | fr == 'F' && to == 'C' = (t - 32) / 1.8
  | fr == 'F' && to == 'K' = (t - 32) / 1.8 + 273.15
  | fr == 'K' && to == 'C' = t - 273.15
  | fr == 'K' && to == 'F' = (t - 273.15) * 1.8 + 32
  | otherwise = t

-- 3or) . A pontuação total da rodada é dada pela soma 
-- dos valores cujos caracteres e
-- Num dado jogo, fichas são rotuladas com letras.  uma rodada pode ser representada
-- por uma String  onde cada letra é uma ficha jogada. Um computador gera uma lista
-- de duplas  do tipo (letra, valstão na lista. 

coin :: String -> [(Char, Float)] -> Float
coin s m = sum [v | c <- s, (c', v) <- m, c == c']

-- coin "laura" [('l',1), ('a',3), ('u', 4), ('r', 5)]
