-- Atividade 07

atividade = "07"

nome = "Kauan Pablo de Sousa Silva"

matricula = "556027"

data Stack a = Empty | Value a (Stack a) deriving (Show)

-- Empurra um elemento para a pilha
push :: Stack a -> a -> Stack a
push stack x = Value x stack

-- Remove o elemento do topo da pilha
pop :: Stack a -> Stack a
pop Empty = Empty
pop (Value _ rest) = rest

-- Obtém o elemento do topo da pilha, se existir
top :: Stack a -> Maybe a
top Empty = Nothing
top (Value x _) = Just x

-- Verifica se a pilha está vazia
is'empty :: Stack a -> Bool
is'empty Empty = True
is'empty _ = False

-- Determina a precedência dos operadores
precedence :: Char -> Int
precedence '^' = 3
precedence '*' = 2
precedence '/' = 2
precedence '+' = 1
precedence '-' = 1
precedence _ = 0

-- Verifica se um caractere é um operador
is'operator :: Char -> Bool
is'operator x = x `elem` ['+', '-', '*', '/', '^']

-- Converte uma expressão infixa para notação polonesa inversa (postfix)
pos'fixa :: [Char] -> [Char]
pos'fixa = pos'fixa' [] Empty

-- Processa a expressão infixa para gerar a notação postfix
pos'fixa' :: [Char] -> Stack Char -> [Char] -> [Char]
pos'fixa' output stack [] = output ++ pop'all'operators stack
pos'fixa' output stack (x : xs)
  | x == '(' = pos'fixa' output (push stack x) xs
  | x == ')' = pos'fixa' (output ++ pop'till'open'paren stack) (pop'till'open'paren' stack) xs
  | is'operator x = pos'fixa' (output ++ pop'operators stack x) (push'operators stack x) xs
  | otherwise = pos'fixa' (output ++ [x]) stack xs

-- Remove operadores da pilha até encontrar um parêntese de abertura
pop'till'open'paren :: Stack Char -> [Char]
pop'till'open'paren Empty = []
pop'till'open'paren (Value '(' _) = []
pop'till'open'paren (Value x rest) = x : pop'till'open'paren rest

-- Remove os parênteses de abertura da pilha
pop'till'open'paren' :: Stack Char -> Stack Char
pop'till'open'paren' Empty = Empty
pop'till'open'paren' (Value '(' rest) = rest
pop'till'open'paren' (Value _ rest) = pop'till'open'paren' rest

-- Remove operadores da pilha com base na precedência
pop'operators :: Stack Char -> Char -> [Char]
pop'operators Empty _ = []
pop'operators stack@(Value t rest) x
  | is'operator t && precedence t >= precedence x = t : pop'operators rest x
  | otherwise = []

-- Empurra operadores na pilha e remove operadores com menor ou igual precedência
push'operators :: Stack Char -> Char -> Stack Char
push'operators stack x = push (pop'operators' stack x) x

-- Remove operadores da pilha com menor ou igual precedência
pop'operators' :: Stack Char -> Char -> Stack Char
pop'operators' Empty _ = Empty
pop'operators' stack@(Value t rest) x
  | is'operator t && precedence t >= precedence x = pop'operators' rest x
  | otherwise = stack

-- Remove todos os operadores da pilha
pop'all'operators :: Stack Char -> [Char]
pop'all'operators Empty = []
pop'all'operators (Value t rest) = t : pop'all'operators rest
