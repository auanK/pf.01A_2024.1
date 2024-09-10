-- Atividade 08
-- Nome: Kauan Pablo de Sousa Silva
-- Matrícula: 556027

-- Árvore binária de busca (BST)
data BST a
  = Null
  | Node a (BST a) (BST a)
  deriving (Show, Eq)

-- Questão 1: Retorna a versão parentizada de uma BST dada
parentize :: BST Int -> String
parentize Null = ""
parentize (Node value left right) =
  "(" ++ show value ++ parentize left ++ parentize right ++ ")"

-- EXEMPLO: da questão 01.
-- Se você inserir os valores 14, 7, 20, 9, 1, 10 na BST, a representação parentizada seria: "(14 (7 (1) (9 (10))) (20))"

-- Questão 2: Constrói uma BST a partir da inserção consecutiva das chaves de uma lista de inteiros dada
addFromVec :: [Int] -> BST Int
addFromVec = foldl (flip insert) Null

insert :: Int -> BST Int -> BST Int
insert x Null = Node x Null Null
insert x (Node value left right)
  | x < value = Node value (insert x left) right
  | x > value = Node value left (insert x right)
  | otherwise = Node value left right

-- Questão 3: Calcula a altura de uma árvore binária de entrada
height :: BST a -> Int
height Null = 0
height (Node _ left right) = 1 + max (height left) (height right)