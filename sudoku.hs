{- @file sudoku.hs
 *  @brief Sistema para solucionar tabuleiros de Sudoku
 *
 *  @author Ricardo Silva Moreira
 *  @author Hudson do Santos
 *
 *  @date Novembro de 2019
 -}

import Data.List
import Data.Char
import System.Directory


type Sudoku = [[Int]]

-- O código fonte "sudoku" em questão
sudoku :: Sudoku 

--Formato de Entrada:
--[[5,3,,,7,,,,,],[6,,,1,9,5,,,,],[,9,8,,,,,6,],[8,,,,6,,,,3],[4,,8,,3,,,1],[7,,,,2,,,,6],[,6,,,,,2,8,],[,,,4,1,9,,,5],[,,,,8,,,7,9]]

sudoku =[ [5,3,0,    0,7,0,    0,0,0],
          [6,0,0,    1,9,5,    0,0,0],
          [0,9,8,    0,0,0,    0,6,0],

          [8,0,0,    0,6,0,    0,0,3],
          [4,0,0,    8,0,3,    0,0,1],
          [7,0,0,    0,2,0,    0,0,6],

          [0,6,0,    0,0,0,    2,8,0],
          [0,0,0,    4,1,9,    0,0,5],
          [0,0,0,    0,8,0,    0,7,9]]
{-
  * Encontra um item no tabuleiro em uma determinada posição
  *
  * @param grade sudoku do qual extraímos o elemento
  * @param i número da linha no tabuleiro
  * @param j número da coluna no tabuleiro
  * @return do elemento da posição (i, j) no tabuleiro
 -}
at :: Sudoku-> Int -> Int -> Int
at grade i j = 
    if i < 0 || j < 0 || i >= length grade || j >= length (head grade) 
        then -1
    else
        (grade !! i) !! j

{-
 * Verifica se podemos salvar um número na linha sudoku específica, de acordo com as regras do jogo
  *
  * @param grade o sudoku específico
  * @param i número da linha no tabuleiro
  * @param val O valor que verificamos para ver se ele pode ser escrito na i-ésima linha
  * @retorn true se pudermos escrever val nessa ordem, false caso contrário
 -}
checaLinha :: Sudoku -> Int -> Int -> Bool
checaLinha grade i valor = not (elem valor (grade !! i))

{-
 * Verifica se podemos escrever um número nesta coluna Sudoku, de acordo com as regras do jogo
  *
  * @param grade o sudoku específicoi
  * número da coluna @param j no tabuleiro
  * @param valor o valor que verificamos para ver se ele pode ser gravado no j-ésimo coluna
  * @Return true se podemos escrever val nesta coluna, caso contrário false
 -}
checaColuna :: Sudoku -> Int -> Int -> Bool
checaColuna grade j valor = not (elem valor (transpose grade !! j))

{-
* Verifica se valor pode ser adicionado à caixa 3x3 específica, que é determinada pela posição (i, j) de acordo com as regras do jogo
  * O quadrado é onde o elemento da posição (i, j) está localizado
  *
  * @param grade o sudoku específico
  * @param i número da linha no tabuleiro
  * @param j número da coluna  no tabuleiro
  * @param valor o valor que verificamos para ver se ele pode ser escrito na caixa 3x3 definida pela posição (i, j)
  * @return true se pudermos escrever valor nesta caixa, false caso contrário
 -}
checaQuadrado :: Sudoku -> Int -> Int -> Int -> Bool
checaQuadrado grade i j valor = not (elem valor [at grade m n| m <- [0 .. 8], n <- [0 .. 8], div m 3 == div i 3, div n 3 == div j 3 ])

{-
 * Retorna um novo sudoku, mas o elemento de posição (i, j) se torna igual a novoValor
  *
  * @param i número da linha em (x: xs)
  * @param j número da coluna (x: xs)
  * @param novoValor novo valor do elemento do item (i, j)
  * @param (x: xs) entrada de sudoku 
  * @return (x: xs), mas o elemento de posição (i, j) se torna novoValor
 -}
atualizar :: Int -> Int -> Int -> Sudoku -> Sudoku
atualizar i j novoValor (x:xs)
    | (x:xs) == [] = []
    | i == 0 = atualizarN j novoValor x : xs
    | otherwise = x : atualizar (i-1) j novoValor xs
    where
        -- Toma posição n, valor novoValor e lista de números inteiros como argumento e retorna uma nova lista de números inteiros, mas o elemento da posição n é igual a novoValor
    atualizarN :: Int -> Int -> [Int] -> [Int]
    atualizarN n novoValor (x:xs)
        | (x:xs) == [] = []
        | n == 0 = novoValor:xs
        | otherwise = x : atualizarN (n-1) novoValor xs

{-
 * Encontra as células vazias do sudoku em particular
  *
  * @param grade o sudoku específico
  * @retorna uma lista de pares ordenados (i, j), onde (i, j) é a posição de uma célula vazia no tabuleiro
 -}
esvaziaCelula :: Sudoku -> [(Int,Int)]
esvaziaCelula grade = [(i,j) | i <- [0 .. 8], j <- [0 .. 8], at grade i j == 0]

{-
 * É aqui que o processo de solução do Sudoku começa.
  * Se não houver mais células vazias, a grade é uma solução válida de sudoku e retornamos uma lista com um único elemento da grade.
  * Caso contrário, o solucaoPossivel funciona com os argumentos da grade e a primeira posição de célula vazia no tabuleiro é chamada.
  * Dessa maneira, todos os preenchimentos possíveis são gerados por meio de solucaoPossivel e, se obtivermos um sudoku inválido, não o adicionaremos à solução.
  *
  * @param entrada de sudoku da grade
  * @return lista de todas as soluções de grade. Em particular, se não houver soluções - uma lista vazia.
 -}
solucionar :: Sudoku -> [Sudoku]
solucionar grade = 
    if esvaziaCelula grade == [] then [grade]
    else solucaoPossivel grade (head (esvaziaCelula grade))

{-
 * Preenche a célula vazia (i, j) com todos os valores possíveis obtidos a partir de valores possíveis
 * e continua com o preenchimento chamando o procedimento de resolução para cada valor possível.
 * Na parte inferior da recursão, se houver um sudoku válido - esta é uma solução.
 * Se, para qualquer etapa recebida pelo sudoku, possiveisValores ​​retornar uma lista vazia, esse é um sudoku inválido,
 * já que ainda temos vagas vazias
 *
 * entrada de sudoku da grade @param
 * @param (i, j) a posição da célula vazia específica no tabuleiro a ser preenchida
 * @return uma lista de todas as soluções de grade e, se não houver solução para uma entrada específica do sudoku - uma lista em branco
 -}
solucaoPossivel :: Sudoku -> (Int, Int) -> [Sudoku]
solucaoPossivel grade (i,j) = [solucao | valor <- possiveisValores grade (i,j), solucao <- solucionar (atualizar i j valor grade)]

{-
 * Encontra os possíveis valores que podem ser gravados na posição (i, j)
  *
  * @param grade o sudoku específico
  * @param (i, j) posição da célula em branco no tabuleiro
  * @return uma lista dos números que podem ser escritos nesta célula de acordo com as regras do jogo
 -}
possiveisValores :: Sudoku -> (Int, Int) -> [Int]
possiveisValores grade (i,j) = [valor | valor <- [1 .. 9], checaLinha grade i valor && checaColuna grade j valor && checaQuadrado grade i j valor]

{-
 * Converte um número em String
  *
  * @param x dígito
  * @return String
  *
  * toStr 3 -> "3"
 -}
toStr :: Int -> String
toStr x 
    | x >= 0 && x <= 9 = [chr (ord '0' + x)] ++ " "
    | otherwise = "z"

{-
* Transforma um sudoku em uma versão para console imprimível
  *
  * @param entrada de sudoku da grade 
  * @param i enfileiro no tabuleiro para iteração recursiva da grade. A primeira chamada é = 0
  * @param j  coluna no tabuleiro para iteração recursiva da grade. A primeira chamada é = 0
  * O resultado @param mantém o resultado até a iteração específica. A primeira chamada é ""
  * @return String em um bom formato, pronto para impressão direta posteriormente no console
 -}
sudokuToString :: Sudoku -> Int -> Int -> String -> String
sudokuToString grade i j result
    | i == 9 = result
    | j == 9 && (i == 2 || i == 5) = sudokuToString grade (i+1) 0 (result ++ "\n-------------------\n")
    | j == 9 = sudokuToString grade (i+1) 0 (result ++ "\n")
    | j == 2 || j == 5 = sudokuToString grade i (j+1) (result ++ (toStr (at grade i j)) ++ "|")
    | otherwise = sudokuToString grade i (j+1) (result ++ (toStr (at grade i j)))

{-
 * Faz todos os julgamentos de um sudoku e imprime o primeiro
  *
  * @param solucoes uma lista de soluções de um sudoku
  * @return Se houver soluções, imprime a primeira, se não - notifica o usuário
 -}
mostraSolucao :: [Sudoku] -> IO ()
mostraSolucao solucoes = 
    if solucoes == [] then putStrLn("Sem soluções")
    else putStr (sudokuToString (head solucoes) 0 0 "")

main = do
    mostraSolucao (solucionar sudoku)

    
    
     


