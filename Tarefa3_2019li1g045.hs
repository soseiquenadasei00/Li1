-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g045 where

import LI11920
import Tarefa0_2019li1g045
import Tarefa1_2019li1g045
import Tarefa2_2019li1g045
import Data.List 




{-|
Mudele    : Tarefa3
Descrição : Módulo Haskell contendo funções desenvolvidas com objetivo de descontruir um mapa 
Copyright : Pedro Saldanha <a90618@alunos.uminho.pt>
            Carlos         <...> -}

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [m2]

m1 = [[Recta Terra 0,Recta Terra 0,Recta Boost 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Recta Boost 0,Rampa Terra 0 1]]
m2 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4]]
m3 = [[Repete 2 [Anda [0] Terra],Anda [0] Boost],[Repete 2 [Anda [1] Terra],Sobe [1] Terra 1],[Anda [2] Terra,Sobe [2] Terra 2,Sobe [2] Relva 2]]
m4 = [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Lama 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 2,Rampa Relva 2 0,Recta Boost 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Boost 0],[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 1,Rampa Relva 1 0,Recta Relva 0,Rampa Relva 0 1,Recta Lama 1,Rampa Lama 1 3,Rampa Lama 3 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 2,Rampa Terra 2 4,Recta Relva 4,Rampa Terra 4 6,Recta Terra 6,Recta Relva 6,Recta Lama 6,Recta Boost 6,Rampa Terra 6 2,Rampa Terra 2 4,Rampa Relva 4 5,Recta Terra 5,Rampa Lama 5 2,Rampa Lama 2 0],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Lama 1 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Rampa Terra 0 2,Recta Relva 2,Rampa Relva 2 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0]]


-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi m= listainst m
 

-- * Padrão Horizontal 
 

{-| Esta função irá transformar cada peça em uma Instrução -} 
construirPeca :: Int -> Peca -> Instrucao 
construirPeca  n (Recta p a)  = Anda [n] p
construirPeca  n (Rampa p x y)  |x < y = Sobe [n] p (y-x) 
                                |otherwise = Desce [n] p (x-y)


{-|A função funcInstru irá receber um mapa (ou [[Peca]]), então o zip [0..] colocará um Int em cada lista . 
Com isso o map vai ao n que é o índice da lista e a cada elemento da lista usa a função 'construirPeca'. Após isso usamos o concat para transformar em uma lista. 
ex: zip [0..] [[1,2,3],[12,3,2] = [(0,[1,2,3]),(1,[12,3,2])]-} 

funcInstru :: [[Peca]] ->Instrucoes
funcInstru = concat . map (\(n,l)->map (construirPeca n) l) . (zip [0..])


{-|Essa função é usada para gerar as repetições.-}
repeticao :: Instrucao ->Int-> Instrucao 
repeticao i n = Repete n [i] 

{-|A função a seguir utiliza a mesma ideia da 'funcInstru' contudo, a cabeça do mapa é ignorada. Para utilizar o repete fiz o group dessa lista colocando os iguais em listas e então no 'Where'
fazemos a contagem dos elementos iguais na lista e acrescentamos o repete e o número (que é o tamanho da lista, ou seja quantas vezes irá repetir) de instruções. -}
listainst:: [[Peca]] -> [Instrucao]
listainst = map f .  group . concat . map (\(n,l)->map (construirPeca n) (tail l)) . (zip [0..])
    where f l| 1<length l = repeticao (head l) (length l)
             | otherwise = head l




-- * Padrão Vertical 
--Em desenvolvimento 

{-| Essa função gera uma matriz transposta.
    Ex: [[1,2,3],[1,2,3],[1,2,3] = [[1,1,1],[2,2,2],[3,3,3]--}
transpormatriz :: [[a]] -> [[a]] 
transpormatriz = foldr (zipWith (:)) (repeat [])

{-| A função é parecida com a 'pecainstru' ou seja, ela irá receber um mapa e então dara uma matriz de instruções.-}
pecainstru2:: [[Peca]] -> [[Instrucao]]
pecainstru2 = (map (map auxrepetete)). (map group) . map (\(n,l)->map (construirPeca n) (tail l)) . (zip [0..])

{-|Função que auxilia para contar a quantidade de instroções iguais-}
auxrepetete :: [Instrucao] -> Instrucao 
auxrepetete  l = if 1<length l then repeticao (head l) (length l) else  head l

{-|Esta função recebe 'pecainstru2' e então com a 'transpormatriz' faz a transposta da matriz e então o map olhará cada elemento da matriz procurando acumular as instruções e então o cancat transforma
a matriz em uma lista.-}
listainst2:: [[Peca]] -> [Instrucao]
listainst2  = concat .  map (\l->acumularInst l )  . transpormatriz . pecainstru2


{-|Função que utiliza as funções abaixo para gerar instruções (lista de instrucao) isto é a função deverá acumular todas as instruções que sejam iguais.-}
acumularInst :: [Instrucao] -> [Instrucao]
acumularInst [] = []
acumularInst l  =  iguais : diferentes 
 where 
    iguais = (acumularIndex . buscariguais ) l 
    diferentes = (acumularInst . buscarDiferentes ) l 


{-|Essa função é utilizada para acumular as instruções iguais, ou seja, o foldr vai buscar a instrução (Anda,Sobe ou Desce) e então procura na lista devolvendo a instrução acumulada.
ex : [Anda [0] Terra, Anda [1] Terra] = Anda [0,1] Terra-}
acumularIndex:: [Instrucao] -> Instrucao
acumularIndex l@((Anda _ p):_) = Anda (foldr (\(Anda y _) a -> y++a ) [] l) p  
acumularIndex l@((Sobe _ p alt):_)=Sobe (foldr (\(Sobe y _ alt) a -> y++a) [] l) p alt
acumularIndex l@((Desce _ p alt):_)=Desce (foldr (\(Desce y _ alt) a -> y++a) [] l) p alt
acumularIndex l@((Repete n u):_) = Repete n u  

{-|Essa função utiliza de um filter, ou seja, o filter como nome ja diz vai filtrar o que for determinado para filtrar (no caso desta função utilizei o True) e com auxilio da função 'comparartipos'.  
Filter é uma função do haskell que recebe (a->bool) -> [a]->[a], com isso a função olha o elemento "a" e busca o que foi determinado (True), então vai a lista e gera uma nova lista,apenas com os elementos iguais-}
buscariguais :: [Instrucao] -> [Instrucao]
buscariguais (i:l) = i : (filter (\x ->comparartipos i x) l) 

{-|A função é parecida com a 'buscariguais' porém ela ira devolver os que não são iguais -}
buscarDiferentes :: [Instrucao] -> [Instrucao]
buscarDiferentes (i:l) = filter (\x ->not (comparartipos i x)) l


{-| Está função possui o objetivo de olhar duas instruções e se o piso e a altura (A altura é considerada apenas para a Sobe e Desce) forém iguais ele devolve True.-}

comparartipos :: Instrucao -> Instrucao -> Bool 
comparartipos (Anda _ p) (Anda _ k) = p == k 
comparartipos (Sobe _ p a) (Sobe _ j a1) = p == j && a == a1 
comparartipos (Desce _ p a) (Desce _ j a1) = p == j && a == a1 
comparartipos  _ _ = False  
 