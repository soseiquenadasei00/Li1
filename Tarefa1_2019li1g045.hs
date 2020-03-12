{-# OPTIONS_GHC -fno-warn-tabs #-}
-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g045 where

import LI11920
import System.Random
import Data.List.Split
import Tarefa0_2019li1g045





{-|
Mudele    : Tarefa1
Descrição : Módulo Haskell contendo funções desenvolvidas com objetivo de gerar um mapa
Copyright : Pedro Saldanha <a90618@alunos.uminho.pt>
            Carlos         <...> -}


-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 =  [(2,20,1), (10,25,120),(4,8,1),(4,12,1),(4,16,5),(4,18,3), (4,20,6), (4,24,10),(4,32,2),(5,20,1),(6,12,1),(5,50,2),(5,200,9),(4,48,3),(1,10,4),(2,8,3),(3,30,2),(4,40,19),(5,100,2),(6,60,2),(7,70,1),(8,80,9),(9,90,4),(10,50,14),(0,10,1),(2,10,20)]



-- * Funções pré-definidas da Tarefa 1.


geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento semente = mapa2 
  where
  mapa = geramapa (geramatriz npistas (comprimento-1) semente)
  mapaFinal :: Mapa -> Mapa
  mapaFinal [] = []
  mapaFinal (h:t) = (Recta Terra 0 : h) : mapaFinal t 
  mapa2 = mapaFinal mapa


{-| Essa função utiliza a 'geraAleatorios' (No qual gera-me uma lista de números pseudo aleatórios no qual está a precisar do comprimento e a semente), então multiplicamos pelo número de pistas (Contudo, para dar a quantidade de pista que necessita 
multiplicamos por 2 {pois o comprimento deve ser o dobro}.  Com isso utilizamos 'agrupa2' para agrupar toda essa lista em tuplos e dividimos a lista  pelo comprimento necessário . 
'chunksOf é uma função  do haskell no qual deve haver um 'import Data.List.Split' para funcionar e então essa função é do tipo :: Int -> [e] -> [[e]] , 
ou seja, ele irá buscar o Int (Cujo é o comprimento) e a lista gerando uma lista de listas  = Matriz = Mapa 
ex : chunksOf 3 [1,2,3,4,5,6] = [[1,2,3][4,5,6]]-}

geramatriz :: Int -> Int -> Int -> [[(Int,Int)]]
geramatriz npistas comprimento semente= chunksOf comprimento (agrupa2$geraAleatorios ((comprimento)*npistas*2)  semente)


{- | Funçao agrupa pega elementos de uma lista e transforma em lista de tuplos -}
agrupa2 :: [Int] -> [(Int,Int)]
agrupa2 [] = []
agrupa2 (h:(x:xs)) = ((h,x) : (agrupa2 xs)) 


-- * Geração de peça



{-| Essa função determina o tipo do piso
'p' é o piso anterior, assim caso o x estiver entre 6 e 9 volta ao piso anterior -}
tipoPiso :: Int -> Piso -> Piso
tipoPiso x  p | x >= 0 && x <= 1 = Terra
              | x >= 2 && x <= 3 = Relva
              | x == 4 = Lama
              | x == 5 = Boost 
              | x >= 6 && x <= 9 = p 
       


{- | A funcao peca, gera um peca com base na anterior (A primeira peca é Recta Terra 0) e então 
com base no primeiro elemento com sua altura a peca seguinte é criada. 
Primeira peca 'Recta Terra 0' 
'Recta p a' peca anterior sendo uma recta com altura a 
'Rampa p _ a' peca anterior sendo uma Rampa com altura inicial _ e final a-}

peca :: Peca -> (Int,Int) -> Peca
peca (Recta p a) (x,y) | y==0 || y ==1 = (Rampa (tipoPiso x p) a (a+y+1))
                       | y>= 2 && y <= 5 = if a==0 then (Recta (tipoPiso x p) a)  
                       	                            else 
                       	                             if a-y+1<0 
                       	                              then (Rampa (tipoPiso x p) a 0)
                                                       else (Rampa (tipoPiso x p) a (a-(y-1)))
                       | y>= 6 && y <= 9 = (Recta (tipoPiso x p) a)
peca (Rampa p _ a) (x,y) | (y==0 || y ==1) = (Rampa (tipoPiso x p) a (a+y+1))
                         | y>= 2 && y <= 5 = if a==0 then (Recta (tipoPiso x p) a)  
                                                    else 
                                                     if a-y+1<0 
                                                      then (Rampa (tipoPiso x p) a 0)
                                                       else (Rampa (tipoPiso x p) a (a-(y-1)))
                         | y>= 6 && y <= 9 = (Recta (tipoPiso x p) a)  


-- * Geração de pistas e o mapa 
 

{- | A função cada pista precisa da peca anterior gerada na função 'peca' e então aplica para
cada pista  gerando uma lista de Pecas = Pista--}

cadapista :: Peca -> [(Int,Int)] -> [Peca] 
cadapista a [] = []
cadapista p (x:xs) = (peca p x) : cadapista (peca p x) xs


{- | a função gera mapa utiliza a função 'cadapista' utilizando a primeira peca 'Recta Terra 0' de forma recursiva, ou seja, aplica a primeira peca para cada pista. Com isso geramos o mapa -}

geramapa :: [[(Int,Int)]] -> Mapa
geramapa [] = []
geramapa (h:t) = cadapista (Recta Terra 0) h : geramapa t


