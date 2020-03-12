-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g045 where

import LI11920
import Tarefa2_2019li1g045
import Tarefa1_2019li1g045
import Tarefa0_2019li1g045



{-|
Mudele    : Tarefa 4 
Descrição : Módulo Haskell contendo funções desenvolvidas com objetivo de calcular velocidade e distancia em relação ao tempo
Copyright : Pedro Saldanha <a90618@alunos.uminho.pt>
            Carlos         <...> -}


-- * Testes
-- | Testes unitários da Tarefa 4. 
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(2.0,m,jds1),(2.0,m,jds2),(1.8,m,jds3),(2.0,m,jds4)]

m=[[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0],[Recta Terra 0,Recta Lama 0,Rampa Relva 0 1,Rampa Terra 1 0,Rampa Terra 0 2]]
jds1 = (Jogador 0 1.0 3 5(Chao True))
jds2 = (Jogador 1 3 1 5(Chao True))
jds3 = (Jogador 1 2.2 1.3 5(Ar 1.3 4 1))
jds4 = (Jogador 1 2 0 5(Morto 1))


-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
-- ^ O tempo decorrido.
-- ^ O mapa utilizado.
-- ^ O estado anterior do 'Jogador'.
-- ^ O estado do 'Jogador' após um 'passo'.

{-| A função "passo" ultiza das funções 'acelera' e 'move' com isso essa função pode alterar a velocidade e a distancia em realação ao tempo-}

passo :: Double->Mapa-> Jogador-> Jogador 
passo t m j = move t m (acelera t m j)



-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
-- ^ O tempo decorrido.
-- ^ O mapa utilizado.
-- ^ O estado anterior do 'Jogador'.
-- ^ O estado do 'Jogador' após acelerar.

{-| Está funão altera a velocidade em relação ao tempo,isto é depende de em estado o jogador apresenta(Chão ou ar). Com isso, cada estado há seu determinado atrito.
Essa função não altera a distancia-}
acelera :: Double -> Mapa -> Jogador -> Jogador
acelera t m j@(Jogador p d v c(est)) =  case est of (Chao _) -> acelerarNoChao t m j 
                                                    (Ar _ _ _) -> velocidadeNoAr t j  
                                                    (Morto _) -> j 



--Funções para acelera 

{-|Funcão feita para calcular a velocidade final do jogador(no Chão).Com sua atual velocidade a aplicar isso ao tempo e atrito da peça atual-}
acelerarNoChao :: Double -> Mapa -> Jogador -> Jogador 
acelerarNoChao t m j@(Jogador p d v c (Chao acel)) = j{velocidadeJogador = velocidadeFinal}
  where pista_idx = pistaJogador j
        peca_idx = floor $ distanciaJogador j
        p = (m !! pista_idx) !! peca_idx
        velocidadeFinal= max 0 (v + (acelerarMota j - (atrito p) * v) * t)


{-| O objetivo da função é ser o gatilho para aceleração da mota-}
acelerarMota ::Jogador -> Double 
acelerarMota (Jogador p d v c (Chao acel)) = if v < 2 && acel  then 1 else 0


{-| Essa função determina o atrito para da tipo de piso no mapa-}
atrito :: Peca -> Double 
atrito (Recta p _ )  | p == Terra = 0.25
                     | p == Relva = 0.75
                     | p == Lama = 1.50 
                     | p == Cola = 3.00
                     | otherwise = (-0.50)  
atrito (Rampa p _ _) | p == Terra = 0.25
                     | p == Relva = 0.75
                     | p == Lama = 1.50 
                     | p == Cola = 3.00
                     | otherwise = (-0.50)       


{-| Calcula a velocidade do jogador quando esta no ar-}
velocidadeNoAr :: Double -> Jogador -> Jogador
velocidadeNoAr t j@(Jogador p d v c(Ar alt inc g)) = j {velocidadeJogador= max 0 (v-(0.125 *v*t)),estadoJogador= Ar alt inc (g + t)}


{-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
-- ^ O tempo decorrido.
-- ^ O mapa utilizado.
-- ^ O estado anterior do 'Jogador'.
-- ^ O estado do 'Jogador' após se movimentar.--}


{-|Essa função altera a distancia do jogador em relação ao tempo-}
move :: Double ->Mapa -> Jogador -> Jogador
move t m j@(Jogador p d v c (estado)) = case estado of  Morto x -> moveMorto t j
                                                        Chao x -> moveChao t m j
                                                        Ar x y z -> moveAr t m j

--Funções para a move no Chao 

{-| A função abaixo altera o estado do jogador quando está morto, ou seja, quando morrer sua velocidade vai a 0 e o tempo de renascer vai ser dedo pelo timeout menos o tempo (Istó é, essa subtração deve ser maior que 0) caso não o jogador renasce com 
estado de Chão False-}
moveMorto :: Double ->Jogador -> Jogador
moveMorto t (Jogador p d v c(Morto timeout))= if ((timeout-t) >0) then (Jogador p d 0 c (Morto (timeout -t))) else (Jogador p d 0 c (Chao False))

{-|Essa função verifica os pontos em que o jogador esta e calcula a sua distancia, além do mais é preciso criar um limite para que o jogador não ultrapasse os limites determindo das peças 'retaf'-}
moveChao:: Double -> Mapa -> Jogador -> Jogador
moveChao t m j@(Jogador p d v c(Chao _)) |intersetam (retaJogador t m j) retaf = pontofinal j (intersecao  (retaJogador t m j) retaf)
                                         |otherwise = pontofinal j (fst(retaJogador t m j))
    where 
    retaf= ((Cartesiano (realToFrac(floor d+1)) 150),(Cartesiano (realToFrac(floor d+1)) (-150)))                               

{-| A função dara um par de pontos no qual é a distancia do jogador-}
retaJogador :: Double -> Mapa -> Jogador -> Reta
retaJogador t m j@(Jogador p d v c (Chao est)) = reta
 where 
   pista_idx = pistaJogador j
   peca_idx  = floor $ distanciaJogador j
   encPos    = encontraPosicaoMatriz (p,peca_idx) m  -- Encontra a posição na Matriz
   andar     = multiplicaVetor t (Polar v (angdapeca encPos))
   vetorinicial= (Cartesiano d (diffAltura1 encPos d)) -- Calcula o vetorInicial 
   vetorFinal  = somaVetores andar vetorinicial -- 
   reta = (vetorFinal,vetorinicial)

{-| Está função pega a distancia X do ponto cartesiano e altera a distancia no Jogador-}

pontofinal :: Jogador -> Ponto -> Jogador 
pontofinal j@(Jogador p d v c(Chao _)) (Cartesiano x y) = j{distanciaJogador= x}



 --funçoes para mover no Ar

{-| Função que calcula a distancia do jogando quando estiver no Ar-}  
moveAr :: Double -> Mapa -> Jogador->Jogador
moveAr t m j@(Jogador p d v c(Ar alt inc g)) | intersetam (retaJogadorAr t j) (retaChao encPos j) = inclinacao t m (pontofinalAr j (intersecao (retaJogadorAr t j) (retaChao encPos j))) 
                                                     | intersetam (retaJogadorAr t j) retaf = pontofinalAr j (intersecao (retaJogadorAr t j) retaf)
                                                     | otherwise = pontofinalAr j (snd (retaJogadorAr t j))
  where 
   pista_idx = pistaJogador j
   peca_idx = floor $ distanciaJogador j
   encPos = encontraPosicaoMatriz (p,peca_idx) m
   retaf= ((Cartesiano (realToFrac(floor d+1)) 150),(Cartesiano (realToFrac(floor d+1)) (-150))) -- limtie criado para que o jogador não ultrapasse os limites determinado 

{-|Esta função tem o objetivo de calcular uma reta idependente da peça que o jogador se encontra, dando resultado um par de Pontos-}
retaChao :: Peca -> Jogador -> Reta
retaChao (Recta _ alt) j@(Jogador p d v c(est)) = ((Cartesiano (realToFrac(floor d)) (realToFrac alt)),(Cartesiano (realToFrac(floor d+1)) (realToFrac alt))) 
retaChao (Rampa _ alt1 alt2) j@(Jogador p d v c(est)) = ((Cartesiano (realToFrac(floor d)) (realToFrac alt1)),(Cartesiano (realToFrac(floor d+1)) (realToFrac alt2)))


{-|Calcula a reta do jogador quando estiver no ar -}
retaJogadorAr :: Double -> Jogador -> Reta
retaJogadorAr t (Jogador p d v c(Ar alt inc g))= retafinal 
 where 
    desloc= multiplicaVetor t (somaVetores (Polar v inc) (Polar g (-90)))
    pontoinicial= (Cartesiano d alt)
    pontofinal = somaVetores desloc pontoinicial 
    retafinal = (pontoinicial,pontofinal)

{-|Essa função pega a distancia X do ponto Cartesiano e altera a distancia do Jogador e com o ponto Y altera a sua altura-}
pontofinalAr :: Jogador -> Ponto -> Jogador 
pontofinalAr j@(Jogador p d v c(Ar alt inc g)) (Cartesiano x y) = j{distanciaJogador=x, estadoJogador= Ar y inc g}


{-| Essa função calcula a inclinação que o jogador terá diante da atual peça-}
inclinacao :: Double -> Mapa -> Jogador -> Jogador 
inclinacao t m j@(Jogador p d v c(Ar alt inc g)) = if abs(inc - (angdapeca p)) >= 45 then j{velocidadeJogador=0,estadoJogador= Morto 1} else j{velocidadeJogador= v *(cos (angToRad(inc-(angdapeca p)))),estadoJogador = (Chao False)}
 where
    pista_idx = pistaJogador j
    peca_idx = floor $ distanciaJogador j
    p = (m !! pista_idx) !! peca_idx

{-| Calcula o angulo da peça-}
angdapeca ::Peca -> Double
angdapeca (Recta _ _) = 0.0
angdapeca (Rampa _ alt1 alt2) = 180*(atan altF)/pi 
 where 
    altF = realToFrac alt2- realToFrac alt1

{-| Calcula a diferença de altura da peça -}
diffAltura1::Peca->Double-> Double
diffAltura1 (Recta _ alt) d =  realToFrac alt 
diffAltura1 (Rampa _ alt1 alt2) d | alt2 > alt1 = d * realToFrac (alt2-alt1)
                                 | otherwise = 1-d * realToFrac (alt1-alt2)
