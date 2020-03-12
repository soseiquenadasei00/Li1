-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g045 where

import LI11920
import Tarefa1_2019li1g045
import Tarefa0_2019li1g045



{-|
Mudele    : Tarefa2
Descrição : Módulo Haskell contendo funções desenvolvidas com objetivo de fazer jogadas
Copyright : Pedro Saldanha <a90618@alunos.uminho.pt>
            Carlos         <...> -}


-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0,Acelera,estado1)
            ,(1,Acelera,estado1)
            ,(2,Desacelera,estado1)
            ,(3,Dispara,estado1)
            ,(2,Movimenta B,estado1)
            ,(1,Acelera,estado1)
            ,(4,Desacelera,estado1)
            ,(2,Movimenta C, estado1)
            ,(2,Acelera,estado1)
            ,(3,Dispara,estado1)
            ,(4,Movimenta C,estado1)
            ,(2,Acelera,estado1)
            ,(1,Dispara,estado1)
            ,(0,Dispara,estado2)
            ,(1,Acelera,estado2)
            ,(2,Dispara,estado2)
            ,(3,Dispara,estado2)
            ,(2,Dispara,estado2)]

mapa1 = gera 5 8 1
js1 = [(Jogador 0 2 0 5 (Morto 2))
      ,(Jogador 1 1.1 0 5 (Ar 1 15 0))
      ,(Jogador 2 2 0 4 (Chao False))
      ,(Jogador 3 4.0 0 0 (Chao True))
      ,(Jogador 4 2.3 0 5 (Chao True))]

estado1 = Estado mapa1 js1

mapa2 = gera 5 8 12
js2 = [(Jogador 0 0 0 5 (Chao True))
      ,(Jogador 1 2 0 4 (Ar 2 90 0))
      ,(Jogador 2 2 0 5 (Chao True))
      ,(Jogador 3 2 0 4 (Chao True))]

estado2 = Estado mapa2 js2

{-| O objetivo dessa tarefa é criar jogada-}

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -> Jogada -> Estado -> Estado 
jogada js x (Estado m l) | x == Acelera = Estado m (atualizaIndiceLista js (aceleramota (encontrarpista js l)) l) 
                         | x == Desacelera = Estado m (atualizaIndiceLista js (desaceleramota (encontrarpista js l)) l)
                         | x == Movimenta C =Estado m (atualizaIndiceLista js (movimenta m j C) l)                         
                         | x == Movimenta B = Estado m (atualizaIndiceLista js (movimenta m j B) l)
                         | x == Movimenta E = Estado m (atualizaIndiceLista js (mudancadeangulo jds E) l)
                         | x == Movimenta D = Estado m (atualizaIndiceLista js (mudancadeangulo jds D) l)
                         | x == Dispara = disparacola (Estado m l) js 
  where 
    j= encontrarpista js l
    jds = encontraIndiceLista js l 
{-| função criada para encontrar a pista em que o jogador se encontra-}
encontrarpista :: Int -> [a] -> a
encontrarpista j l = encontraIndiceLista j l


-- * Estado do Jogador


 {- As funçõe abaixo foram criadas com objetivo de verificar o estado em que o jogador se encontra-}
noChao::EstadoJogador -> Bool
noChao (Chao _) = True
noChao _ = False

noAr :: EstadoJogador->Bool
noAr (Ar _ _ _)= True
noAr _ = False 

estaMorto :: EstadoJogador ->Bool
estaMorto (Morto _) = True
estaMorto _ = False

-- * Disparar cola
{-| A função 'dispararcola' tem o objetivo de alterar o tipo do piso na peça anterior (Funcionando apenas quando o jogador a dispara),caso o jogador tente disparar sem munição o piso anterior não é 
alterado e não é possivel alterar o primeiro piso-}

disparacola :: Estado -> Int -> Estado 
disparacola e@(Estado m l) n 
 | cola>0 && (floor distancia > 1) && (noChao estado) = (Estado novomapa (atualizaIndiceLista n novojogador l))
 | otherwise = e 
  where 
    (Jogador pista distancia vel cola estado) = encontrarpista n l 
    alfa = (floor distancia) -1 
    novapeca= mudartipo (encontraPosicaoMatriz (pista,alfa) m )
    novomapa= atualizaPosicaoMatriz (pista,alfa) novapeca m 
    novojogador = (Jogador pista distancia vel (cola-1) estado)
{-| Essa função foi desenvolvida para auxliar a 'dispararcola' no qual altera o tipo do piso na peca-}

mudartipo:: Peca -> Peca
mudartipo (Recta _ x)= (Recta Cola x)
mudartipo (Rampa _ x y)= (Rampa Cola x y)   

-- * Os tipos de jogadas 
 
{-As duas funções abaixo são para alterar o estado do jogador permitindo que o acelere (Chao True) ou desacelere (Chao False). Contudo, nessa tarefa não é desenvolvido a velocidade e a diferença de 
distância -}

aceleramota ::Jogador->Jogador
aceleramota (Jogador a b c d (Chao _)) = (Jogador a b c d (Chao True))
aceleramota x = x 

desaceleramota ::Jogador->Jogador
desaceleramota (Jogador a b c d (Chao _)) = (Jogador a b c d (Chao False))
desaceleramota x = x

{-| O objetivo dessa função é me devolver a diferença de altura ,pois caso a altura entre uma pista e outra for maior que 0.2 o jogador deve devolver um estado de morto.
sendo uma função auxiliar da 'movimenta'-}

diffAltura::Jogador->Mapa->Direcao->Double
diffAltura (Jogador p d _ _ (Chao _)) m dir = abs(altAtual-proxAltura)
  where
    pecaAtual = encontraPosicaoMatriz (p,max (floor d) 0) m
    proxPeca = encontraPosicaoMatriz (p+(movePista dir),min (max (floor d) 0) (length m)) m
    getaltura (Recta _ x)   = toDouble x
    getaltura (Rampa _ x y) = (d-toDouble(floor d))*toDouble(abs(x-y))
    altAtual = getaltura pecaAtual
    proxAltura = getaltura proxPeca
diffAltura _ _ _ = 0


{-|Função auxiliar da 'diffAltura' para que o jogador altere de pista e mude o indice da pista que se encontra-}
movePista::Direcao->Int
movePista dir
  | dir == C = -1
  | dir == B = 1
  | otherwise = 0

{-| Função criada com objetivo de pegar um numero inteiro e transforma-lo em um double-}

toDouble :: Integral a => a -> Double
toDouble x = fromIntegral x::Double

{-| A função irá percorrer cada jogador alterando o seu estado  
1º Como o jogador está morto não é alterado nada
2º O jogador está no ar então a função devolve o estado com nova inclinação ou a mesma inclinação inicial
3º Dependendo da direção que aperta D ou E a função altera o estado do jogador para True ou False (Acelerar ou Desacelerar) 
4º Como o jogador não posso mudar de pista com altura maior que 0.2 a função altera o estado do jogador para (Morto 1)
5º Por último o a função irá alterar a pista em que o jogador se encontra (Subindo ou descendo) -}

movimenta::Mapa->Jogador->Direcao->Jogador
movimenta _ j@(Jogador _ _ _ _ (Morto _)) _ = j
movimenta m j@(Jogador p d v c (Chao x)) dir
  | dir==D = (Jogador p d v c (Chao True))
  | dir==E = (Jogador p d v c (Chao False))
  | 0.2<=diffAltura j m dir = (Jogador p d v c (Morto 1))
  | otherwise = (Jogador (p+movePista dir) d v c (Chao x))
{-|Função criada com o objetivo de limitar a inclinação das motas-}
upinclinacao :: Double -> Double
upinclinacao a 
  | novai < (-90) = -90
  | novai > 90 = 90 
  | otherwise = novai
  where 
    novai= a 
{-|Função no qual altera a inclinação +15 ou -15, nunca ultrapassando os 90 graus ou os -90 graus -}
dir2graus::Direcao->Double
dir2graus d = encontraIndiceLista (fromEnum d) [0,15,0,-15]

mudancadeangulo :: Jogador-> Direcao -> Jogador
mudancadeangulo j@(Jogador p d v c(Ar alt inc g)) dir |dir == E = (Jogador p d v c (Ar alt (inc + 15.0) g))
                                                      |dir == D = (Jogador p d v c (Ar alt (inc - 15.0) g))



     

