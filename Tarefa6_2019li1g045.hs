-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g045 where

import LI11920
import Tarefa4_2019li1g045
import Tarefa2_2019li1g045
import Tarefa0_2019li1g045
-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot = undefined



acelerar:: Estado -> Maybe Jogada
acelerar est =  




inclinacao :: Estado -> Int -> Maybe Jogada 
inclinacao est idBot | abs ((inclinacaoJogador estado) - (angdapeca p)) >= 45 = Just (Movimenta D)
                     | abs ((inclinacaoJogador estado) - (angdapeca p)) <= 45 = Just (Movimenta C)
                     | otherwise= Nothing
 where 
  bot = encontraIndiceLista idBot (jogadoresEstado est) 
  estado= estadoJogador bot
  m = mapaEstado est
  pista_idx = pistaJogador bot
  peca_idx = floor $ distanciaJogador bot
  p = (m !! pista_idx) !! peca_idx
  



{--pecas :: Peca -> Int 
pecas (Recta p _) | p == Boost = 1 
                  | p == Terra = 2
                  | p == Relva = 3 
                  | p == Lama = 4 
                  | p == Cola = 5 
pecas (Rampa p _ _) | p == Boost = 6 
                    | p == Terra = 7
                    | p == Relva = 8 
                    | p == Lama = 9
                    | p == Cola = 10 
--}


