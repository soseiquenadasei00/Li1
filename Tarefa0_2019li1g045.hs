-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g045 where
import Data.Fixed
    
-- * Funções não-recursivas.
    
-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving Show
    
-- | Um ângulo em graus.
type Angulo = Double
    
-- ** Funções sobre vetores
    
-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>
    
-- *** Funções gerais sobre 'Vetor'es.
-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores v1 v2 = Cartesiano (posx v1 + posx v2) (posy v1+ posy v2)

angToRad::Double -> Double
angToRad a = a*pi/180


posx :: Vetor -> Double
posx (Cartesiano x y) = x 
posx (Polar r a) = r * cos (angToRad a) 

posy :: Vetor -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin (angToRad a)
    
-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores v1 v2 = Cartesiano (posx v1 - posx v2) (posy v1 - posy v2)
    
-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor a (Cartesiano x y) = Cartesiano (a * x) (a * y)
multiplicaVetor a (Polar dist ang ) = Polar (a * dist) (ang)

    
-- ** Funções sobre rectas.
    
-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)
    
-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam  ((Cartesiano x1 y1), (Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = div/=0 && a >= 0 && a <= 1 && b>=0 && b<=1   
    where 
        div = (((x4-x3)*(y1-y2)) - (x1-x2) * (y4-y3))
        a = ((y3-y4)*(x1-x3)+ (x4-x3)*(y1-y3))/div
        b = (((y1-y2)*(x1-x3)) + ((x2-x1) * (y1-y3))) /div
intersetam (p1,p2) (p3,p4) =  resultado
         where
            (x1,y1) = (posx p1, posy p1) 
            (x2,y2) = (posx p2, posy p2)
            (x3,y3) = (posx p3, posy p3)
            (x4,y4) = (posx p4, posy p4)
            p1r1 = (Cartesiano x1 y1)
            p2r2 = (Cartesiano x2 y2)
            p3r3 = (Cartesiano x3 y3)
            p4r4 = (Cartesiano x4 y4)
            resultado = intersetam (p1r1,p2r2) (p3r3,p4r4)
   
-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas,   deriving (Read,Show,Eq)
--como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao  (p1,p2) (p3,p4)  = prod
     where 
       (x1,y1) = (posx p1, posy p1) 
       (x2,y2) = (posx p2 ,posy p2)
       (x3,y3) = (posx p3, posy p3)
       (x4,y4) = (posx p4, posy p4)
       d       = (((x4-x3)*(y1-y2)) - (x1-x2) * (y4-y3))
       ta      = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3)) / d
       sub     = subtraiVetores p2 p1
       mult    = multiplicaVetor ta sub
       prod    = somaVetores p1 mult 
-- ** Funções sobre listas
-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.
-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função length que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x y = 0 <= x && x < (length y) 

            
    
-- ** Funções sobre matrizes.
    
-- *** Funções gerais sobre matrizes.
    
-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)
    
-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)
    
-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]
    
-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensaoMatriz (h:t) | length h == 0 = (0,0)
                     | otherwise     = (length (h:t),length h) 

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool
ePosicaoMatrizValida _ [] = False 
ePosicaoMatrizValida (p1,p2) (h:t) | p1 >= length (h:t) = False
                                       | p2 >= length h = False
                                       | otherwise = True
    
-- * Funções recursivas.
    
-- ** Funções sobre ângulos
    
-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
    
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo x =   mod' x 360    
-- ** Funções sobre listas.
    
-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista _ [x] = x
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista n (h:t) = encontraIndiceLista (n-1) t 

    
-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista i x [] = []
atualizaIndiceLista i x (h:t) | i == 0 = (x:t) 
                                  | i > length (h:t) =(h:t)
                                  | otherwise = [h] ++ atualizaIndiceLista (i-1) x t

    
-- ** Funções sobre matrizes.
    
-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz _ [] = error "erro matriz vazia"
encontraPosicaoMatriz (x,y) a@(h:t) | x > length (a) || y > length (h) = error "A posição é maior que a matriz"
encontraPosicaoMatriz (0,y) (h:t) = encontraIndiceLista y h 
encontraPosicaoMatriz (x,y) (h:t) = encontraPosicaoMatriz (x-1,y) t
  
  
-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (_,_) x [] = [[]]
atualizaPosicaoMatriz (p1,p2) x (h:t) | p1 == 0 = (atualizaIndiceLista p2 x h):t
                                          |otherwise = [h] ++ atualizaPosicaoMatriz (p1-1,p2) x t
    
    
    