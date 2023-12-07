-- fc59798,fc59821

module Blackjack
( Baralho (..)
, geraBaralho
, embaralha
, valorAposta
, converte
, tamanho
, EstadoJogo (..)
, inicializa
, creditos
, baralho
, terminado
, primeiraDistribuicao
, segundaDistribuicao
, terceiraDistribuicao
, pontos
, resultadoFinal
, simulaRonda
) where

import System.Random
import Test.QuickCheck


newtype Carta = Carta String deriving Show
newtype Baralho = Baralho [Carta] deriving Show

instance Arbitrary Carta where
    arbitrary = do
        valor <- elements "A23456789TJQK"
        naipe <- elements "SHDC"
        return $ Carta [valor, naipe]

instance Arbitrary Baralho where
    arbitrary = do
        numBaralhos <- choose (1,5)
        seed <- choose (0, maxBound)
        let gen = mkStdGen seed
        return $ converte $ embaralha gen $ geraBaralho numBaralhos

geraBaralho :: Int -> [String]
geraBaralho numBaralhos =
    let valor = "A23456789TJQK"
        naipe = "SHDC"
    in concat $ replicate numBaralhos [[v,n] | v <- valor, n <- naipe]

embaralha :: StdGen -> [String] -> [String]
embaralha _ [] = []
embaralha gen xs = x : embaralha gen2 xs2
    where
        (index, gen2) = randomR (0, length xs - 1) gen
        x = xs !! index
        xs2 = take index xs ++ drop (index + 1) xs

valorAposta :: String -> Int
valorAposta decisao = read (words decisao !! 1)

converte :: [String] -> Baralho
converte = Baralho . map Carta

tamanho :: Baralho -> Int
tamanho (Baralho xs) = length xs

mostrarBaralho :: Baralho -> String
mostrarBaralho (Baralho []) = ""
mostrarBaralho (Baralho [x]) = mostrarCarta x
mostrarBaralho (Baralho (x:xs)) = mostrarCarta x ++ " " ++ mostrarBaralho (Baralho xs)

mostrarCarta :: Carta -> String
mostrarCarta (Carta c) = c

data EstadoJogo = EstadoJogo { creditosJogador :: Int
                             , baralhoMesa :: Baralho
                             , jogador ::Baralho
                             , casa :: Baralho
                             }

instance Show EstadoJogo where
    show (EstadoJogo creditos baralho jogador casa) = "jogador: "  ++ mostrarBaralho jogador ++ "\ncasa: "
        ++ mostrarBaralho casa

instance Arbitrary EstadoJogo where
    arbitrary = inicializa <$> arbitrary

baralhoVazio :: Baralho
baralhoVazio = converte []

inicializa :: Baralho -> EstadoJogo
inicializa baralho = EstadoJogo 100 baralho baralhoVazio baralhoVazio

creditos :: EstadoJogo -> Int
creditos = creditosJogador

baralho :: EstadoJogo -> Baralho
baralho = baralhoMesa

terminado :: EstadoJogo -> Bool
terminado (EstadoJogo x b _ _) = x == 0 || tamanho b <= 20

valorCarta :: Carta -> Int
valorCarta (Carta c)
    | valor `elem` ['1'..'9'] = read [valor]
    | valor == 'A' = 1
    | otherwise = 10
    where valor = head c

pontos :: Baralho -> Int
pontos (Baralho xs)
    | contaAces (Baralho xs) > 0 = pontosAux $ foldl (\acc (Carta x) -> acc + valorCarta (Carta x)) 0 xs
    | otherwise = foldl (\acc (Carta x) -> acc + valorCarta (Carta x)) 0 xs
   where pontosAux n
            | n + 10 <= 21 = n + 10
            | otherwise = n

contaAces :: Baralho -> Int
contaAces (Baralho xs) = length $ filter (\(Carta x) -> head x == 'A') xs

simulaRonda :: Int -> EstadoJogo -> EstadoJogo
simulaRonda aposta estadoJogo =
  let primeiroEstado = primeiraDistribuicao aposta estadoJogo
      segundoEstado = segundaDistribuicao primeiroEstado
      terceiroEstado = terceiraDistribuicao segundoEstado
  in
    resultadoFinal aposta terceiroEstado

-- Distribui duas cartas para o jogador e duas cartas para a casa
primeiraDistribuicao :: Int -> EstadoJogo -> EstadoJogo
primeiraDistribuicao valor (EstadoJogo creditos baralho jogador casa) =
     EstadoJogo (creditos - valor) (retira 4 baralho ) (distribuiCartas 2 baralho)
            (distribuiCartasCasa baralho)
        where distribuiCartasCasa (Baralho xs) = Baralho (take 2 (drop 2 xs))

retira :: Int -> Baralho -> Baralho
retira x (Baralho xs) = Baralho (drop x xs)

distribuiCartas :: Int -> Baralho -> Baralho
distribuiCartas x (Baralho xs) = Baralho (take x xs)

unir :: Baralho -> Baralho -> Baralho
unir (Baralho xs) (Baralho ys) = Baralho $ xs ++ ys

--Distribui cartas para o jogador 
segundaDistribuicao :: EstadoJogo -> EstadoJogo
segundaDistribuicao  (EstadoJogo creditos baralho jogador casa) = EstadoJogo creditos (retira 1 baralho) (unir jogador (distribuiCartas 1 baralho)) casa

-- Distribui cartas para a casa
terceiraDistribuicao :: EstadoJogo -> EstadoJogo
terceiraDistribuicao = distribuiAte17

distribuiAte17 :: EstadoJogo -> EstadoJogo
distribuiAte17 (EstadoJogo creditos baralho jogador casa)
    | pontos casa < 17 = distribuiAte17 (EstadoJogo creditos (retira 1 baralho) jogador (unir casa (distribuiCartas 1 baralho)))
    | otherwise = EstadoJogo creditos baralho jogador casa

resultadoFinal :: Int -> EstadoJogo -> EstadoJogo
resultadoFinal aposta (EstadoJogo creditos baralho jogador casa)
    | not (rebentou jogador) && rebentou casa = vitoriaJogador
    | not (rebentou jogador) && not (rebentou casa) && pontos jogador > pontos casa = vitoriaJogador
    | not (rebentou jogador) && not (rebentou casa) && pontos jogador == pontos casa = empate
    | otherwise = vitoriaCasa
        where rebentou baralho = pontos baralho > 21
              vitoriaJogador = EstadoJogo (creditos + (aposta * 2)) baralho baralhoVazio baralhoVazio
              vitoriaCasa = EstadoJogo creditos baralho baralhoVazio baralhoVazio
              empate = EstadoJogo (creditos + aposta) baralho baralhoVazio baralhoVazio


