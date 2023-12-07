-- fc59798,fc59821

module Testes where

import Test.QuickCheck
import System.Random
import Blackjack

prop_pontosMaoInicial :: Int -> EstadoJogo -> Property
prop_pontosMaoInicial aposta estado = aposta <= creditos estado ==>
    let estadoAposDistribuir = primeiraDistribuicao aposta estado
        pontosJogador = pontos (jogador estadoAposDistribuir)
        pontosCasa = pontos (casa estadoAposDistribuir)
    in pontosJogador <= 21 && pontosCasa <= 21

prop_creditosAposRonda :: Int -> EstadoJogo -> Property
prop_creditosAposRonda aposta estado = aposta <= creditos estado ==>
    let estadoAposRonda = simulaRonda aposta estado
        creditosInicioRonda = creditos estado
        creditosAposRonda = creditos estadoAposRonda
    in creditosAposRonda == creditosInicioRonda - aposta || creditosAposRonda == creditosInicioRonda + aposta || creditosAposRonda == creditosInicioRonda

prop_pontosMaoCasa :: Int -> EstadoJogo -> Property
prop_pontosMaoCasa aposta estado = aposta <= creditos estado ==>
    let estadoAposPrimeira = primeiraDistribuicao aposta estado
        estadoAposCasa = terceiraDistribuicao estadoAposPrimeira
    in pontos (casa estadoAposCasa) >= 17


-- Verifica se apos a distribuicao da mao inicial, o jogador e a casa tinha exatamente 2 cartas cada
prop_quantasCartasMaoInicial :: Int -> EstadoJogo -> Property
prop_quantasCartasMaoInicial aposta estado = aposta <= creditos estado ==>
    let estadoAposDistribuir = primeiraDistribuicao aposta estado
    in tamanho (jogador estadoAposDistribuir) == 2 && tamanho (casa estadoAposDistribuir) == 2

-- Verifica se apos distribuir as cartas iniciais e apos distribuir uma carta para o jogador, o baralho tinha 5 cartas a menos do que no
-- estado inicial.
prop_tamanhoBaralhoDiminui :: Int -> EstadoJogo -> Property
prop_tamanhoBaralhoDiminui aposta estado = aposta <= creditos estado ==>
    let estadoAposPrimeira = primeiraDistribuicao aposta estado 
        estadoAposSegunda = segundaDistribuicao estadoAposPrimeira
    in tamanho (baralhoMesa estadoAposSegunda) == tamanho (baralhoMesa estado) - 5

-- Verifica que apos a distribuicao da primeira mao, é subtraida dos creditos do jogador a aposta feita
prop_creditosAposDistribuicao :: Int -> EstadoJogo -> Property
prop_creditosAposDistribuicao aposta estado = aposta <= creditos estado ==>
    let estadoAposDistribuir = primeiraDistribuicao aposta estado
    in creditos estadoAposDistribuir == creditos estado - aposta
    
