{-
Princípios de Programação
Projeto 3 - Modelo de submissão

* A vossa submissão deverá ser composta por um único ficheiro zip
p3_XXXXX_YYYYY.zip onde XXXXX, YYYYY são os vossos números de aluno
por ordem crescente.
* O ficheiro zip deverá conter no mínimo um ficheiro com o nome Main.hs
* O vosso código deverá ser compilável com uma instrução do tipo

> stack ghc Main.hs

A instrução acima produz um executável Main, que deverá ser executável
através de um dos seguintes quatro tipos de instruções:

> ./Main ficheiro -- carrega um baralho para jogar Blackjack
> ./Main          -- carrega o baralho default.bar
> ./Main -n X     -- carrega um baralho aleatório formado por X baralhos normais de cartas
> ./Main -t       -- corre os testes
-}

-- fc59798,fc59821

import Test.QuickCheck
import System.Environment
import System.Random
import System.IO
import Blackjack
import System.Directory
import Testes

main :: IO()
main = do
    args <- getArgs
    if "-t" `elem` args
        then runTest
        else runGame args

runGame :: [String] -> IO ()
runGame args = do
    let (filePath, numBaralho) = inicializacao args
    fileExists <- doesFileExist filePath
    if null filePath ||  (not fileExists && filePath /= "-n")
        then modosUtilizacao
        else do
            baralhoString <- tiposInicializacao (filePath, numBaralho)
            jogo $ inicializa $ converte baralhoString

inicializacao :: [String] -> (FilePath, Int)
inicializacao args = case args of
                [file] -> (file, 1)
                ["-n", x] -> ("-n", read x)
                [] -> ("default.bar", 1)
                _ -> ("", -1)

tiposInicializacao :: (FilePath, Int) -> IO [String]
tiposInicializacao (file, numBaralho)
    | file == "-n" = baralhoAleatorio numBaralho
    | otherwise = lerBaralho file

modosUtilizacao :: IO ()
modosUtilizacao = do
    putStrLn "Utilização:"
    putStrLn "./Main ficheiro -- carrega um baralho para jogar Blackjack"
    putStrLn "./Main          -- carrega o baralho default.bar"
    putStrLn "./Main -n X     -- carrega um baralho aleatório formado por X baralhos normais"
    putStrLn "./Main -t       -- corre os testes"

baralhoAleatorio :: Int -> IO [String]
baralhoAleatorio n = do
    gen <- getStdGen
    let baralhoEmbaralhado = embaralha gen (geraBaralho n)
    return baralhoEmbaralhado

lerBaralho :: FilePath -> IO [String]
lerBaralho filePath = do
    content <- readFile filePath
    return (lines content)

printCartasCredito :: EstadoJogo -> IO ()
printCartasCredito estadoJogo = do
    putStrLn ("cartas: " ++ show (tamanho (baralho estadoJogo)))
    putStrLn ("creditos: " ++ show (creditos estadoJogo))

jogo :: EstadoJogo -> IO ()
jogo estadoJogo = do
    printCartasCredito estadoJogo
    decisao <- getLine
    verificarDecisao estadoJogo decisao

verificarDecisao :: EstadoJogo -> String -> IO ()
verificarDecisao estadoJogo decisao
    | decisao == "sair" = sair estadoJogo
    | valorAposta decisao > creditos estadoJogo = do
        erro estadoJogo
        jogo estadoJogo
    | otherwise = continuar estadoJogo decisao

continuar :: EstadoJogo -> String -> IO ()
continuar estadoJogo decisao = do
    novoEstado <- ronda estadoJogo (valorAposta decisao)
    if terminado novoEstado
        then printCartasCredito novoEstado >> sair novoEstado
        else jogo novoEstado


ronda :: EstadoJogo -> Int -> IO EstadoJogo
ronda = primeiraFase

primeiraFase :: EstadoJogo -> Int -> IO EstadoJogo
primeiraFase estadoJogo valor = segundaFase (primeiraDistribuicao valor estadoJogo) valor

segundaFase :: EstadoJogo -> Int -> IO EstadoJogo
segundaFase estadoJogo valor
    | pontos (jogador estadoJogo) == 21 = do
        printCartasJogo estadoJogo
        terceiraFase estadoJogo valor
    | pontos (jogador estadoJogo) > 21 = derrota estadoJogo valor
    | otherwise = hitStand estadoJogo valor

hitStand :: EstadoJogo -> Int -> IO EstadoJogo
hitStand estadoJogo valor = do
        printCartasJogo estadoJogo
        decisao <- getLine
        if decisao == "hit"
            then segundaFase (segundaDistribuicao estadoJogo) valor
            else terceiraFase estadoJogo valor

terceiraFase :: EstadoJogo -> Int -> IO EstadoJogo
terceiraFase estadoJogo valor = do
    let estadoFinal = terceiraDistribuicao estadoJogo
    printCartasJogo estadoFinal
    let resultado = resultadoFinal valor estadoFinal
    printResultado valor estadoFinal resultado
    return resultado

printCartasJogo :: EstadoJogo -> IO ()
printCartasJogo estado = putStrLn (show estado)

printResultado :: Int -> EstadoJogo -> EstadoJogo -> IO ()
printResultado valor estadoFinal resultado
    | creditos estadoFinal + valor == creditos resultado = putStrLn "Empate"
    | creditos estadoFinal + (2 * valor) == creditos resultado = putStrLn "Vitoria"
    | otherwise = putStrLn "Derrota"

erro :: EstadoJogo -> IO ()
erro estadoJogo = do
    putStrLn "O valor da aposta não é valido, insira novamente"

sair :: EstadoJogo -> IO ()
sair estadoJogo = putStrLn ("saldo final: " ++ show (creditos estadoJogo))

derrota :: EstadoJogo -> Int -> IO EstadoJogo
derrota estado valor = do
    printCartasJogo estado
    putStrLn "Derrota"
    return (resultadoFinal valor estado)

runTest :: IO ()
runTest = do
    quickCheck prop_pontosMaoInicial
    quickCheck prop_creditosAposRonda
    quickCheck prop_pontosMaoCasa
    quickCheck prop_quantasCartasMaoInicial
    quickCheck prop_tamanhoBaralhoDiminui
    quickCheck prop_creditosAposDistribuicao


