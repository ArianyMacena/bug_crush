module UI (limparTela,telaInicial,esperarM,menuInicial,telaLogin,telaRegras,telaInstrucoes,renderHUD,telaGameOver) where

import System.IO (hFlush, stdout)

green, reset :: String
green = "\ESC[32m"
reset = "\ESC[0m"

-- Limpar a tela do terminal
limparTela :: IO ()
limparTela = do
    putStr "\ESC[2J"
    putStr "\ESC[H"

-- 1. Criação da Tela Inicial
telaInicial :: IO ()
telaInicial = do
    limparTela
    putStrLn $ green ++ "                                            " ++ reset
    putStrLn $ green ++ "  ____  _    _  _____      _____ _____  _    _  _____ _    _ " ++ reset
    putStrLn $ green ++ " |  _ \\| |  | |/ ____|    / ____|  __ \\| |  | |/ ____| |  | |" ++ reset
    putStrLn $ green ++ " | |_) | |  | | |  __    | |    | |__) | |  | | (___ | |__| |" ++ reset
    putStrLn $ green ++ " |  _ <| |  | | | |_ |   | |    |  _  /| |  | |\\___ \\|  __  |" ++ reset
    putStrLn $ green ++ " | |_) | |__| | |__| |   | |____| | \\ \\| |__| |____) | |  | |" ++ reset
    putStrLn $ green ++ " |____/ \\____/ \\_____|    \\_____|_|  \\_\\\\____/|_____/|_|  |_|" ++ reset
    putStrLn ""
    --putStrLn $ green ++ "     ~ 🐜 ~ 🐞 ~ 🐝 ~ 🦗 ~ 🕸️ ~" ++ reset
    putStrLn $ green ++ " [ Pressione a tecla 'M' para ir ao Menu Inicial ]" ++ reset
    esperarM

--2. Esperar o usuário digitar a entrada válida
esperarM :: IO ()
esperarM = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input == "m" || input == "M"
        then return ()
        else do
            putStrLn "Entrada inválida. Aperte 'M' para continuar."
            esperarM


--3. Criação do Menu Inicial
menuInicial :: IO Int
menuInicial = do
    limparTela
    putStrLn "===================="
    putStrLn "      BUG CRUSH     "
    putStrLn "===================="
    putStrLn "1-Iniciar Jogo"
    putStrLn "2-Regras"
    putStrLn "3-Instruções" 
    putStrLn "4-Sair"
    putStrLn "Escolha uma opção: "
    hFlush stdout
    
    input <- getLine
    case reads input :: [(Int, String)] of
        [(opcao, "")] -> return opcao
        _ -> do
            putStrLn "Opção inválida. Pressione ENTER e tente novamente."
            _ <- getLine
            menuInicial

--4. Criação da tela de login acessada antes de iniciar o jogo
telaLogin :: IO String 
telaLogin = do 
    limparTela
    putStrLn "===== LOGIN ====="
    putStrLn ""
    putStr "Digite o seu nome: "
    hFlush stdout
    nome <- getLine
    putStrLn ""
    putStrLn ("Bem vindo(a), " ++ nome ++ "!")
    putStrLn "Pressione ENTER para iniciar o jogo..."
    _ <- getLine
    return nome

--5. Exibe a tela com as regras do jogo
telaRegras :: IO ()
telaRegras = do
    limparTela
    putStrLn "===== REGRAS ===== "
    putStrLn "1-Troque duas peças vizinhas na horizontal ou na vertical."
    putStrLn "2-Forme combinações de 3 ou mais peças iguais."
    putStrLn "3-Cada troca consome um movimento."
    putStrLn "4-A fase termina quando os movimentos acabam ou quando o objetivo do nível for alcançado."
    putStrLn ""
    putStrLn "Pressione [ENTER] para retornar ao Menu Inicial"
    _ <- getLine
    return ()

--5. Exibe as instruções de como o jogador pode realizar uma combinação
telaInstrucoes :: IO ()
telaInstrucoes = do
    limparTela
    putStrLn "===== INSTRUÇÕES ====="
    putStrLn "Digite as coordenadas das peças para trocar suas posições."
    putStrLn "Formato: linha1 coluna1 linha2 coluna2"
    putStrLn "Exemplo: 1 2 1 3"
    putStrLn ""
    putStrLn "Pressione [ENTER] para retornar ao Menu Inicial"
    _ <- getLine
    return ()


-- Define a largura interna da caixa (sem contar as bordas laterais)
boxWidth :: Int
boxWidth = 40 

-- Função auxiliar para criar uma linha formatada
-- Ela pega o label ("Pontos: ") e o valor ("100") e preenche o meio com espaços
formatLine :: String -> String -> String
formatLine label value = 
    let contentLen = length label + length value
        paddingLen = boxWidth - contentLen
        padding    = replicate paddingLen ' ' -- Cria os espaços que faltam
    in "║ " ++ label ++ value ++ padding ++ " ║"

--6. Renderiza a HUD (informações do jogador durante o jogo)
--Neste momento a HUD já suporta exibição de pontos e movimentos
--restantes, mesmo que a lógica da atualização de pontos e movimentos
--ainda esteja sendo implementada no módulo de lógica
renderHUD :: String -> Int -> Int -> IO ()
renderHUD nome pontos movimentos = do
    putStrLn $ "╔" ++ replicate (boxWidth + 2) '═' ++ "╗"
    
    let titulo = "BUG CRUSH"
    let padTit = replicate ((boxWidth - length titulo) `div` 2) ' '
    
    putStrLn $ "║ " ++ padTit ++ titulo ++ padTit ++ "  ║" 
    putStrLn $ "╠" ++ replicate (boxWidth + 2) '═' ++ "╣"
    putStrLn $ formatLine "Jogador: " nome
    putStrLn $ formatLine "Pontos:  " (show pontos)
    putStrLn $ formatLine "Movimentos Restantes: " (show movimentos) 
    putStrLn $ "╚" ++ replicate (boxWidth + 2) '═' ++ "╝"
    putStrLn ""

--7. Tela de fim de jogo
--Exibe o nome do jogador e a pontuação final 
--Ainda não avalia vitória ou derrota
--trabalha apenas com o fim dos movimentos(arbitrário)
telaGameOver :: String -> Int -> IO ()
telaGameOver nome pontos = do
    limparTela
    putStrLn "╔═══════════════════════════════════╗"
    putStrLn "║           FIM DE JOGO             ║"
    putStrLn "╚═══════════════════════════════════╝"
    putStrLn (" Jogador: " ++ nome)
    putStrLn (" Pontuação final: " ++ show pontos)
    putStrLn ""
    putStrLn " Pressione [ENTER] para voltar ao menu"
    _ <- getLine
    return ()