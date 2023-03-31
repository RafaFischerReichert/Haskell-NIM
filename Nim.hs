module Nim where

-- Função que verifica se o jogo acabou
gameOver :: [Int] -> Bool
gameOver = all (== 0)

-- Função que imprime o tabuleiro
printBoard :: [Int] -> IO ()
printBoard xs = putStrLn $ unlines $ map (\n -> replicate n '*') xs

-- Função que realiza a jogada
move :: [Int] -> Int -> Int -> [Int]
move board row numToRemove = take row board ++ [board !! row - numToRemove] ++ drop (row + 1) board

-- Função que lê a jogada do usuário
getMove :: IO (Int, Int)
getMove = do
  putStrLn "Digite o número da linha e o número de asteriscos a serem removidos, separados por um espaço:"
  input <- getLine
  let [row, numToRemove] = map read $ words input
  return (row - 1, numToRemove)

-- Função que joga uma rodada
playRound :: Int -> [Int] -> IO ()
playRound player board = do
  putStrLn $ "Jogador " ++ show player ++ ", é a sua vez:"
  printBoard board
  (row, numToRemove) <- getMove
  let newBoard = move board row numToRemove
  if gameOver newBoard
    then do
      putStrLn $ "Jogador " ++ show player ++ " venceu!"
      printBoard newBoard
    else playRound (3 - player) newBoard

-- Função que inicia o jogo
playGame :: IO ()
playGame = do
  putStrLn "Bem-vindo ao jogo Nim!"
  let board = [5, 4, 3, 2, 1]
  playRound 1 board