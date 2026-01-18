module Types where

-- Tipos de Insetos
data Bug = Red | Blue | Green | Yellow | Purple | Empty
    deriving (Eq, Show)

-- O Tabuleiro
type Board = [[Bug]]

-- Coordenadas (Linha, Coluna)
type Coord = (Int, Int)

-- Constantes Globais
width, height :: Int
width = 8
height = 8