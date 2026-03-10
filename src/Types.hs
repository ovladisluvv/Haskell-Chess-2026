module Types where

import Data.Vector (Vector)

-- Цвет шахматной фигуры: белый или черный
data Color = White | Black deriving (Show, Eq)

-- Меняет цвет на противоположный
oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

-- Тип шахматной фигуры: пешка, конь, слон, ладья, ферзь или король
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

-- Шахматная фигура. Включает тип и цвет
data Piece = Piece { pieceType :: PieceType, 
                     pieceColor :: Color 
                   } deriving (Show, Eq)

-- Позиция на шахматной доске, представленная в виде структуры с полями для столбца и ряда
data Pos = Pos { file :: Int, -- Столбец на шахматной доске, от 0 до 7 [0 = 'a', 1 = 'b', ..., 7 = 'h']
                 rank :: Int  -- Ряд на шахматной доске, от 0 до 7 [0 = '1', 1 = '2', ..., 7 = '8']
               } deriving (Show, Eq, Ord)

-- Шахматная доска, представленная в виде отображения позиций в фигуры
type Board = Vector (Maybe Piece)

-- Ход в фигуры. Включает начальную и конечную позицию, информацию о том, в какую фигуру превратилась пешка, дошедшая до края доски (если это был ход пешкой)
data Move = Move { moveFrom :: Pos,
                   moveTo :: Pos,
                   movePromote :: Maybe PieceType -- Nothing, если ход не связан с превращением пешки, Just PieceType - тип фигуры, в которую превращается пешка
                 } deriving (Show, Eq)

-- Состояние игры. Включает текущую позицию на доске, активного игрока и номер хода
data GameState = GameState { board :: Board,
                             activePlayer :: Color,
                             moveNumber :: Int -- Номер хода (увеличивается после хода черных)
                           } deriving (Show, Eq)