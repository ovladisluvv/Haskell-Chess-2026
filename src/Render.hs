module Render
    ( windowSize
    , drawGame
    ) where

import Graphics.Gloss
import Data.Maybe (fromMaybe)

import Types (Piece(..), PieceType(..), GameState(..), Board, Pos(..))
import qualified Types as T
import Board (getPiece)                             

-- \\-- Константы для рендеринга
-- Размер одной клетки в пикселях
squareSize :: Float
squareSize = 100

-- Размер окна (8 клеток)
windowSize :: Int
windowSize = round (squareSize * 8)

-- Цвета клеток доски
lightSquare :: Color
lightSquare = makeColorI 240 217 181 255

darkSquare :: Color
darkSquare = makeColorI 181 136 99 255
-- \\--

-- \\-- Перевод координаты доски (0–7) в экранную координату Gloss
-- Gloss: (0,0) — центр экрана
toScreenX :: Int -> Float
toScreenX f = fromIntegral f * squareSize - fromIntegral windowSize / 2 + squareSize / 2

toScreenY :: Int -> Float
toScreenY r = fromIntegral r * squareSize - fromIntegral windowSize / 2 + squareSize / 2
-- \\ --

-- \\-- Главная функция, собирает всю сцену
drawGame :: [(String, Picture)] -> GameState -> Picture
drawGame imgs state = Pictures [drawBoard, drawPieces imgs (board state), drawLabels]

-- Рисует 64 клетки шахматной доски
drawBoard :: Picture
drawBoard = Pictures [drawSquare f r | f <- [0..7], r <- [0..7]]
  where
    drawSquare f r =
        Translate (toScreenX f) (toScreenY r) $
        Color (cellColor f r) $
        Polygon [(-s, -s), (s, -s), (s, s), (-s, s)]
      where
        s = squareSize / 2

    -- a1 (0,0) — тёмная клетка
    cellColor f r
      | even (f + r) = darkSquare
      | otherwise    = lightSquare

-- Рисует все фигуры на доске
drawPieces :: [(String, Picture)] -> Board -> Picture
drawPieces imgs b = Pictures [drawAt f r | f <- [0..7], r <- [0..7]]
  where
    drawAt f r = case getPiece b (Pos f r) of
        Nothing -> Blank
        Just p  -> Translate (toScreenX f) (toScreenY r) (renderPiece imgs p)

-- Рисует одну фигуру
renderPiece :: [(String, Picture)] -> Piece -> Picture
renderPiece imgs (Piece pt pc) = 
    fromMaybe Blank (lookup imageName imgs) 
  where 
    imageName = colorPrefix ++ pieceSuffix pt

    colorPrefix = case pc of
        T.White -> "w"
        T.Black -> "b"

    pieceSuffix Pawn   = "p"
    pieceSuffix Knight = "n"
    pieceSuffix Bishop = "b"
    pieceSuffix Rook   = "r"
    pieceSuffix Queen  = "q"
    pieceSuffix King   = "k"

-- Рисует подписи координат (a–h, 1–8)
drawLabels :: Picture
drawLabels = Pictures (fileLbls ++ rankLbls)
  where
    lblColor = makeColorI 80 80 80 255
    edge     = fromIntegral windowSize / 2
    
    offset   = 5

    fileLbls = [Translate (toScreenX f - squareSize/2 + offset) (-edge + offset) $
                Scale 0.12 0.12 $ Color lblColor $ Text [fileChar f]
               | f <- [0..7]]

    rankLbls = [Translate (-edge + offset) (toScreenY r + squareSize/2 - 15) $
                Scale 0.12 0.12 $ Color lblColor $ Text (show (r + 1))
               | r <- [0..7]]

    fileChar f = toEnum (fromEnum 'a' + f)

-- \-- 