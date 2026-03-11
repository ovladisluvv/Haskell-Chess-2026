module Moves where

import Types
import Board
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (isNothing)

-- /--- Вспомогательная библиотека для генерации ходов 
-- Возможные смещения для Коня
knightOffsets :: [(Int, Int)]
knightOffsets = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

-- Возможные смещения для Короля
kingOffsets :: [(Int, Int)]
kingOffsets = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

-- Направления для Ладьи
rookDirs :: [(Int, Int)]
rookDirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]
 
-- Направления для Слона
bishopDirs :: [(Int, Int)]
bishopDirs = [(1, 1), (1, -1), (-1, -1), (-1, 1)]

-- Направления для Ферзя
queenDirs :: [(Int, Int)]
queenDirs = rookDirs ++ bishopDirs
-- \---

-- /--- Библиотека генерации ходов
-- Генерация ходов для Коня и Короля. Применяет фиксированные смещения с проверкой валидности хода
stepMoves :: Board -> Pos -> Color -> [(Int, Int)] -> [Pos]
stepMoves b pos color offsets = filter (isValidPos piecePos && notOwnPiece piecePos) (map nextPos offsets)
    where
        notOwnPiece piecePos = case (getPiece b piecePos) of
            Nothing -> True
            Just (Piece _ pieceColor) -> pieceColor /= color

        nextPos (dx, dy) = Pos (file pos + dx) (rank pos + dy)

-- Генерация ходов для Ладьи, Слона и Ферзя. Продолжает движение по лучу, пока не встретит край доски или фигуру
slideMoves :: Board -> Pos -> Color -> [(Int, Int)] -> [Pos]
slideMoves b pos color dirs = concatMap (slide pos) dirs
    where
        slide curPos (dx, dy)
            | isValidPos nextPos = case (getPiece b nextPos) of
                Nothing -> nextPos : slide nextPos (dx, dy)
                Just (Piece _ pieceColor) -> [nextPos | pieceColor /= color]
            | otherwise = []
            where nextPos = Pos (file curPos + dx) (rank curPos + dy)

-- Генерация ходов для пешек. Учитывает направление движения, начальную позицию для двойного шага и возможность взятия по диагонали
pawnMoves :: Board -> Pos -> Color -> [Pos]
pawnMoves b (Pos f r) color = forwardMoves ++ filter (isValidPos piecePos && hasOpponentPiece piecePos) [Pos (f - 1) (r + dir), Pos (f + 1) (r + dir)]
    where
        (dir, startRank) = case color of
            White -> (1, 1)
            Black -> (-1, 6)

        forward1 = Pos f (r + dir)
        forward2 = Pos f (r + 2 * dir)

        isPathClear piecePos = (isValidPos piecePos) && (isNothing (getPiece b piecePos))

        forwardMoves
            | (isPathClear forward1) && (r == startRank && (isPathClear forward2)) = [forward1, forward2]
            | isPathClear forward1 = [forward1]
            | otherwise = []
        
        hasOpponentPiece piecePos = case (getPiece b piecePos) of
            Just (Piece _ pieceColor) -> pieceColor /= color
            Nothing -> False

-- Получение всех возможных ходов для конкретной фигуры
piecePossibleMoves :: Board -> Pos -> [Move]
piecePossibleMoves b pos = case (getPiece b pos) of
    Nothing -> []
    Just (Piece pieceType color) -> map makeMove pieceTypes
        where
            pieceTypes = case pieceType of
                Pawn -> pawnMoves b pos color
                Knight -> stepMoves b pos color knightOffsets
                Bishop -> slideMoves b pos color bishopDirs
                Rook -> slideMoves b pos color rookDirs
                Queen -> slideMoves b pos color queenDirs
                King -> stepMoves b pos color kingOffsets
            
            makeMove target = Move pos target Nothing -- Nothing для пешки, пока не реализовано превращение

-- Генерация всех возможных ходов для активного игрока
allPossibleMoves :: GameState -> [Move]
allPossibleMoves gs = concatMap getMovesForPos [Pos f r | f <- [0..7], r <- [0..7]]
    where
        getMovesForPos piecePos = case (getPiece (board gs) piecePos) of
            Just (Piece _ pieceColor) | pieceColor == (activePlayer gs) -> piecePossibleMoves (board gs) piecePos
            _ -> []

-- ToDo : Реализовать превращение пешки 
-- ToDo : Реализовать рокировку
-- ToDo : Реализовать взятие на проходе
-- \---
