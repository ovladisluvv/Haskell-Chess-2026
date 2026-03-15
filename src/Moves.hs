module Moves where

import Types
import Board
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (isNothing)

-- /--- Вспомогательная библиотека для генерации ходов 
-- Возможные смещения для Коня
knightOffsets :: [Offsets]
knightOffsets = [Offsets 1 2, Offsets 2 1, Offsets 2 (-1), Offsets 1 (-2), Offsets (-1) (-2), Offsets (-2) (-1), Offsets (-2) 1, Offsets (-1) 2]

-- Возможные смещения для Короля
kingOffsets :: [Offsets]
kingOffsets = [Offsets 0 1, Offsets 1 1, Offsets 1 0, Offsets 1 (-1), Offsets 0 (-1), Offsets (-1) (-1), Offsets (-1) 0, Offsets (-1) 1]

-- Направления для Ладьи
rookDirs :: [Directions]
rookDirs = [Directions 0 1, Directions 1 0, Directions 0 (-1), Directions (-1) 0]
 
-- Направления для Слона
bishopDirs :: [Directions]
bishopDirs = [Directions 1 1, Directions 1 (-1), Directions (-1) (-1), Directions (-1) 1]

-- Направления для Ферзя
queenDirs :: [Directions]
queenDirs = rookDirs ++ bishopDirs
-- \---

-- /--- Библиотека генерации ходов
-- Генерация ходов для Коня и Короля. Применяет фиксированные смещения с проверкой валидности хода
stepMoves :: Board -> Pos -> Color -> [Offsets] -> [Pos]
stepMoves b pos color offsets = filter isValidStep (map nextPos offsets)
    where
        isValidStep piecePos = isValidPos piecePos && notOwnPiece piecePos

        notOwnPiece piecePos = case getPiece b piecePos of
            Nothing -> True
            Just (Piece _ pieceColor) -> pieceColor /= color

        nextPos (Offsets dx dy) = Pos (file pos + dx) (rank pos + dy)

-- Генерация ходов для Ладьи, Слона и Ферзя. Продолжает движение по лучу, пока не встретит край доски или фигуру
slideMoves :: Board -> Pos -> Color -> [Directions] -> [Pos]
slideMoves b pos color dirs = concatMap (slide pos) dirs
    where
        slide curPos (Directions dx dy)
            | isValidPos nextPos = case getPiece b nextPos of
                Nothing -> nextPos : slide nextPos (Directions dx dy)
                Just (Piece _ pieceColor) -> [nextPos | pieceColor /= color]
            | otherwise = []
            where nextPos = Pos (file curPos + dx) (rank curPos + dy)

-- Генерация ходов для пешек. Учитывает направление движения, начальную позицию для двойного шага и возможность взятия по диагонали
pawnMoves :: Board -> Pos -> Color -> [Pos]
pawnMoves b (Pos f r) color = forwardMoves ++ filter captureMoves [Pos (f - 1) (r + dir), Pos (f + 1) (r + dir)]
    where
        captureMoves piecePos = isValidPos piecePos && hasOpponentPiece piecePos
        
        (dir, startRank) = case color of
            White -> (1, 1)
            Black -> (-1, 6)

        forward1 = Pos f (r + dir)
        forward2 = Pos f (r + 2 * dir)

        isPathClear piecePos = isValidPos piecePos && isNothing (getPiece b piecePos)

        forwardMoves
            | isPathClear forward1 && (r == startRank && isPathClear forward2) = [forward1, forward2]
            | isPathClear forward1 = [forward1]
            | otherwise = []
        
        hasOpponentPiece piecePos = case getPiece b piecePos of
            Just (Piece _ pieceColor) -> pieceColor /= color
            Nothing -> False

-- Генерация ходов взятия на проходе для пешек. Проверяет, соответствует ли цель взятия на проходе и находится ли она по диагонали от пешки
enPassantMoves :: GameState -> Pos -> Color -> [Pos]
enPassantMoves gs pos color = case enPassantTarget gs of
    Nothing -> []
    Just epTarget
        | isDiagonal pos epTarget color -> [epTarget]
        | otherwise -> []
    where
        isDiagonal (Pos f1 r1) (Pos f2 r2) White = abs (f1 - f2) == 1 && r2 - r1 == 1
        isDiagonal (Pos f1 r1) (Pos f2 r2) Black = abs (f1 - f2) == 1 && r1 - r2 == 1

-- Получение всех возможных ходов для конкретной фигуры
piecePossibleMoves :: GameState -> Pos -> [Move]
piecePossibleMoves gs pos = case getPiece b pos of
    Nothing -> []
    Just (Piece pieceType color) -> case pieceType of
        Pawn -> concatMap (makePawnMove color) (pawnMoves b pos color ++ enPassantMoves gs pos color)
        Knight -> map makeMove (stepMoves b pos color knightOffsets)
        Bishop -> map makeMove (slideMoves b pos color bishopDirs)
        Rook -> map makeMove (slideMoves b pos color rookDirs)
        Queen -> map makeMove (slideMoves b pos color queenDirs)
        King -> map makeMove (stepMoves b pos color kingOffsets)

    where
        b = board gs

        makeMove target = Move pos target Nothing

        makePawnMove color target 
            | (rank target == 7 && color == White) || (rank target == 0 && color == Black) = [ Move pos target (Just Queen), 
                                                                                               Move pos target (Just Rook),
                                                                                               Move pos target (Just Bishop),
                                                                                               Move pos target (Just Knight) ]
            | otherwise = [Move pos target Nothing]

-- Генерация всех возможных ходов для активного игрока
allPossibleMoves :: GameState -> [Move]
allPossibleMoves gs = concatMap getMovesForPos [Pos f r | f <- [0..7], r <- [0..7]]
    where
        getMovesForPos piecePos = case getPiece (board gs) piecePos of
            Just (Piece _ pieceColor) | pieceColor == activePlayer gs -> piecePossibleMoves gs piecePos
            _ -> []

-- ToDo : Реализовать превращение пешки 
-- ToDo : Реализовать рокировку
-- ToDo : Реализовать взятие на проходе
-- \---
