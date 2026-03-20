module Moves where

import Types
import Board
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
        isValidStep pPos = isValidPos pPos && notOwnPiece pPos

        notOwnPiece pPos = case getPiece b pPos of
            Nothing -> True
            Just (Piece _ pColor) -> pColor /= color

        nextPos (Offsets dx dy) = Pos (file pos + dx) (rank pos + dy)

-- Генерация ходов для Ладьи, Слона и Ферзя. Продолжает движение по лучу, пока не встретит край доски или фигуру
slideMoves :: Board -> Pos -> Color -> [Directions] -> [Pos]
slideMoves b pos color dirs = concatMap (slide pos) dirs
    where
        slide curPos (Directions dx dy)
            | isValidPos nextPos = case getPiece b nextPos of
                Nothing -> nextPos : slide nextPos (Directions dx dy)
                Just (Piece _ pColor) -> [nextPos | pColor /= color]
            | otherwise = []
            where nextPos = Pos (file curPos + dx) (rank curPos + dy)

-- Генерация ходов для пешек. Учитывает направление движения, начальную позицию для двойного шага и возможность взятия по диагонали
pawnMoves :: Board -> Pos -> Color -> [Pos]
pawnMoves b (Pos f r) color = forwardMoves ++ filter captureMoves [Pos (f - 1) (r + dir), Pos (f + 1) (r + dir)]
    where
        captureMoves pPos = isValidPos pPos && hasOpponentPiece pPos
        
        (dir, startRank) = case color of
            White -> (1, 1)
            Black -> (-1, 6)

        forward1 = Pos f (r + dir)
        forward2 = Pos f (r + 2 * dir)

        isPathClear pPos = isValidPos pPos && isNothing (getPiece b pPos)

        forwardMoves
            | isPathClear forward1 && (r == startRank && isPathClear forward2) = [forward1, forward2]
            | isPathClear forward1 = [forward1]
            | otherwise = []
        
        hasOpponentPiece pPos = case getPiece b pPos of
            Just (Piece _ pColor) -> pColor /= color
            Nothing -> False

-- Вспомогательная функция для генерации ходов взятия на проходе. Проверяет, находится ли цель взятия на проходе по диагонали от пешки
isDiagonal :: Pos -> Pos -> Color -> Bool
isDiagonal (Pos f1 r1) (Pos f2 r2) White = abs (f1 - f2) == 1 && r2 - r1 == 1
isDiagonal (Pos f1 r1) (Pos f2 r2) Black = abs (f1 - f2) == 1 && r1 - r2 == 1

-- Генерация ходов взятия на проходе для пешек. Проверяет, соответствует ли цель взятия на проходе и находится ли она по диагонали от пешки
enPassantMoves :: GameState -> Pos -> Color -> [Pos]
enPassantMoves gs pos color = case enPassantTarget gs of
    Nothing -> []
    Just epTarget
        | isDiagonal pos epTarget color -> [epTarget]
        | otherwise -> []

-- Вспомогательная функция для получения возможных рокировок. Выдает начальную горизонталь короля в зависимости от цвета
homeRank :: Color -> Int
homeRank White = 0
homeRank Black = 7

-- Вспомогательная функция для проверки прав на короткую рокировку
canCastleKSide :: Color -> CastlingRights -> Bool
canCastleKSide White = whiteKingSide
canCastleKSide Black = blackKingSide

-- Вспомогательная функция для проверки прав на длинную рокировку
canCastleQSide :: Color -> CastlingRights -> Bool
canCastleQSide White = whiteQueenSide
canCastleQSide Black = blackQueenSide

-- Генерация возможных ходов рокировки для короля. Проверяет наличие прав на рокировку, отсутствие фигур между королем и ладьей, а также отсутствие шаха на пути короля
castlingMoves :: GameState -> Color -> [Pos]
castlingMoves gs color = castleKSide ++ castleQSide
    where
        isClear f = isNothing (getPiece (board gs) (Pos f (homeRank color)))
        
        castleKSide
            | canCastleKSide color (castlingRights gs) && isClear 5 && isClear 6 = [Pos 6 (homeRank color)]
            | otherwise = []
            
        castleQSide
            | canCastleQSide color (castlingRights gs) && isClear 1 && isClear 2 && isClear 3 = [Pos 2 (homeRank color)]
            | otherwise = []

-- Получение всех возможных ходов для конкретной фигуры
piecePossibleMoves :: GameState -> Pos -> [Move]
piecePossibleMoves gs pos = case getPiece b pos of
    Nothing -> []
    Just (Piece pType color) -> case pType of
        Pawn -> concatMap (makePawnMove color) (pawnMoves b pos color ++ enPassantMoves gs pos color)
        Knight -> map makeMove (stepMoves b pos color knightOffsets)
        Bishop -> map makeMove (slideMoves b pos color bishopDirs)
        Rook -> map makeMove (slideMoves b pos color rookDirs)
        Queen -> map makeMove (slideMoves b pos color queenDirs)
        King -> map makeMove (stepMoves b pos color kingOffsets ++ castlingMoves gs color)

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
        getMovesForPos pPos = case getPiece (board gs) pPos of
            Just (Piece _ pColor) | pColor == activePlayer gs -> piecePossibleMoves gs pPos
            _ -> []
-- \---
