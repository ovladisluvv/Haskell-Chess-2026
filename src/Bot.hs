module Bot (isBotTurn, makeBotMove) where

import Types
import Rules (allLegalMoves, makeMove, isCheckmate, isDraw)
import Board (getPiece)

-- /--- Библиотека для логики бота (Minimax)
-- Функция для получения материальной ценности фигуры
getMaterialValue :: PieceType -> Int
getMaterialValue Pawn = 10
getMaterialValue Knight = 30
getMaterialValue Bishop = 30
getMaterialValue Rook = 50
getMaterialValue Queen = 90
getMaterialValue King = 9000

-- Вспомогательная функция для расчета оценки позиции. Возвращает число > 0 в пользу белых, < 0 в пользу черных
evaluatePosition :: GameState -> Int
evaluatePosition gs = sum (map pieceValue allPieces)
    where
        allPieces = concatMap getPieces [Pos f r | f <- [0..7], r <- [0..7]]
        
        getPieces pos = case getPiece (board gs) pos of
            Just piece -> [piece]
            Nothing -> []

        pieceValue (Piece pieceType White) = getMaterialValue pieceType
        pieceValue (Piece pieceType Black) = -(getMaterialValue pieceType)

-- Функция-максимизатор для алгоритма Minimax. Возвращает лучшую оценку для белых
maxi :: GameState -> Int -> Int
maxi gs 0 = evaluatePosition gs
maxi gs depth
    | null (allLegalMoves gs) = evaluatePosition gs
    | otherwise = maximum [mini (makeMove gs m) (depth - 1) | m <- allLegalMoves gs]

-- Функция-минимизатор для алгоритма Minimax. Возвращает лучшую оценку для черных
mini :: GameState -> Int -> Int
mini gs 0 = evaluatePosition gs
mini gs depth
    | null (allLegalMoves gs) = evaluatePosition gs
    | otherwise = minimum [maxi (makeMove gs m) (depth - 1) | m <- allLegalMoves gs]

-- Главная функция для получения лучшего хода бота. Выбирает ход с лучшей оценкой после применения Minimax с заданной глубиной
getBestMove :: GameState -> Int -> Move
getBestMove gs depth
    | activePlayer gs == White = snd (maximum whiteScore)
    | otherwise = snd (minimum blackScore)
    where
        whiteScore = [(mini (makeMove gs m) (depth - 1), m) | m <- allLegalMoves gs]
        blackScore = [(maxi (makeMove gs m) (depth - 1), m) | m <- allLegalMoves gs]
-- \---

-- /--- Библиотека для ходов бота
-- Проверяет, ход ли сейчас бота
isBotTurn :: GameState -> Bool
isBotTurn state = menuState state == Hidden &&
                  not (isCheckmate state || isDraw state) &&
                  Just (activePlayer state) == botColor state

-- Совершает ход бота на основе алгоритма Minimax
makeBotMove :: GameState -> GameState
makeBotMove gs
    | null (allLegalMoves gs) = gs
    | otherwise = newGs { promotionState = Nothing, selectedPos = Nothing }
    where
        newGs = makeMove gs bestMove
        bestMove = getBestMove gs 3 -- 3 - глубина поиска
-- \---
