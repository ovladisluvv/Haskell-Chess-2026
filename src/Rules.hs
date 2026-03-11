module Rules where

import Types
import Board
import Moves (allPossibleMoves)

--- /--- Библиотека для проверки правил шахматной игры
-- Применение хода к состоянию игры: обновление доски, смена активного игрока и увеличение счетчика ходов после хода черных
applyMove :: GameState -> Move -> GameState
applyMove gs move = gs { board = newBoard,
                         activePlayer = oppositeColor (activePlayer gs),
                         moveNumber = moveNumber gs + turnInc (activePlayer gs)
                       }
    where
        newBoard = movePiece (board gs) (moveFrom move) (moveTo move)

        turnInc White = 0
        turnInc Black = 1

-- Поиск короля заданного цвета на доске. Проходит по всем позициям и возвращает позицию, на которой находится король
findKing :: Board -> Color -> Pos
findKing b color = head (filter isKing [Pos f r | f <- [0..7], r <- [0..7]])
    where
        isKing piecePos = case getPiece b piecePos of
            Just (Piece King pieceColor) -> pieceColor == color
            _ -> False

-- Проверка на шах
isCheck :: GameState -> Color -> Bool
isCheck gs color = kingPos `elem` (map moveTo opponentMoves)
    where
        kingPos = findKing (board gs) color
        
        -- Эмуляция очереди хода оппонента для получения всех клеток под атакой
        tempGs = gs { activePlayer = oppositeColor color }
        opponentMoves = allPossibleMoves tempGs

-- Проверка легальности хода. Ход легален, если после его совершения король ходившего игрока не находится под шахом
isMoveLegal :: GameState -> Move -> Bool
isMoveLegal gs move = not (isCheck (applyMove gs move) (activePlayer gs))

--Генерация всех легальных ходов для активного игрока
allLegalMoves :: GameState -> [Move]
allLegalMoves gs = filter (isMoveLegal gs) (allPossibleMoves gs)

-- Проверка на мат
isCheckmate :: GameState -> Bool
isCheckmate gs = null (allLegalMoves gs) && isCheck gs (activePlayer gs)

-- Проверка на пат
isStalemate :: GameState -> Bool
isStalemate gs = null (allLegalMoves gs) && not (isCheck gs (activePlayer gs))

-- ToDo : Реализовать проверку на ничью по правилу 50 ходов
-- ToDo : Реализовать проверку на ничью по повторению позиции
-- \---
