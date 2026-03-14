module Rules where

import Types
import Board
import Moves (allPossibleMoves)

--- /--- Библиотека для проверки правил шахматной игры
-- Применение хода к состоянию игры: обновление доски, смена активного игрока и увеличение счетчика ходов после хода черных
applyMove :: GameState -> Move -> GameState
applyMove gs move = gs { board = finalBoard,
                         activePlayer = oppositeColor (activePlayer gs),
                         moveNumber = moveNumber gs + turnInc (activePlayer gs),
                         halfMoveCount = updatedHalfMoveCount
                       }
    where
        newBoard = movePiece (board gs) (moveFrom move) (moveTo move)

        finalBoard = case movePromote move of
            Just promotedPieceType -> setPiece newBoard (moveTo move) (Just (Piece promotedPieceType (activePlayer gs)))
            Nothing -> newBoard

        turnInc White = 0
        turnInc Black = 1

        updatedHalfMoveCount
            | isCapture || isPawnMove = 0
            | otherwise = halfMoveCount gs + 1

        isPawnMove = case getPiece (board gs) (moveFrom move) of
            Just (Piece Pawn _) -> True
            _ -> False
            
        isCapture = case getPiece (board gs) (moveTo move) of
            Just _ -> True
            Nothing -> False

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

-- Проверка на ничью
isDraw :: GameState -> Bool
isDraw gs = isStalemate gs || isInsufficientMaterial gs || isFiftyMoveRule gs

-- Проверка на пат
isStalemate :: GameState -> Bool
isStalemate gs = null (allLegalMoves gs) && not (isCheck gs (activePlayer gs))

-- Проверка на ничью по недостатку материала
isInsufficientMaterial :: GameState -> Bool
isInsufficientMaterial gs = case filter notKing allPieces of
    [] -> True -- Король против Короля
    [(_, Piece Knight _)] -> True -- Король и Конь против Короля
    [(_, Piece Bishop _)] -> True -- Король и Слон против Короля
    [(pos1, Piece Knight _), (pos2, Piece Knight _)] -> True -- Король и два коня против Короля
    [(pos1, Piece Bishop _), (pos2, Piece Bishop _)] -> isSameColor pos1 pos2 -- Два слона на одноцветных полях
    _ -> False -- Во всех остальных случаях материала достаточно
    where
        notKing (_, p) = pieceType p /= King

        allPieces = concatMap getPosAndPiece [Pos f r | f <- [0..7], r <- [0..7]]
        
        getPosAndPiece pos = case getPiece (board gs) pos of
            Just p -> [(pos, p)]
            Nothing -> []

        isSameColor (Pos f1 r1) (Pos f2 r2) = ((f1 + r1) `mod` 2) == ((f2 + r2) `mod` 2)

-- Проверка на ничью по правилу 50 ходов
isFiftyMoveRule :: GameState -> Bool
isFiftyMoveRule gs = halfMoveCount gs >= 100
-- \---
