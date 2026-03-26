module Rules where

import Types
import Board
import Moves

--- /--- Библиотека для проверки правил шахматной игры
-- Вспомотельная функция для применения хода. Увеличивает счетчик ходов после хода черных
turnInc :: Color -> Int
turnInc White = 0
turnInc Black = 1

-- Вспомогательная функция для применения хода. Определяет направление движения пешки в зависимости от цвета
pawnPushDir :: Color -> Int
pawnPushDir White = 1
pawnPushDir Black = -1

-- Применение хода к состоянию игры
makeMove gs move = gs { board = finalBoard,
                         activePlayer = oppositeColor (activePlayer gs),
                         moveNumber = moveNumber gs + turnInc (activePlayer gs),
                         halfMoveCount = updatedHalfMoveCount,
                         enPassantTarget = newEnPassantTarget,
                         castlingRights = updatedCastlingRights,
                         gameStory = gameStory gs ++ [newMoveRecord]
                       }
    where
        movedBoard
            | isCastling gs move = movePiece (movePiece (board gs) (moveFrom move) (moveTo move)) rookFrom rookTo
            | otherwise = movePiece (board gs) (moveFrom move) (moveTo move)

        (rookFrom, rookTo) = case file (moveTo move) of
            6 -> (Pos 7 (rank (moveTo move)), Pos 5 (rank (moveTo move))) -- Короткая рокировка
            2 -> (Pos 0 (rank (moveTo move)), Pos 3 (rank (moveTo move))) -- Длинная рокировка

        boardAfterEp
            | isPawnMove && Just (moveTo move) == enPassantTarget gs = setPiece movedBoard enPassantedPawnPos Nothing
            | otherwise = movedBoard

        enPassantedPawnPos = Pos (file (moveTo move)) (rank (moveFrom move))

        finalBoard = case movePromote move of
            Just promotedPiece -> setPiece boardAfterEp (moveTo move) (Just (Piece promotedPiece (activePlayer gs)))
            Nothing -> boardAfterEp

        updatedHalfMoveCount
            | isPawnMove || isCapture = 0
            | otherwise = halfMoveCount gs + 1

        isPawnMove = case getPiece (board gs) (moveFrom move) of
            Just (Piece Pawn _) -> True
            _ -> False
            
        isCapture = case getPiece (board gs) (moveTo move) of
            Just _ -> True
            Nothing -> False

        isEpCapture = isPawnMove && Just (moveTo move) == enPassantTarget gs

        newEnPassantTarget
            | isPawnMove && abs (rank (moveTo move) - rank (moveFrom move)) == 2 = 
                Just (Pos (file (moveFrom move)) (rank (moveFrom move) + pawnPushDir (activePlayer gs)))
            | otherwise = Nothing

        updatedCastlingRights = CastlingRights { whiteKingSide = whiteKingSide (castlingRights gs) && keepWhiteKingside,
                                                 whiteQueenSide = whiteQueenSide (castlingRights gs) && keepWhiteQueenside,
                                                 blackKingSide = blackKingSide (castlingRights gs) && keepBlackKingside,
                                                 blackQueenSide = blackQueenSide (castlingRights gs) && keepBlackQueenside
                                               }

        -- Шаблон проверки: не двигался король && нужная ладья не двигалась && нужная ладья не была съедена 
        keepWhiteKingside = (moveFrom move) /= Pos 4 0 && (moveFrom move) /= Pos 7 0 && (moveTo move) /= Pos 7 0
        keepWhiteQueenside = (moveFrom move) /= Pos 4 0 && (moveFrom move) /= Pos 0 0 && (moveTo move) /= Pos 0 0
        keepBlackKingside = (moveFrom move) /= Pos 4 7 && (moveFrom move) /= Pos 7 7 && (moveTo move) /= Pos 7 7
        keepBlackQueenside = (moveFrom move) /= Pos 4 7 && (moveFrom move) /= Pos 0 7 && (moveTo move) /= Pos 0 7

        curCapturedPiece
            | isEpCapture = getPiece (board gs) enPassantedPawnPos
            | otherwise = getPiece (board gs) (moveTo move)

        newMoveRecord = MoveRecord { playedMove = move,
                                     undoInfo = undoState
                                   }

        undoState = UndoInfo { capturedPiece = curCapturedPiece,
                               prevHalfMoveCount = halfMoveCount gs,
                               prevEnPassantTarget = enPassantTarget gs,
                               prevCastlingRights = castlingRights gs
                             }

-- Отмена последнего хода с восстановлением предыдущего состояния игры
unmakeMove :: GameState -> GameState
unmakeMove gs
    | null (gameStory gs) = gs -- Если история пуста, возвращаем текущее состояние
    | otherwise = gs { board = prevBoard,
                       activePlayer = prevPlayerColor,
                       moveNumber = moveNumber gs - turnInc prevPlayerColor,
                       halfMoveCount = prevHalfMoveCount toUndo,
                       enPassantTarget = prevEnPassantTarget toUndo,
                       castlingRights = prevCastlingRights toUndo,
                       gameStory = take (length (gameStory gs) - 1) (gameStory gs)
                     }
    where
        prevPlayerColor = oppositeColor (activePlayer gs)

        toUndo = undoInfo (last (gameStory gs))

        prevMove = playedMove (last (gameStory gs))

        movedPiece = getPiece (board gs) (moveTo prevMove)

        isPromotion = case movePromote prevMove of
            Just _ -> True
            Nothing -> False

        isEp = movedPiece == Just (Piece Pawn prevPlayerColor) && Just (moveTo prevMove) == prevEnPassantTarget toUndo

        isCastle = movedPiece == Just (Piece King prevPlayerColor) && abs (file (moveTo prevMove) - file (moveFrom prevMove)) == 2
        
        -- Двигаем фигуру обратно на место
        boardAfterMoveBack = movePiece (board gs) (moveTo prevMove) (moveFrom prevMove)

        -- Если было превращение, заменяем новую фигуру обратно на пешку
        boardAfterDemotion 
            | isPromotion = setPiece boardAfterMoveBack (moveFrom prevMove) (Just (Piece Pawn prevPlayerColor))
            | otherwise = boardAfterMoveBack

        -- Восстанавливаем съеденную фигуру, если она была
        boardAfterRestoreCapture = case capturedPiece toUndo of
            Nothing -> boardAfterDemotion
            Just opponentPiece 
                | isEp -> setPiece boardAfterDemotion (Pos (file (moveTo prevMove)) (rank (moveFrom prevMove))) (Just opponentPiece)
                | otherwise -> setPiece boardAfterDemotion (moveTo prevMove) (Just opponentPiece)

        -- Если была рокировка, возвращаем ладью на место
        prevBoard 
            | isCastle && file (moveTo prevMove) == 6 = movePiece boardAfterRestoreCapture (Pos 5 (rank (moveFrom prevMove))) (Pos 7 (rank (moveFrom prevMove))) -- Возврат короткой
            | isCastle && file (moveTo prevMove) == 2 = movePiece boardAfterRestoreCapture (Pos 3 (rank (moveFrom prevMove))) (Pos 0 (rank (moveFrom prevMove))) -- Возврат длинной
            | otherwise = boardAfterRestoreCapture

-- Проверка, является ли ход рокировкой
isCastling :: GameState -> Move -> Bool
isCastling gs move = isKingMove && isLongMove
  where
    isKingMove = case getPiece (board gs) (moveFrom move) of
        Just (Piece King _) -> True
        _ -> False

    isLongMove = abs (file (moveTo move) - file (moveFrom move)) == 2

-- Проверка, находится ли клетка под атакой
isSquareAttacked :: GameState -> Pos -> Color -> Bool
isSquareAttacked gs targetPos color = targetPos `elem` (map moveTo opponentMoves) || isPawnAttacking
    where
        -- Эмуляция очереди хода оппонента для получения всех клеток под атакой
        tempGs = gs { activePlayer = oppositeColor color }

        opponentMoves = allPossibleMoves tempGs

        isPawnAttacking = any hasEnemyPawn pawnAttackPos

        pawnAttackPos = [ Pos (file targetPos - 1) (rank targetPos + pawnPushDir color),
                          Pos (file targetPos + 1) (rank targetPos + pawnPushDir color) 
                        ]
                         
        hasEnemyPawn pos = isValidPos pos && case getPiece (board gs) pos of
            Just (Piece Pawn opponentColor) -> opponentColor == oppositeColor color
            _ -> False

-- Поиск короля заданного цвета на доске. Проходит по всем позициям и возвращает позицию, на которой находится король
findKing :: Board -> Color -> Pos
findKing b color = head (filter isKing [Pos f r | f <- [0..7], r <- [0..7]])
    where
        isKing piecePos = case getPiece b piecePos of
            Just (Piece King pColor) -> pColor == color
            _ -> False

-- Проверка на шах
isCheck :: GameState -> Color -> Bool
isCheck gs color = isSquareAttacked gs (findKing (board gs) color) color

-- Проверка легальности хода. Ход легален, если после его совершения король ходившего игрока не находится под шахом и рокировка проведена по правилам
isMoveLegal :: GameState -> Move -> Bool
isMoveLegal gs move
    | isCastling gs move = not (isCheck gs (activePlayer gs)) && 
                           not (isSquareAttacked gs passedSquare (activePlayer gs)) && 
                           not (isCheck (makeMove gs move) (activePlayer gs))
    | otherwise = not (isCheck (makeMove gs move) (activePlayer gs))
    where
        -- Клетка, которую перепрыгивает король
        passedSquare = Pos ((file (moveFrom move) + file (moveTo move)) `div` 2) (rank (moveFrom move))

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
    [(_, Piece Knight _), (_, Piece Knight _)] -> True -- Король и два коня против Короля
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
