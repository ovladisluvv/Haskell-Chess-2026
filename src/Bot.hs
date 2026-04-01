module Bot (isBotTurn, makeBotMove) where

import Types
import Rules (allLegalMoves, makeMove, isCheckmate, isDraw)
import Board (getPiece)
import qualified Data.Vector as V

-- /--- Библиотека для логики бота (Minimax)
-- Функция для получения материальной ценности фигуры
getMaterialValue :: PieceType -> Int
getMaterialValue Pawn = 10
getMaterialValue Knight = 30
getMaterialValue Bishop = 30
getMaterialValue Rook = 50
getMaterialValue Queen = 90
getMaterialValue King = 9000

getPosValue :: PieceType -> Color -> Pos -> Int
getPosValue pieceType White (Pos f r) = getTable pieceType V.! ((7 - r) * 8 + f)
getPosValue pieceType Black (Pos f r) = getTable pieceType V.! (r * 8 + f)

getTable :: PieceType -> V.Vector Int
getTable Pawn = pawnTable
getTable Knight = knightTable
getTable Bishop = bishopTable
getTable Rook = rookTable
getTable Queen = queenTable
getTable King = kingTable

pawnTable :: V.Vector Int
pawnTable = V.fromList 
    [ 0,  0,  0,  0,  0,  0,  0,  0,
     50, 50, 50, 50, 50, 50, 50, 50,
     10, 10, 20, 30, 30, 20, 10, 10,
      5,  5, 10, 25, 25, 10,  5,  5,
      0,  0,  0, 20, 20,  0,  0,  0,
      5, -5,-10,  0,  0,-10, -5,  5,
      5, 10, 10,-20,-20, 10, 10,  5,
      0,  0,  0,  0,  0,  0,  0,  0
    ]

knightTable :: V.Vector Int
knightTable = V.fromList 
    [-50,-40,-30,-30,-30,-30,-40,-50,
     -40,-20,  0,  0,  0,  0,-20,-40,
     -30,  0, 10, 15, 15, 10,  0,-30,
     -30,  5, 15, 20, 20, 15,  5,-30,
     -30,  0, 15, 20, 20, 15,  0,-30,
     -30,  5, 10, 15, 15, 10,  5,-30,
     -40,-20,  0,  5,  5,  0,-20,-40,
     -50,-40,-30,-30,-30,-30,-40,-50
    ]

bishopTable :: V.Vector Int
bishopTable = V.fromList 
    [-20,-10,-10,-10,-10,-10,-10,-20,
     -10,  0,  0,  0,  0,  0,  0,-10,
     -10,  0,  5, 10, 10,  5,  0,-10,
     -10,  5,  5, 10, 10,  5,  5,-10,
     -10,  0, 10, 10, 10, 10,  0,-10,
     -10, 10, 10, 10, 10, 10, 10,-10,
     -10,  5,  0,  0,  0,  0,  5,-10,
     -20,-10,-10,-10,-10,-10,-10,-20
    ]

rookTable :: V.Vector Int
rookTable = V.fromList 
    [ 0,  0,  0,  0,  0,  0,  0,  0,
      5, 10, 10, 10, 10, 10, 10,  5,
     -5,  0,  0,  0,  0,  0,  0, -5,
     -5,  0,  0,  0,  0,  0,  0, -5,
     -5,  0,  0,  0,  0,  0,  0, -5,
     -5,  0,  0,  0,  0,  0,  0, -5,
     -5,  0,  0,  0,  0,  0,  0, -5,
      0,  0,  0,  5,  5,  0,  0,  0
    ]

queenTable :: V.Vector Int
queenTable = V.fromList
    [-20,-10,-10, -5, -5,-10,-10,-20,
     -10,  0,  0,  0,  0,  0,  0,-10,
     -10,  0,  5,  5,  5,  5,  0,-10,
      -5,  0,  5,  5,  5,  5,  0, -5,
       0,  0,  5,  5,  5,  5,  0, -5,
     -10,  5,  5,  5,  5,  5,  0,-10,
     -10,  0,  5,  0,  0,  0,  0,-10,
     -20,-10,-10, -5, -5,-10,-10,-20
    ]

kingTable :: V.Vector Int
kingTable = V.fromList
    [-30,-40,-40,-50,-50,-40,-40,-30,
     -30,-40,-40,-50,-50,-40,-40,-30,
     -30,-40,-40,-50,-50,-40,-40,-30,
     -30,-40,-40,-50,-50,-40,-40,-30,
     -20,-30,-30,-40,-40,-30,-30,-20,
     -10,-20,-20,-20,-20,-20,-20,-10,
      20, 20,  0,  0,  0,  0, 20, 20,
      20, 30, 10,  0,  0, 10, 30, 20
    ]

-- Вспомогательная функция для расчета оценки позиции. Возвращает число > 0 в пользу белых, < 0 в пользу черных
evaluatePosition :: GameState -> Int
evaluatePosition gs = sum (map pieceValue allPieces)
    where
        allPieces = concatMap getPosAndPieces [Pos f r | f <- [0..7], r <- [0..7]]

        getPosAndPieces pos = case getPiece (board gs) pos of
            Just piece -> [(pos, piece)]
            Nothing -> []

        pieceValue (pos, Piece pieceType White) = getMaterialValue pieceType + getPosValue pieceType White pos
        pieceValue (pos, Piece pieceType Black) = -(getMaterialValue pieceType + getPosValue pieceType Black pos)

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
