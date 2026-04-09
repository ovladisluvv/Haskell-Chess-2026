module Bot (isBotTurn, makeBotMove) where

import Types
import Rules
import Board
import Moves
import qualified Data.Vector as V

-- /--- Библиотека для логики бота (Minimax)
-- Функция для получения материальной ценности фигуры
getMaterialValue :: PieceType -> Int
getMaterialValue Pawn = 100
getMaterialValue Knight = 310
getMaterialValue Bishop = 340
getMaterialValue Rook = 500
getMaterialValue Queen = 900
getMaterialValue King = 20000

-- Функция для получения позиционной ценности фигуры на доске
getPosValue :: GameState -> PieceType -> Color -> Pos -> Int
getPosValue gs pieceType White (Pos f r) = getTable gs pieceType V.! ((7 - r) * 8 + f)
getPosValue gs pieceType Black (Pos f r) = getTable gs pieceType V.! (r * 8 + f)

-- Генерация таблицы с позиционной ценностью для каждой фигуры
getTable :: GameState -> PieceType -> V.Vector Int
getTable _ Pawn = pawnTable
getTable _ Knight = knightTable
getTable _ Bishop = bishopTable
getTable _ Rook = rookTable
getTable _ Queen = queenTable
getTable gs King 
    | isEndgame gs = kingEndgameTable
    | otherwise = kingMiddlegameTable

-- Вспомогательная функция для определения, находится ли позиция в эндшпиле. Эндшпиль наступает, когда суммарная материальная ценность всех фигур, кроме пешек и королей, на доске меньше или равна 2600 (примерно 2 ладьи и 1 слон/конь на игрока)
isEndgame :: GameState -> Bool
isEndgame gs = totalNonPawnMaterial <= 2600
    where
        totalNonPawnMaterial = sum (map nonPawnValue allPieces)

        allPieces = concatMap getPosAndPieces [Pos f r | f <- [0..7], r <- [0..7]]

        getPosAndPieces pos = case getPiece (board gs) pos of
            Just piece -> [(pos, piece)]
            Nothing -> []

        nonPawnValue (pos, Piece pieceType _)
            | pieceType == Pawn = 0
            | pieceType == King = 0
            | otherwise = getMaterialValue pieceType

-- Таблица позиционной ценности для пешки
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

-- Таблица позиционной ценности для коня
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

-- Таблица позиционной ценности для слона
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

-- Таблица позиционной ценности для ладьи
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

-- Таблица позиционной ценности для ферзя
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

-- Таблица позиционной ценности для короля в дебюте и миттельшпиле (стремится оставаться у начальной позиции)
kingMiddlegameTable :: V.Vector Int
kingMiddlegameTable = V.fromList
    [-30,-40,-40,-50,-50,-40,-40,-30,
     -30,-40,-40,-50,-50,-40,-40,-30,
     -30,-40,-40,-50,-50,-40,-40,-30,
     -30,-40,-40,-50,-50,-40,-40,-30,
     -20,-30,-30,-40,-40,-30,-30,-20,
     -10,-20,-20,-20,-20,-20,-20,-10,
      20, 20,  0,  0,  0,  0, 20, 20,
      20, 30, 10,  0,  0, 10, 30, 20
    ]

-- Таблица позиционной ценности для короля в эндшпиле (стремится к центру)
kingEndgameTable :: V.Vector Int
kingEndgameTable = V.fromList
    [-50,-40,-30,-20,-20,-30,-40,-50,
     -30,-20,-10,  0,  0,-10,-20,-30,
     -30,-10, 20, 30, 30, 20,-10,-30,
     -30,-10, 30, 40, 40, 30,-10,-30,
     -30,-10, 30, 40, 40, 30,-10,-30,
     -30,-10, 20, 30, 30, 20,-10,-30,
     -30,-30,  0,  0,  0,  0,-30,-30,
     -50,-30,-30,-30,-30,-30,-30,-50
    ]

-- Вспомогательная функция для расчета оценки позиции. Возвращает число > 0 в пользу белых, < 0 в пользу черных
evaluatePosition :: GameState -> Int
evaluatePosition gs = baseScore + mobilityScore
    where
        baseScore = sum (map pieceValue allPieces)

        allPieces = concatMap getPosAndPieces [Pos f r | f <- [0..7], r <- [0..7]]

        getPosAndPieces pos = case getPiece (board gs) pos of
            Just piece -> [(pos, piece)]
            Nothing -> []

        pieceValue (pos, Piece pieceType White) = getMaterialValue pieceType + getPosValue gs pieceType White pos
        pieceValue (pos, Piece pieceType Black) = -(getMaterialValue pieceType + getPosValue gs pieceType Black pos)

        mobilityScore = (length allWhiteMoves - length allBlackMoves) * 4

        allWhiteMoves = allPossibleMoves (gs {activePlayer = White})
        allBlackMoves = allPossibleMoves (gs {activePlayer = Black})

-- Вспомогательная функция для просчета Альфа-Бета отсечений функции-максимизатора
maxiLoopAB :: GameState -> [Move] -> Int -> Int -> Int -> Int
maxiLoopAB _ [] _ alpha _ = alpha
maxiLoopAB gs (m:ms) depth alpha beta
    | evalNext >= beta = beta
    | otherwise = maxiLoopAB gs ms depth (max alpha evalNext) beta
    where
        evalNext = mini (makeMove gs m) (depth - 1) alpha beta

-- Функция-максимизатор для алгоритма Minimax. Возвращает лучшую оценку для белых
maxi :: GameState -> Int -> Int -> Int -> Int
maxi gs 0 _ _ = evaluatePosition gs
maxi gs depth alpha beta
    | isCheckmate gs = -(1000000 + depth)
    | isDraw gs = 0
    | null (allLegalMoves gs) = 0
    | otherwise = maxiLoopAB gs (allLegalMoves gs) depth alpha beta

-- Вспомогательная функция для просчета Alpha-Beta отсечений функции-минимизатора
miniLoopAB :: GameState -> [Move] -> Int -> Int -> Int -> Int
miniLoopAB _ [] _ _ beta = beta
miniLoopAB gs (m:ms) depth alpha beta
    | evalNext <= alpha = alpha
    | otherwise = miniLoopAB gs ms depth alpha (min beta evalNext)
    where
        evalNext = maxi (makeMove gs m) (depth - 1) alpha beta

-- Функция-минимизатор для алгоритма Minimax. Возвращает лучшую оценку для черных
mini :: GameState -> Int -> Int -> Int -> Int
mini gs 0 _ _ = evaluatePosition gs
mini gs depth alpha beta
    | isCheckmate gs = 1000000 + depth
    | isDraw gs = 0
    | null (allLegalMoves gs) = 0
    | otherwise = miniLoopAB gs (allLegalMoves gs) depth alpha beta

-- Вспомогательная функция для поиска лучшего хода для белых
findBestMax :: GameState -> [Move] -> Int -> Int -> Int -> Move -> Move
findBestMax _ [] _ _ _ bestMove = bestMove
findBestMax gs (m:ms) depth alpha beta bestMove
    | evalNext > alpha = findBestMax gs ms depth evalNext beta m
    | otherwise = findBestMax gs ms depth alpha beta bestMove
    where
        evalNext = mini (makeMove gs m) (depth - 1) alpha beta

-- Вспомогательная функция для поиска лучшего хода для черных
findBestMin :: GameState -> [Move] -> Int -> Int -> Int -> Move -> Move
findBestMin _ [] _ _ _ bestMove = bestMove
findBestMin gs (m:ms) depth alpha beta bestMove
    | evalNext < beta = findBestMin gs ms depth alpha evalNext m
    | otherwise = findBestMin gs ms depth alpha beta bestMove
    where
        evalNext = maxi (makeMove gs m) (depth - 1) alpha beta

-- Главная функция для получения лучшего хода бота. Выбирает ход с лучшей оценкой после применения Minimax Alpha-Beta с заданной глубиной
getBestMove :: GameState -> Int -> Move
getBestMove gs depth
    | activePlayer gs == White = findBestMax gs (allLegalMoves gs) depth (-1000000) 1000000 (head (allLegalMoves gs))
    | otherwise = findBestMin gs (allLegalMoves gs) depth (-1000000) 1000000 (head (allLegalMoves gs))
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
        bestMove = getBestMove gs 4 -- 4 - глубина поиска
-- \---
