module Board where

import Types
import qualified Data.Vector as V

-- /--- Библиотека для работы с позицией на шахматной доске
-- Проверка, находится ли позиция в пределах доски (0-7 для столбца и 0-7 для ряда)
isValidPos :: Pos -> Bool
isValidPos (Pos f r) = f >= 0 && f <= 7 && r >= 0 && r <= 7

-- Перевод 2D-координату в 1D-индекс вектора (от 0 до 63)
posToInd :: Pos -> Int
posToInd (Pos f r) = r * 8 + f

-- Обратная конвертация из 1D-индекса в 2D-координату
indToPos :: Int -> Pos
indToPos ind = Pos (mod ind 8) (div ind 8)
-- \---

-- /--- Библиотека для работы с фигурами на доске
-- Получение фигуры на доске по позиции. Возвращает Nothing, если позиция недействительна или клетка пуста
getPiece :: Board -> Pos -> Maybe Piece
getPiece (Board b) pos
    | isValidPos pos = b V.! ind
    | otherwise = Nothing
    where ind = posToInd pos

-- Установка содержимого клетки (фигуры или пустоты). Если позиция недействительна, возвращает неизмененную доску
setPiece :: Board -> Pos -> Maybe Piece -> Board
setPiece (Board b) pos piece
    | isValidPos pos = Board (b V.// [(ind, piece)])
    | otherwise = Board b
    where ind = posToInd pos

-- Вспомогательная функция: поставить фигуру
placePiece :: Board -> Pos -> Piece -> Board
placePiece (Board b) pos piece = setPiece (Board b) pos (Just piece)

-- Вспомогательная функция: убрать фигуру
removePiece :: Board -> Pos -> Board
removePiece (Board b) pos = setPiece (Board b) pos Nothing

-- Перемещение фигуры на доске с posFrom на posTo. Клетка posFrom становится пустой
movePiece :: Board -> Pos -> Pos -> Board
movePiece (Board b) posFrom posTo = setPiece boardWithoutPiece posTo (getPiece (Board b) posFrom)
    where boardWithoutPiece = setPiece (Board b) posFrom Nothing        
-- \---

-- /--- Библиотека для инициализации игры
-- Пустая доска (64 значения Nothing)
emptyBoard :: Board
emptyBoard = Board (V.replicate 64 Nothing)

-- Инициализация доски с начальной расстановкой фигур
initBoard :: Board
initBoard = Board (V.generate 64 initSquare)
    where
        initSquare ind = case r of
            1 -> Just (Piece Pawn White) -- 2-я горизонталь (белые пешки)
            6 -> Just (Piece Pawn Black) -- 7-я горизонталь (черные пешки)
            0 -> Just (Piece (backRank f) White) -- 1-я горизонталь (белые фигуры)
            7 -> Just (Piece (backRank f) Black) -- 8-я горизонталь (черные фигуры)
            _ -> Nothing
            where Pos f r = indToPos ind

        backRank 0 = Rook
        backRank 1 = Knight
        backRank 2 = Bishop
        backRank 3 = Queen
        backRank 4 = King
        backRank 5 = Bishop
        backRank 6 = Knight
        backRank 7 = Rook

-- Инициализация состояния игры
initGameState :: GameState
initGameState = GameState { board = initBoard,
                            activePlayer = White,
                            moveNumber = 1,
                            halfMoveCount = 0,
                            enPassantTarget = Nothing,
                            castlingRights = CastlingRights True True True True,
                            selectedPos = Nothing,
                            promotionState = Nothing }
-- \---
