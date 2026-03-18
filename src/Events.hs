module Events 
    ( handleEvent
    ) where

import Graphics.Gloss.Interface.Pure.Game
import Types
import Board (isValidPos, getPiece, initGameState)
import Rules (applyMove, allLegalMoves, isCheckmate, isDraw)
import Render (windowSize, squareSize)
import Data.Maybe (listToMaybe)

-- Перевод координат мыши в координаты доски
screenToBoard :: (Float, Float) -> Pos
screenToBoard (x, y) = Pos f r
  where
    f = floor ((x + fromIntegral windowSize / 2) / squareSize)
    r = floor ((y + fromIntegral windowSize / 2) / squareSize)

-- Обработка событий экрана
handleEvent :: Event -> GameState -> GameState
-- Перезапуск игры по нажатию клавиши R
handleEvent (EventKey (Char 'r') Down _ _) _ = initGameState
handleEvent (EventKey (Char 'R') Down _ _) _ = initGameState
-- Клик мыши работает только если игра продолжается
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) gs 
    | not (isCheckmate gs || isDraw gs) = 
        let pos = screenToBoard mousePos
        in case promotionState gs of
            Just (fromPos, toPos) -> handlePromotionClick pos fromPos toPos gs
            Nothing -> if isValidPos pos then handleSquareClick pos gs else gs
handleEvent _ gs = gs

-- Логика выбора из меню превращения
handlePromotionClick :: Pos -> Pos -> Pos -> GameState -> GameState
handlePromotionClick (Pos f_click r_click) fromPos toPos@(Pos f_to r_to) gs =
    let playerColor = activePlayer gs
        -- Для белых меню идет вниз (7, 6, 5, 4), для черных вверх (0, 1, 2, 3)
        dir = if playerColor == White then -1 else 1
        choices = [ (r_to, Queen), (r_to + dir, Rook), (r_to + 2*dir, Bishop), (r_to + 3*dir, Knight) ]
        -- Проверяем, попал ли клик в одну из кнопок меню
        match = lookup r_click [ (r, pt) | (r, pt) <- choices, f_click == f_to ]
    in case match of
        Just pt -> 
            let legalMoves = allLegalMoves gs
                matchedMove = listToMaybe [m | m <- legalMoves, moveFrom m == fromPos, moveTo m == toPos, movePromote m == Just pt]
            in case matchedMove of
                Just move -> (applyMove gs move) { selectedPos = Nothing, promotionState = Nothing }
                Nothing -> gs { promotionState = Nothing, selectedPos = Nothing }
        Nothing -> gs { promotionState = Nothing, selectedPos = Nothing } -- Кликнули мимо меню - отменяем

-- Логика выбора фигура и совершения хода
handleSquareClick :: Pos -> GameState -> GameState
handleSquareClick clickedPos gs = case selectedPos gs of
    Nothing -> 
        -- Пытаемся выбрать фигуру
        case getPiece (board gs) clickedPos of
            Just (Piece _ playerColor) | playerColor == activePlayer gs -> gs { selectedPos = Just clickedPos }
            _ -> gs
    Just selPos ->
        -- Фигура уже выбрана
        if clickedPos == selPos then
            gs { selectedPos = Nothing } -- Снятие выделения, если кликнули по той же фигуре
        else
            let legalMoves = allLegalMoves gs
                -- Ищем все ходы с одной клетки на другую
                matchedMoves = [m | m <- legalMoves, moveFrom m == selPos, moveTo m == clickedPos]
            in case matchedMoves of
                [] -> 
                    -- Если кликнули на другую свою фигуру, перевыбираем её
                    case getPiece (board gs) clickedPos of
                        Just (Piece _ playerColor) | playerColor == activePlayer gs -> gs { selectedPos = Just clickedPos }
                        _ -> gs { selectedPos = Nothing }
                [move] -> (applyMove gs move) { selectedPos = Nothing } -- Обычный ход (один вариант)
                _ -> gs { promotionState = Just (selPos, clickedPos) } -- Несколько вариантов хода - значит это превращение пешки
