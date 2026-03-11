module Events 
    ( handleEvent
    ) where

import Graphics.Gloss.Interface.Pure.Game
import Types
import Board (isValidPos, getPiece)
import Rules (applyMove, allLegalMoves)
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
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) gs = 
    let pos = screenToBoard mousePos
    in if isValidPos pos then handleSquareClick pos gs else gs
handleEvent _ gs = gs

-- Логика выбора фигура и совершения хода
handleSquareClick :: Pos -> GameState -> GameState
handleSquareClick clickedPos gs = case selectedPos gs of
    Nothing -> 
        -- Пытаемся выбрать фигуру
        case getPiece (board gs) clickedPos of
            Just (Piece _ color) | color == activePlayer gs -> gs { selectedPos = Just clickedPos }
            _ -> gs
    Just selPos ->
        -- Фигура уже выбрана
        if clickedPos == selPos then
            gs { selectedPos = Nothing } -- Снятие выделения, если кликнули по той же фигуре
        else
            let legalMoves = allLegalMoves gs
                -- Ищем ход, который соответствует выбору пользователя
                matchedMove = listToMaybe [m | m <- legalMoves, moveFrom m == selPos, moveTo m == clickedPos]
            in case matchedMove of
                Just move -> (applyMove gs move) { selectedPos = Nothing } -- Делаем ход
                Nothing -> 
                    -- Если кликнули на другую свою фигуру, перевыбираем её
                    case getPiece (board gs) clickedPos of
                        Just (Piece _ color) | color == activePlayer gs -> gs { selectedPos = Just clickedPos }
                        _ -> gs { selectedPos = Nothing }
