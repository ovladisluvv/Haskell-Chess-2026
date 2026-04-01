module Events 
    ( handleEvent
    ) where

import Graphics.Gloss.Interface.Pure.Game
import Types
import Board (isValidPos, getPiece, initGameState)
import Rules (makeMove, allLegalMoves, isCheckmate, isDraw, unmakeMove)
import Render (windowWidth, windowHeight, squareSize)
import Data.Maybe (listToMaybe)

-- Перевод координат мыши в координаты доски
screenToBoard :: (Float, Float) -> Pos
screenToBoard (x, y) = Pos f r
  where
    f = floor ((x + fromIntegral windowWidth / 2) / squareSize)
    r = floor ((y + fromIntegral windowWidth / 2) / squareSize)

-- Обработка событий экрана
resetGame :: GameState -> GameState
resetGame gs = initGameState { menuState = menuState gs, botColor = botColor gs }

smartUndo :: GameState -> GameState
smartUndo gs = case botColor gs of
    Nothing -> unmakeMove gs
    Just bc -> 
        if activePlayer gs == bc
        then unmakeMove gs 
        else unmakeMove (unmakeMove gs) 

handleEvent :: Event -> GameState -> GameState
-- Перезапуск игры по нажатию клавиши R или К (русская)
handleEvent (EventKey (Char 'r') Down _ _) gs | menuState gs == Hidden = resetGame gs
handleEvent (EventKey (Char 'R') Down _ _) gs | menuState gs == Hidden = resetGame gs
handleEvent (EventKey (Char 'к') Down _ _) gs | menuState gs == Hidden = resetGame gs
handleEvent (EventKey (Char 'К') Down _ _) gs | menuState gs == Hidden = resetGame gs

-- Выход в главное меню по нажатию M или Ь (русская)
handleEvent (EventKey (Char 'm') Down _ _) gs | menuState gs == Hidden = initGameState
handleEvent (EventKey (Char 'M') Down _ _) gs | menuState gs == Hidden = initGameState
handleEvent (EventKey (Char 'ь') Down _ _) gs | menuState gs == Hidden = initGameState
handleEvent (EventKey (Char 'Ь') Down _ _) gs | menuState gs == Hidden = initGameState

-- Кнопка возврата хода по нажатию клавиши Z или Я (русская)
handleEvent (EventKey (Char 'z') Down _ _) gs | menuState gs == Hidden = smartUndo gs
handleEvent (EventKey (Char 'Z') Down _ _) gs | menuState gs == Hidden = smartUndo gs
handleEvent (EventKey (Char 'я') Down _ _) gs | menuState gs == Hidden = smartUndo gs
handleEvent (EventKey (Char 'Я') Down _ _) gs | menuState gs == Hidden = smartUndo gs

-- Выбор режима игры
handleEvent (EventKey (Char '1') Down _ _) gs | menuState gs == MainMenu = gs { menuState = Hidden, botColor = Nothing }
handleEvent (EventKey (Char '2') Down _ _) gs | menuState gs == MainMenu = gs { menuState = ColorMenu }
handleEvent (EventKey (Char '1') Down _ _) gs | menuState gs == ColorMenu = gs { menuState = Hidden, botColor = Just Black }
handleEvent (EventKey (Char '2') Down _ _) gs | menuState gs == ColorMenu = gs { menuState = Hidden, botColor = Just White }
handleEvent (EventKey (Char '3') Down _ _) gs | menuState gs == ColorMenu = gs { menuState = MainMenu }

-- Клик мыши работает только если игра продолжается
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) gs 
    | menuState gs == MainMenu =
        if x >= -100 && x <= 100 && y >= -30 && y <= 30 then gs { menuState = Hidden, botColor = Nothing }
        else if x >= -100 && x <= 100 && y >= -110 && y <= -50 then gs { menuState = ColorMenu }
        else gs
    | menuState gs == ColorMenu =
        if x >= -100 && x <= 100 && y >= -30 && y <= 30 then gs { menuState = Hidden, botColor = Just Black }
        else if x >= -100 && x <= 100 && y >= -110 && y <= -50 then gs { menuState = Hidden, botColor = Just White }
        else if x >= -100 && x <= 100 && y >= -190 && y <= -130 then gs { menuState = MainMenu }
        else gs
    | otherwise = 
        let undoBox = x >= 280 && x <= 400 && y >= 445 && y <= 485
            menuBox = x >= 160 && x <= 280 && y >= 445 && y <= 485
        in if undoBox then smartUndo gs
           else if menuBox then initGameState
           else if not (isCheckmate gs || isDraw gs) then
               let pos = screenToBoard (x, y)
               in case promotionState gs of
                   Just (fromPos, toPos) -> handlePromotionClick pos fromPos toPos gs
                   Nothing -> if isValidPos pos then handleSquareClick pos gs else gs
           else gs
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
                Just move -> (makeMove gs move) { selectedPos = Nothing, promotionState = Nothing }
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
                [move] -> (makeMove gs move) { selectedPos = Nothing } -- Обычный ход (один вариант)
                _ -> gs { promotionState = Just (selPos, clickedPos) } -- Несколько вариантов хода - значит это превращение пешки
