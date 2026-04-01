module Render
    ( windowWidth
    , windowHeight
    , drawGame
    , squareSize
    ) where

import Graphics.Gloss
import Data.Maybe (fromMaybe)

import Types (Piece(..), PieceType(..), GameState(..), MoveRecord(..), Board, Pos(..), Move(..), oppositeColor, MenuState(..))
import qualified Types as T
import Board (getPiece)    
import Rules (allLegalMoves, isCheckmate, isDraw, isCheck, findKing)                    

-- \\-- Константы для рендеринга
-- Размер одной клетки в пикселях
squareSize :: Float
squareSize = 100

-- Размер шахматной доски
boardSize :: Float
boardSize = squareSize * 8

topBarHeight :: Float
topBarHeight = 80

windowWidth :: Int
windowWidth = round boardSize

windowHeight :: Int
windowHeight = round (boardSize + topBarHeight)

-- Цвета клеток доски
lightSquare :: Color
lightSquare = makeColorI 240 217 181 255

darkSquare :: Color
darkSquare = makeColorI 181 136 99 255
-- \\--

-- \\-- Перевод координаты доски (0–7) в экранную координату Gloss
-- Gloss: (0,0) — центр экрана
toScreenX :: Int -> Float
toScreenX f = fromIntegral f * squareSize - boardSize / 2 + squareSize / 2

toScreenY :: Int -> Float
toScreenY r = fromIntegral r * squareSize - boardSize / 2 + squareSize / 2 - topBarHeight / 2
-- \\ --

-- \\-- Главная функция, собирает всю сцену
drawGame :: [(Piece, Picture)] -> GameState -> Picture
drawGame imgs  state
    | menuState state == MainMenu = Pictures [drawBoard, drawPieces imgs state, drawMenu]
    | menuState state == ColorMenu = Pictures [drawBoard, drawPieces imgs state, drawColorMenu]
    | otherwise = Pictures [drawBoard, drawCheckHighlight state, drawHighlights state, drawPieces imgs state, drawLabels, drawTurnIndicator  state, drawUndoMenuButton, drawPromotionMenu imgs state, drawGameOver  state]

-- Главное меню (Выбор режима)
drawMenu :: Picture
drawMenu  = Pictures [
    Color (makeColorI 30 30 30 200) $ Polygon [(-400, -450), (400, -450), (400, 450), (-400, 450)],
    Translate (-180) 100 $ boldText 0.6 "Select Game Mode",
    Translate 0 0 $ Color (makeColorI 70 70 70 255) $ Polygon [(-100, -30), (100, -30), (100, 30), (-100, 30)],
    Translate (-50) (-10) $ boldText 0.4 "1: PvP",
    Translate 0 (-80) $ Color (makeColorI 70 70 70 255) $ Polygon [(-100, -30), (100, -30), (100, 30), (-100, 30)],
    Translate (-50) (-90) $ boldText 0.4 "2: PvE"
  ]

-- Меню выбора цвета (Выбор цвета игрока против бота)
drawColorMenu :: Picture
drawColorMenu  = Pictures [
    Color (makeColorI 30 30 30 200) $ Polygon [(-400, -450), (400, -450), (400, 450), (-400, 450)],
    Translate (-160) 100 $ boldText 0.6 "Select Your Color",
    Translate 0 0 $ Color (makeColorI 70 70 70 255) $ Polygon [(-100, -30), (100, -30), (100, 30), (-100, 30)],
    Translate (-80) (-10) $ boldText 0.4 "1: White",
    Translate 0 (-80) $ Color (makeColorI 70 70 70 255) $ Polygon [(-100, -30), (100, -30), (100, 30), (-100, 30)],
    Translate (-80) (-90) $ boldText 0.4 "2: Black",
    Translate 0 (-160) $ Color (makeColorI 70 70 70 255) $ Polygon [(-100, -30), (100, -30), (100, 30), (-100, 30)],
    Translate (-80) (-170) $ boldText 0.4 "3: Back"
  ]

-- Кнопка отмены хода и выхода
drawUndoMenuButton :: Picture
drawUndoMenuButton  =
    let barY = boardSize / 2 + 10 -- Поднимем повыше панель
        undoX = boardSize / 2 - 60
        menuX = boardSize / 2 - 180
    in Pictures [
        Translate undoX barY $ Color (makeColorI 80 80 80 255) $ Polygon [(-50, -12), (50, -12), (50, 12), (-50, 12)],
        Translate (undoX - 45) (barY - 5) $ boldText 0.3 "Undo (Z)",
        Translate menuX barY $ Color (makeColorI 80 80 80 255) $ Polygon [(-50, -12), (50, -12), (50, 12), (-50, 12)],
        Translate (menuX - 45) (barY - 5) $ boldText 0.3 "Menu (M)"
    ]

formatPos :: Pos -> String
formatPos (Pos f r) = [toEnum (fromEnum 'a' + f), toEnum (fromEnum '1' + r)]

formatMove :: Move -> String
formatMove m = formatPos (moveFrom m) ++ "-" ++ formatPos (moveTo m)

formatHistory :: [MoveRecord] -> String
formatHistory history = 
    let maxDisplay = 5
        recent = drop (max 0 (length history - maxDisplay)) history
    in unwords [formatMove (playedMove m) | m <- recent]

-- Индикатор текущего хода 
drawTurnIndicator :: GameState -> Picture
drawTurnIndicator  state = 
    let turnText = if activePlayer state == T.White then "Turn: White" else "Turn: Black"
        barText = turnText ++ " | Move: " ++ show (moveNumber state)
        histText = "History: " ++ formatHistory (gameStory state)
        barY = boardSize / 2 -- середина высоты для топ бара (относительно 0), верхняя граница окна: boardSize/2 + topBarHeight/2, низ топбара = boardSize/2 - topBarHeight/2
    in Pictures 
       [ Translate 0 barY $ Color (makeColorI 40 40 40 255) $ Polygon [(-boardSize/2, -topBarHeight/2), (boardSize/2, -topBarHeight/2), (boardSize/2, topBarHeight/2), (-boardSize/2, topBarHeight/2)]
       , Translate (-boardSize/2 + 20) (barY + 10) $ Color white $ boldText 0.35 barText
       , Translate (-boardSize/2 + 20) (barY - 15) $ Color (makeColorI 200 200 200 255) $ boldText 0.3 histText
       ]

-- Подсветка короля, если он под шахом
drawCheckHighlight :: GameState -> Picture
drawCheckHighlight state
    | isCheck state (activePlayer state) =
        let Pos f r = findKing (board state) (activePlayer state)
            s = squareSize / 2
        in Translate (toScreenX f) (toScreenY r) $
           Color (makeColorI 255 0 0 180) $ -- красный полупрозрачный фон
           Polygon [(-s, -s), (s, -s), (s, s), (-s, s)]
    | otherwise = Blank

-- Рисует меню превращения пешки
drawPromotionMenu :: [(Piece, Picture)] -> GameState -> Picture
drawPromotionMenu imgs state = case promotionState state of
    Nothing -> Blank
    Just (_, Pos f r) -> 
        let playerColor = activePlayer state
            dir = if playerColor == T.White then -1 else 1
            opts = [ (r, Queen), (r + dir, Rook), (r + 2*dir, Bishop), (r + 3*dir, Knight) ]
            sq = squareSize / 2
            
            drawOpt (rLoc, pt) = 
                let menuBg = Translate (toScreenX f) (toScreenY rLoc) $ 
                             Graphics.Gloss.Color (makeColorI 200 200 200 240) $ 
                             Polygon [(-sq, -sq), (sq, -sq), (sq, sq), (-sq, sq)]
                    border = Translate (toScreenX f) (toScreenY rLoc) $ 
                             Graphics.Gloss.Color (makeColorI 100 100 100 255) $ 
                             Line [(-sq, -sq), (sq, -sq), (sq, sq), (-sq, sq), (-sq, -sq)]
                    pic = Translate (toScreenX f) (toScreenY rLoc) $ renderPiece imgs (Piece pt playerColor)
                in Pictures [menuBg, border, pic]
        in Pictures (map drawOpt opts)

-- Рисует 64 клетки шахматной доски
drawBoard :: Picture
drawBoard = Pictures [drawSquare f r | f <- [0..7], r <- [0..7]]
  where
    drawSquare f r =
        Translate (toScreenX f) (toScreenY r) $
        Color (cellColor f r) $
        Polygon [(-s, -s), (s, -s), (s, s), (-s, s)]
      where
        s = squareSize / 2

    -- a1 (0,0) — тёмная клетка
    cellColor f r
      | even (f + r) = darkSquare
      | otherwise    = lightSquare

-- Рисует все фигуры на доске
drawPieces :: [(Piece, Picture)] -> GameState -> Picture
drawPieces imgs state = Pictures [drawAt f r | f <- [0..7], r <- [0..7]]
  where
    b = board state
    drawAt f r = case getPiece b (Pos f r) of
        Nothing -> Blank
        Just p  -> Translate (toScreenX f) (toScreenY r) (renderPieceRotated p)

    renderPieceRotated p
        | isCheckmate state && p == Piece King (activePlayer state) = Rotate 90 (renderPiece imgs p)
        | otherwise = renderPiece imgs p

-- Рисует одну фигуру
renderPiece :: [(Piece, Picture)] -> Piece -> Picture
renderPiece imgs piece = fromMaybe Blank (lookup piece imgs)

-- Рисует подписи координат (a–h, 1–8)
drawLabels :: Picture
drawLabels  = Pictures (fileLbls ++ rankLbls)
  where
    lblColor = makeColorI 80 80 80 255
    edgeX     = boardSize / 2
    boardBottom = -boardSize / 2 - topBarHeight / 2
    
    offset   = 5

    fileLbls = [Translate (toScreenX f - squareSize/2 + offset) (boardBottom + offset) $
                Color lblColor $ boldText 0.3 [fileChar f]
               | f <- [0..7]]

    rankLbls = [Translate (-edgeX + offset) (toScreenY r + squareSize/2 - 15) $
                Color lblColor $ boldText 0.3 (show (r + 1))
               | r <- [0..7]]

    fileChar f = toEnum (fromEnum 'a' + f)

-- Рисует рамки возможных ходов для выбранной фигуры
drawHighlights :: GameState -> Picture
drawHighlights state = case selectedPos state of
    Nothing -> Blank
    Just selPos -> 
        let legalMoves = [moveTo m | m <- allLegalMoves state, moveFrom m == selPos]
        in Pictures (drawActivePiece selPos : [drawHighlight pos | pos <- legalMoves])
  where
    drawHighlight (Pos f r) = 
        Translate (toScreenX f) (toScreenY r) $
        Color (makeColorI 100 100 100 200) $
        ThickCircle (squareSize / 2 - 5) 5 -- Серая рамка-круг внутри клетки

    drawActivePiece (Pos f r) =
        let s = squareSize / 2
        in Translate (toScreenX f) (toScreenY r) $
           Color (makeColorI 150 200 150 180) $ -- зеленый полупрозрачный фон
           Polygon [(-s, -s), (s, -s), (s, s), (-s, s)]

-- Рисует окно окончания игры
drawGameOver :: GameState -> Picture
drawGameOver  state
    | isCheckmate state = drawMessage (winnerMessage ++ " Wins! (Mate)")
    | isDraw state = drawMessage "Draw!"
    | otherwise = Blank
  where
    -- Если мат, то проиграл активный игрок. Выигрывает противоположный.
    winnerMessage = case oppositeColor (activePlayer state) of
        T.White -> "White"
        T.Black -> "Black"

    drawMessage msg = Pictures 
        [ -- Полупрозрачный фон поверх всей доски
          Graphics.Gloss.Color (makeColorI 0 0 0 150) $ Polygon [(-w, -h), (w, -h), (w, h), (-w, h)]
          -- Основной текст результата
        , Translate (-150) 40 $ Graphics.Gloss.Color white $ boldText 0.8 msg
        -- Подсказка для рестарта
        , Translate (-120) (-40) $ Graphics.Gloss.Color (makeColorI 200 200 200 255) $ boldText 0.4 "Press R to Restart"
        ]
      where
        w = fromIntegral windowWidth / 2
        h = fromIntegral windowHeight / 2
-- \--


-- Эффект жирного шрифта
boldText :: Float -> String -> Picture
boldText s str = Scale (s * 0.45) (s * 0.45) $ Pictures 
    [ Translate dx dy (Text str) | dx <- [-0.5, 0, 0.5], dy <- [-0.5, 0, 0.5] ]
