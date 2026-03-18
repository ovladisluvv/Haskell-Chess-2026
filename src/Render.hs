module Render
    ( windowWidth
    , windowHeight
    , drawGame
    , squareSize
    ) where

import Graphics.Gloss
import Data.Maybe (fromMaybe)

import Types (Piece(..), PieceType(..), GameState(..), Board, Pos(..), Move(..), oppositeColor)
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
topBarHeight = 35

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
drawGame imgs state = Pictures [drawBoard, drawCheckHighlight state, drawHighlights state, drawPieces imgs (board state), drawLabels, drawTurnIndicator state, drawPromotionMenu imgs state, drawGameOver state]

-- Индикатор текущего хода 
drawTurnIndicator :: GameState -> Picture
drawTurnIndicator state = 
    let turnText = if activePlayer state == T.White then "Turn: White" else "Turn: Black"
        barY = boardSize / 2 -- середина высоты для топ бара (относительно 0), верхняя граница окна: boardSize/2 + topBarHeight/2, низ топбара = boardSize/2 - topBarHeight/2
    in Pictures 
       [ Translate 0 barY $ Color (makeColorI 40 40 40 255) $ Polygon [(-boardSize/2, -topBarHeight/2), (boardSize/2, -topBarHeight/2), (boardSize/2, topBarHeight/2), (-boardSize/2, topBarHeight/2)]
       , Translate (-boardSize/2 + 20) (barY - 5) $ Scale 0.15 0.15 $ Color white $ Text turnText
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
drawPieces :: [(Piece, Picture)] -> Board -> Picture
drawPieces imgs b = Pictures [drawAt f r | f <- [0..7], r <- [0..7]]
  where
    drawAt f r = case getPiece b (Pos f r) of
        Nothing -> Blank
        Just p  -> Translate (toScreenX f) (toScreenY r) (renderPiece imgs p)

-- Рисует одну фигуру
renderPiece :: [(Piece, Picture)] -> Piece -> Picture
renderPiece imgs piece = fromMaybe Blank (lookup piece imgs)

-- Рисует подписи координат (a–h, 1–8)
drawLabels :: Picture
drawLabels = Pictures (fileLbls ++ rankLbls)
  where
    lblColor = makeColorI 80 80 80 255
    edgeX     = boardSize / 2
    boardBottom = -boardSize / 2 - topBarHeight / 2
    
    offset   = 5

    fileLbls = [Translate (toScreenX f - squareSize/2 + offset) (boardBottom + offset) $
                Scale 0.12 0.12 $ Color lblColor $ Text [fileChar f]
               | f <- [0..7]]

    rankLbls = [Translate (-edgeX + offset) (toScreenY r + squareSize/2 - 15) $
                Scale 0.12 0.12 $ Color lblColor $ Text (show (r + 1))
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
drawGameOver state
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
        , Translate (-150) 40 $ Scale 0.3 0.3 $ Graphics.Gloss.Color white $ Text msg
          -- Подсказка для рестарта
        , Translate (-120) (-40) $ Scale 0.15 0.15 $ Graphics.Gloss.Color (makeColorI 200 200 200 255) $ Text "Press R to Restart"
        ]
      where
        w = fromIntegral windowWidth / 2
        h = fromIntegral windowHeight / 2
-- \--
