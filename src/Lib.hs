module Lib
    ( run
    ) where

import Graphics.Gloss 
import Graphics.Gloss.Interface.Pure.Game

import Board (initGameState)
import Types (GameState)
import Render (windowSize, drawGame)

-- \\-- Главная функция запуска приложения
-- Окно приложения
window :: Display
window = InWindow "Haskell Chess 2026" (windowSize, windowSize) (50, 50)

-- Фон вокруг доски
bgColor :: Color
bgColor = makeColorI 50 50 50 255

-- Количество обновлений экрана в секунду
fps :: Int
fps = 30

-- Обработка событий (пока заглушка)
event :: Event -> GameState -> GameState
event _ state = state

-- Шаг по времени (не нужен без таймера)
update :: Float -> GameState -> GameState
update _ state = state

-- Загружаем спрайты
loadImages :: IO [(String, Picture)]
loadImages = do
    wpPic <- loadBMP "assets/pieces/wp.bmp"
    bpPic <- loadBMP "assets/pieces/bp.bmp"
    wnPic <- loadBMP "assets/pieces/wn.bmp"
    bnPic <- loadBMP "assets/pieces/bn.bmp"
    wkPic <- loadBMP "assets/pieces/wk.bmp"
    bkPic <- loadBMP "assets/pieces/bk.bmp"
    wbPic <- loadBMP "assets/pieces/wb.bmp"
    bbPic <- loadBMP "assets/pieces/bb.bmp"
    wrPic <- loadBMP "assets/pieces/wr.bmp"
    brPic <- loadBMP "assets/pieces/br.bmp"
    wqPic <- loadBMP "assets/pieces/wq.bmp"
    bqPic <- loadBMP "assets/pieces/bq.bmp"

    return [ ("wp", wpPic), ("bp", bpPic), ("wn", wnPic), ("bn", bnPic), ("wk", wkPic), ("bk", bkPic), ("wb", wbPic), ("bb", bbPic), ("wr", wrPic), ("br", brPic), ("wq", wqPic), ("bq", bqPic) ]

-- \\-- Запуск

run :: IO ()
run = do
    images <- loadImages
    let draw state = drawGame images state
    play window bgColor fps initGameState draw event update
