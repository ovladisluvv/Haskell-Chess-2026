module Lib
    ( run
    ) where

import Graphics.Gloss 
import Board (initGameState)
import Types (GameState, Piece(..), PieceType(..))
import qualified Types as T
import Render (windowSize, drawGame)
import Events (handleEvent)

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

-- Шаг по времени (не нужен без таймера)
update :: Float -> GameState -> GameState
update _ state = state

-- Загружаем спрайты
-- Загружаем спрайты
loadImages :: IO [(Piece, Picture)]
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

    return [ 
        (Piece Pawn T.White, wpPic),   (Piece Pawn T.Black, bpPic),
        (Piece Knight T.White, wnPic), (Piece Knight T.Black, bnPic),
        (Piece King T.White, wkPic),   (Piece King T.Black, bkPic),
        (Piece Bishop T.White, wbPic), (Piece Bishop T.Black, bbPic),
        (Piece Rook T.White, wrPic),   (Piece Rook T.Black, brPic),
        (Piece Queen T.White, wqPic),  (Piece Queen T.Black, bqPic) 
      ]
-- \\-- Запуск

run :: IO ()
run = do
    images <- loadImages
    let draw state = drawGame images state
    play window bgColor fps initGameState draw handleEvent update
