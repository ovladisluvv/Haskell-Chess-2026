module Lib
    ( run
    ) where

import Bot (isBotTurn, makeBotMove)
import Board (initGameState)
import Events (handleEvent)
import Graphics.Gloss
import Render (drawGame, windowHeight, windowWidth)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import Types (GameState(..), Piece(..), PieceType(..))
import qualified Types as T

-- Главная функция запуска приложения
-- Окно приложения
window :: Display
window = InWindow "Haskell Chess 2026" (windowWidth, windowHeight) (50, 50)

-- Фон вокруг доски
bgColor :: Color
bgColor = makeColorI 50 50 50 255

-- Количество обновлений экрана в секунду
fps :: Int
fps = 30

-- Шаг по времени
update :: Float -> GameState -> GameState
update dt state
    -- Если сейчас ход бота, делаем ход после задержки
    | isBotTurn state =
        if botTimer state <= 0
        then makeBotMove state
        else state { botTimer = botTimer state - dt }
    | otherwise = state

resolveAssetPath :: FilePath -> IO FilePath
resolveAssetPath fileName = do
    exePath <- getExecutablePath
    let packagedPath = takeDirectory exePath </> "assets" </> "pieces" </> fileName
        devPath = "assets" </> "pieces" </> fileName
    packagedExists <- doesFileExist packagedPath
    pure $ if packagedExists then packagedPath else devPath

loadPieceImage :: FilePath -> IO Picture
loadPieceImage fileName = do
    path <- resolveAssetPath fileName
    loadBMP path

-- Загружаем спрайты
loadImages :: IO [(Piece, Picture)]
loadImages = do
    wpPic <- loadPieceImage "wp.bmp"
    bpPic <- loadPieceImage "bp.bmp"
    wnPic <- loadPieceImage "wn.bmp"
    bnPic <- loadPieceImage "bn.bmp"
    wkPic <- loadPieceImage "wk.bmp"
    bkPic <- loadPieceImage "bk.bmp"
    wbPic <- loadPieceImage "wb.bmp"
    bbPic <- loadPieceImage "bb.bmp"
    wrPic <- loadPieceImage "wr.bmp"
    brPic <- loadPieceImage "br.bmp"
    wqPic <- loadPieceImage "wq.bmp"
    bqPic <- loadPieceImage "bq.bmp"

    return
        [ (Piece Pawn T.White, wpPic),   (Piece Pawn T.Black, bpPic)
        , (Piece Knight T.White, wnPic), (Piece Knight T.Black, bnPic)
        , (Piece King T.White, wkPic),   (Piece King T.Black, bkPic)
        , (Piece Bishop T.White, wbPic), (Piece Bishop T.Black, bbPic)
        , (Piece Rook T.White, wrPic),   (Piece Rook T.Black, brPic)
        , (Piece Queen T.White, wqPic),  (Piece Queen T.Black, bqPic)
        ]

-- Запуск
run :: IO ()
run = do
    images <- loadImages
    let draw = drawGame images
    play window bgColor fps initGameState draw handleEvent update
