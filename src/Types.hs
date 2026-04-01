module Types where

import Data.Vector (Vector)

-- Цвет шахматной фигуры: белый или черный
data Color = White | Black deriving (Show, Eq)

-- Меняет цвет на противоположный
oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

-- Тип шахматной фигуры: пешка, конь, слон, ладья, ферзь или король
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq, Ord)

-- Шахматная фигура. Включает тип и цвет
data Piece = Piece { pieceType :: PieceType, 
                     pieceColor :: Color 
                   } deriving (Show, Eq)

-- Позиция на шахматной доске, представленная в виде структуры с полями для столбца и ряда
data Pos = Pos { file :: Int, -- Столбец на шахматной доске, от 0 до 7 [0 = 'a', 1 = 'b', ..., 7 = 'h']
                 rank :: Int -- Ряд на шахматной доске, от 0 до 7 [0 = '1', 1 = '2', ..., 7 = '8']
               } deriving (Show, Eq, Ord)

-- Шахматная доска, представленная в виде отображения позиций в фигуры
newtype Board = Board (Vector (Maybe Piece)) deriving (Show, Eq)

-- Ход в фигуры. Включает начальную и конечную позицию, информацию о том, в какую фигуру превратилась пешка, дошедшая до края доски (если это был ход пешкой)
data Move = Move { moveFrom :: Pos,
                   moveTo :: Pos,
                   movePromote :: Maybe PieceType -- Nothing, если ход не связан с превращением пешки, Just PieceType - тип фигуры, в которую превращается пешка
                 } deriving (Show, Eq, Ord)

-- Права на рокировку для обеих сторон. Каждое поле указывает, разрешена ли соответствующая рокировка
data CastlingRights = CastlingRights { whiteKingSide :: Bool,
                                       whiteQueenSide :: Bool,
                                       blackKingSide :: Bool,
                                       blackQueenSide :: Bool
                                     } deriving (Show, Eq)

-- Состояние игры. Включает текущую позицию на доске, активного игрока и номер хода
data GameState = GameState { board :: Board,
                             activePlayer :: Color,
                             moveNumber :: Int, -- Номер хода увеличивается после хода черных
                             halfMoveCount :: Int, -- Счетчик полуходов для правила 50 ходов
                             enPassantTarget :: Maybe Pos, -- Позиция, доступная для взятия на проходе, если таковая имеется
                             castlingRights :: CastlingRights, -- Права на рокировку для обеих сторон
                             gameStory :: [MoveRecord],
                             selectedPos :: Maybe Pos, -- Позиция выбранной фигуры для хода, если есть
                             promotionState :: Maybe (Pos, Pos), -- Координаты (откуда, куда) для отрисовки меню превращения
                             botColor :: Maybe Color, -- Цвет фигуры бота
                             menuState :: MenuState, -- Состояние меню выбора режима
                             botTimer :: Float, -- Задержка бота в секундах
                             deadWhite :: [Piece], -- Взятые белые фигуры
                             deadBlack :: [Piece] -- Взятые черные фигуры
                           } deriving (Show)

-- Состояние меню выбора режима: главное меню, меню выбора цвета, меню скрыто
data MenuState = MainMenu | ColorMenu | Hidden deriving (Show, Eq)

-- Переопределение функции сравнения для GameState, поскольку генератор случайных чисел не поддерживает сравнение на равенство
instance Eq GameState where
  gs1 == gs2 = board gs1 == board gs2 &&
               activePlayer gs1 == activePlayer gs2 &&
               enPassantTarget gs1 == enPassantTarget gs2 &&
               castlingRights gs1 == castlingRights gs2

-- Смещения. Необходимы для ходов Коня и Короля
data Offsets = Offsets { fileOffset :: Int,
                         rankOffset :: Int
                       } deriving (Show, Eq)

-- Направления движения. Необходимы для Слона, Ладьи и Ферзя
data Directions = Directions { fileDirection :: Int,
                               rankDirection :: Int
                             } deriving (Show, Eq)

-- Информация, необходимая для отмены хода
data UndoInfo = UndoInfo { capturedPiece :: Maybe Piece,
                           prevHalfMoveCount :: Int, 
                           prevEnPassantTarget :: Maybe Pos,
                           prevCastlingRights :: CastlingRights
                         } deriving (Show, Eq)

-- Запись о ходе, включая информацию для отмены
data MoveRecord = MoveRecord { playedMove :: Move,
                               undoInfo :: UndoInfo
                             } deriving (Show, Eq)
