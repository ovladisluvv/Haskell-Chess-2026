module Bot (isBotTurn, makeBotMove) where

import Types
import Board (getPiece)
import Rules (allLegalMoves, makeMove, isCheckmate, isDraw)
import System.Random (randomR)

-- Проверяет, ход ли сейчас бота
isBotTurn :: GameState -> Bool
isBotTurn state = 
    menuState state == Hidden 
    && not (isCheckmate state || isDraw state) 
    && Just (activePlayer state) == botColor state

evaluateBoard :: Board -> Int
evaluateBoard b = sum [pieceValue p | f <- [0..7], r <- [0..7], Just p <- [getPiece b (Pos f r)]]
    where
      pieceValue (Piece pt c) = (if c == White then 1 else -1) *
        case pt of
            Pawn -> 10
            Knight -> 30
            Bishop -> 30
            Rook -> 50
            Queen -> 90
            King -> 900

-- Совершает умный ход бота (одноуровневый жадный алгоритм + рандом среди равных)
makeBotMove :: GameState -> GameState
makeBotMove state = 
    let moves = allLegalMoves state
    in if null moves
       then state
       else let c = activePlayer state
                eval m = 
                    let nextGs = makeMove state m
                        score = evaluateBoard (board nextGs)
                        mateBonus = if isCheckmate nextGs then 10000 else 0
                    in if c == White then score + mateBonus else -(score) + mateBonus
                
                evaluated = [(eval m, m) | m <- moves]
                bestScore = maximum (map fst evaluated)
                bestMoves = [m | (s, m) <- evaluated, s == bestScore]
                
                (idx, newGen) = randomR (0, length bestMoves - 1) (botGen state)
                chosenMove = bestMoves !! idx
                newState = makeMove state chosenMove
            in newState { botGen = newGen, promotionState = Nothing, selectedPos = Nothing }
