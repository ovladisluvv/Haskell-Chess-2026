module Bot (isBotTurn, makeBotMove) where

import Types (GameState(..), MenuState(..))
import Rules (allLegalMoves, makeMove, isCheckmate, isDraw)
import System.Random (randomR)

-- Проверяет, ход ли сейчас бота
isBotTurn :: GameState -> Bool
isBotTurn state = 
    menuState state == Hidden 
    && not (isCheckmate state || isDraw state) 
    && Just (activePlayer state) == botColor state

-- Совершает случайный ход бота
makeBotMove :: GameState -> GameState
makeBotMove state = 
    let moves = allLegalMoves state
    in if null moves
       then state
       else let (idx, newGen) = randomR (0, length moves - 1) (botGen state)
                chosenMove = moves !! idx
                newState = makeMove state chosenMove
            in newState { botGen = newGen, promotionState = Nothing, selectedPos = Nothing }
