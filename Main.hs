import Data.Default
import Data.Monoid
import Control.Monad
import Control.Monad.Free
import Data.Functor.Rep

import qualified Entities.Mark as Mark
import qualified Entities.Coordinate as Coordinate
import qualified Entities.Board as Board
import qualified Entities.Player as Player

type Mark = Mark.Mark
type Coordinate = Coordinate.Coordinate
type Board = Board.Board
type Player = Player.Player

data Turn = Turn { player' :: Player, board' :: Board Mark } 

data Instruction next = Start next | Move (Turn -> next) | End Player
type Playing = Free Instruction

alternating :: Turn -> IO Turn
alternating (Turn player board) = 
	move (Player.another player) board where

		move :: Player -> Board Mark -> IO Turn
		move player board = do
			coordinate <- Coordinate.ask
			if index board coordinate /= Mark.E
			then print "This cell already filled" >> move player board
			else return $ Turn player $ Board.change board (Player.whose player) coordinate

main = print "typechecked"


