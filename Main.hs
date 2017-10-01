import Data.Maybe
import Data.Monoid
import Data.Default
import Control.Monad
import Control.Monad.Free
import Data.Functor.Rep

import Entities.Mark
import Entities.Coordinate
import Entities.Board
import Entities.Player

data Turn = Turn { player' :: Player, board' :: (Board Mark) } 

data Play next 
	= Start next
	| Ask (Coordinate () -> next)
	| Wrong (Coordinate ()) next
	| Loop Turn next
	| Final String

type Playing = Free Play

rules :: Playing ()
rules = Free $ Start $ gameloop (Turn Crosses (def :: Board Mark)) where

	gameloop :: Turn -> Playing ()
	gameloop turn = case check (board' turn) of
			Just E -> Free $ Loop turn $ continue turn
			Nothing -> Free $ Loop turn $ Free $ Final "Standoff here!"
			Just O -> Free $ Loop turn $ Free $ Final "Noughts win!"
			Just X -> Free $ Loop turn $ Free $ Final "Crosses win!"

	continue :: Turn -> Playing ()
	continue (Turn player board) = Free $ Ask $ \coordinate ->
		if index board coordinate /= E
		then Free $ Wrong coordinate $ gameloop $ Turn player board
		else gameloop $ Turn (another player) $
			change board (whose player) coordinate

run :: Playing () -> IO ()
run (Pure r) = return r
run (Free (Start next)) = run next
run (Free (Ask f)) = print "Your turn: " >> ask >>= run . f
run (Free (Wrong coordinate next)) = print ((show coordinate) ++ " already filled") >> run next
run (Free (Loop turn next)) = print (board' turn) >> run next
run (Free (Final message)) = print message

main = run rules