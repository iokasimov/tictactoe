import Data.Default
import Data.Monoid
import Control.Monad
import Control.Monad.Free
import Data.Functor.Rep

import Entities.Mark
import Entities.Coordinate
import Entities.Line
import Entities.Board

move :: Board Mark -> Coordinate () -> Mark -> Maybe (Board Mark)
move board coordinate mark = if index board coordinate /= E then Nothing
	else Just $ change board mark coordinate

main = print "typechecked"