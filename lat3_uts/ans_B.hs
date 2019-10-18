import Data.Array
import Data.List
import Control.Monad
import Control.Applicative
import System.IO
-- import SOE

type Position = (Int, Int)

data Color = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White
     deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

data Direction = North | East | South | West
    deriving (Eq, Show, Enum)

data RobotState = RobotState { 
        position :: Position, 
        facing :: Direction,
        pen :: Bool,
        color :: Color, 
        treasure :: [Position],
        pocket :: Int, 
        energy :: Int
    } deriving Show
