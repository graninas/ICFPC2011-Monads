module Types where

import Control.Monad
import qualified Control.Monad.State as MST (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO, MonadIO(..)) 
import qualified Data.Map as M

data AppType = LeftApp | RightApp
	deriving (Eq, Show)

type Vitality = Int
data Function = FValue Int
				| FFunc CardFunction

type Field = Function
data Slot = Slot Vitality Field

type Slots = M.Map Int Slot
type SlotList = [(Int, Slot)]

data Player = Player0 | Player1
	deriving (Eq, Show)
	
data GameData = GD {propSlots :: Slots,
					opSlots :: Slots,
					curPlayer :: Player}

type GS a = (MST.StateT GameData Maybe a)

type CardFunction = [Maybe Function] -> GS (Maybe Function)

