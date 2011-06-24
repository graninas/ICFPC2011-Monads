module Types where

import Control.Monad
import qualified Control.Monad.State as MST (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO, MonadIO(..)) 
import qualified Data.Map as M

data AppType = LeftApp | RightApp
	deriving (Eq, Show, Read)

type Vitality = Int
data Function = FValue Int
			| I
			| Get
	deriving (Eq, Show, Read)

type Field = Function
data Slot = Slot Vitality Field
	deriving (Eq, Show, Read)

type Slots = M.Map Int Slot

data Player = Player0 | Player1
	deriving (Eq, Show, Read)
	
data GameData = GD {propSlots :: Slots,
					opSlots :: Slots,
					curPlayer :: Player}
	deriving (Eq, Show, Read)

type GS a = (MST.StateT GameData Maybe a)
	
defaultSlot = Slot 10000 I
defaultSlots = zip [0..255] (replicate 256 defaultSlot)

dummyGameData = GD {propSlots = M.fromList defaultSlots,
					opSlots   = M.fromList defaultSlots,
					curPlayer = Player0}

currentPlayerSlots (GD slots _ Player0) = slots 
currentPlayerSlots (GD _ slots Player1) = slots


field :: Int -> Slots -> Maybe Function
field i slots = do
			case M.lookup i slots of
				Just (Slot v field) -> Just field
				Nothing -> Nothing

fZero :: [Maybe Function] -> GS (Maybe Function)
fZero [] = return $ Just $ FValue 0
fZero _  = return $ Nothing

fI :: [Maybe Function] -> GS (Maybe Function)
fI (Just func:[]) = return $ Just $ func
fI _ = return Nothing

fGet :: [Maybe Function] -> GS (Maybe Function)
fGet (Just (FValue i):[]) =
		do
			gd <- MST.get
			return $ field i (currentPlayerSlots gd)
fGet _ = return Nothing

test1 = do
			x <- fZero []
			fI [x]
test2 = [fI, fZero, fGet]