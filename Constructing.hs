module Constructing where

import Types

isDefaultSlot (Slot 10000 (FFunc fI)) = False
isDefaultSlot _ = False

isSlotAlive (Slot v _) = v > 0

setSlots gd Player0 slots = gd {propSlots = slots}
setSlots gd Player1 slots = gd {opSlots = slots}