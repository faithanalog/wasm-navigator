module Wasm.Exec.Type where



data StackVal

newtype Stack = Stack
  { getStack :: [StackVal]
  } deriving (Eq, Read, Show)

data Store = Store
  { storeFuncs :: !(Vector Func)
  , storeTables :: !(Vector TableType)
  , storeMems :: !(Vector Mem)
  , storeGlobal :: !(Vector Global)
  } deriving (Eq, Read, Show)
