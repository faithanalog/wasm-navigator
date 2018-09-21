module Wasm.Type where

import Data.Vector (Vector)
import Data.Word
import Data.ByteString (ByteString)
import Data.Text (Text)

data ValType
  = VTi32
  | VTi64
  | VTf32
  | VTf64
  deriving (Eq, Read, Show, Ord, Enum)

newtype BlockType = BlockType
  { getBlockType :: Maybe ValType
  } deriving (Eq, Read, Show)

data FuncType = FuncType
  { ftParams :: !(Vector ValType)
  , ftResults :: !(Vector ValType)
  } deriving (Eq, Read, Show)

data Limits = Limits
  { limitsMin :: !Word32
  , limitsMax :: !(Maybe Word32)
  } deriving (Eq, Read, Show)

newtype MemType = MemType
  { getMemType :: Limits
  } deriving (Eq, Read, Show)

data TableType = TableType
  { ttElemType :: !ElemType
  , ttLimits :: !Limits
  } deriving (Eq, Read, Show)

data ElemType =
  ETAnyFunc
  deriving (Eq, Read, Show, Ord, Enum)

data GlobalType = GlobalType
  { gtValType :: !ValType
  , gtMutability :: !Mutability
  } deriving (Eq, Read, Show)

data Mutability
  = MConst
  | MVar
  deriving (Eq, Read, Show, Ord, Enum)

data MemArg = MemArg
  { maAlign :: !Word32
  , maOffset :: !Word32
  } deriving (Eq, Read, Show)

newtype TypeIdx = TypeIdx
  { getTypeIdx :: Word32
  } deriving (Eq, Read, Show, Ord)

newtype FuncIdx = FuncIdx
  { getFuncIdx :: Word32
  } deriving (Eq, Read, Show, Ord)

newtype TableIdx = TableIdx
  { getTableIdx :: Word32
  } deriving (Eq, Read, Show, Ord)

newtype MemIdx = MemIdx
  { getMemIdx :: Word32
  } deriving (Eq, Read, Show, Ord)

newtype GlobalIdx = GlobalIdx
  { getGlobalIdx :: Word32
  } deriving (Eq, Read, Show, Ord)

newtype LocalIdx = LocalIdx
  { getLocalIdx :: Word32
  } deriving (Eq, Read, Show, Ord)

newtype LabelIdx = LabelIdx
  { getLabelIdx :: Word32
  } deriving (Eq, Read, Show, Ord)

data Import = Import
  { importModule :: !Text
  , importName :: !Text
  , importDesc :: !ImportDesc
  } deriving (Eq, Read, Show)

data ImportDesc
  = ImportDescFunc !TypeIdx
  | ImportDescTable !TableType
  | ImportDescMem !MemType
  | ImportDescGlobal !GlobalType
  deriving (Eq, Read, Show)

newtype Table = Table
  { tableType :: TableType
  } deriving (Eq, Read, Show)

newtype Mem = Mem
  { memType :: MemType
  } deriving (Eq, Read, Show)

data Global = Global
  { globalType :: !GlobalType
  , globalInit :: !Expr
  } deriving (Eq, Read, Show)

data Export = Export
  { exportName :: !Text
  , exportDesc :: !ExportDesc
  } deriving (Eq, Read, Show)

data ExportDesc
  = ExportDescFunc !FuncIdx
  | ExportDescTable !TableIdx
  | ExportDescMem !MemIdx
  | ExportDescGlobal !GlobalIdx
  deriving (Eq, Read, Show)

newtype Start = Start
  { getStart :: FuncIdx
  } deriving (Eq, Read, Show)

data Elem = Elem
  { elemTable :: !TableIdx
  , elemOffset :: !Expr
  , elemInit :: !(Vector FuncIdx)
  } deriving (Eq, Read, Show)

data Code = Code
  { codeLocals :: !(Vector ValType)
  , codeBody :: !Expr
  } deriving (Eq, Read, Show)

newtype Expr = Expr
  { getExpr :: Vector Instr
  } deriving (Eq, Read, Show)

data DataSegment = DataSegment
  { dataSegmentData :: !MemIdx
  , dataSegmentOffset :: !Expr
  , dataSegmentInit :: !ByteString
  } deriving (Eq, Read, Show)

data Func = Func
  { funcType :: !TypeIdx
  , funcLocals :: !(Vector ValType)
  , funcBody :: !Expr
  } deriving (Eq, Read, Show)

data Module = Module
  { moduleTypes :: !(Vector FuncType)
  , moduleFuncs :: !(Vector Func)
  , moduleTables :: !(Vector TableType)
  , moduleMem :: !(Vector Mem)
  , moduleGlobals :: !(Vector Global)
  , moduleElem :: !(Vector Elem)
  , moduleData :: !(Vector DataSegment)
  , moduleStart :: !(Maybe Start)
  , moduleImports :: !(Vector Import)
  , moduleExports :: !(Vector Export)
  } deriving (Eq, Read, Show)

data Section
  = SectionCustom !Text !ByteString
  | SectionType !(Vector FuncType)
  | SectionImport !(Vector Import)
  | SectionFunc !(Vector TypeIdx)
  | SectionTable !(Vector TableType)
  | SectionMem !(Vector Mem)
  | SectionGlobal !(Vector Global)
  | SectionExport !(Vector Export)
  | SectionStart !Start
  | SectionElem !(Vector Elem)
  | SectionCode !(Vector Code)
  | SectionData !(Vector DataSegment)
  deriving (Eq, Read, Show)

data Instr
  = IUnreachable
  | INop
  | IBlock !BlockType !(Vector Instr)
  | ILoop !BlockType !(Vector Instr)
  | IIf !BlockType !(Vector Instr) !(Vector Instr)
  | IBr !LabelIdx
  | IBrIf !LabelIdx
  | IBrTable !(Vector LabelIdx) !LabelIdx
  | IReturn
  | ICall !FuncIdx
  | ICallIndirect !TypeIdx
  | IDrop
  | ISelect
  | IGetLocal !LocalIdx
  | ISetLocal !LocalIdx
  | ITeeLocal !LocalIdx
  | IGetGlobal !GlobalIdx
  | ISetGlobal !GlobalIdx
  | Ii32Load !MemArg
  | Ii64Load !MemArg
  | If32Load !MemArg
  | If64Load !MemArg
  | Ii32Load8S !MemArg
  | Ii32Load8U !MemArg
  | Ii32Load16S !MemArg
  | Ii32Load16U !MemArg
  | Ii64Load8S !MemArg
  | Ii64Load8U !MemArg
  | Ii64Load16S !MemArg
  | Ii64Load16U !MemArg
  | Ii64Load32S !MemArg
  | Ii64Load32U !MemArg
  | Ii32Store !MemArg
  | Ii64Store !MemArg
  | If32Store !MemArg
  | If64Store !MemArg
  | Ii32Store8 !MemArg
  | Ii32Store16 !MemArg
  | Ii64Store8 !MemArg
  | Ii64Store16 !MemArg
  | Ii64Store32 !MemArg
  | IMemorySize
  | IMemoryGrow
  | Ii32Const !Word32
  | Ii64Const !Word64
  | If32Const !Float
  | If64Const !Double
  | Ii32EqZ
  | Ii32Eq
  | Ii32Ne
  | Ii32LtS
  | Ii32LtU
  | Ii32GtS
  | Ii32GtU
  | Ii32LeS
  | Ii32LeU
  | Ii32GeS
  | Ii32GeU
  | Ii64EqZ
  | Ii64Eq
  | Ii64Ne
  | Ii64LtS
  | Ii64LtU
  | Ii64GtS
  | Ii64GtU
  | Ii64LeS
  | Ii64LeU
  | Ii64GeS
  | Ii64GeU
  | If32Eq
  | If32Ne
  | If32Lt
  | If32Gt
  | If32Le
  | If32Ge
  | If64Eq
  | If64Ne
  | If64Lt
  | If64Gt
  | If64Le
  | If64Ge
  | Ii32Clz
  | Ii32Ctz
  | Ii32PopCnt
  | Ii32Add
  | Ii32Sub
  | Ii32Mul
  | Ii32DivS
  | Ii32DivU
  | Ii32RemS
  | Ii32RemU
  | Ii32And
  | Ii32Or
  | Ii32Xor
  | Ii32Shl
  | Ii32ShrS
  | Ii32ShrU
  | Ii32RotL
  | Ii32RotR
  | Ii64Clz
  | Ii64Ctz
  | Ii64PopCnt
  | Ii64Add
  | Ii64Sub
  | Ii64Mul
  | Ii64DivS
  | Ii64DivU
  | Ii64RemS
  | Ii64RemU
  | Ii64And
  | Ii64Or
  | Ii64Xor
  | Ii64Shl
  | Ii64ShrS
  | Ii64ShrU
  | Ii64RotL
  | Ii64RotR
  | If32Abs
  | If32Neg
  | If32Ceil
  | If32Floor
  | If32Trunc
  | If32Nearest
  | If32Sqrt
  | If32Add
  | If32Sub
  | If32Mul
  | If32Div
  | If32Min
  | If32Max
  | If32CopySign
  | If64Abs
  | If64Neg
  | If64Ceil
  | If64Floor
  | If64Trunc
  | If64Nearest
  | If64Sqrt
  | If64Add
  | If64Sub
  | If64Mul
  | If64Div
  | If64Min
  | If64Max
  | If64CopySign
  | Ii32Wrapi64
  | Ii32TruncSf32
  | Ii32TruncUf32
  | Ii32TruncSf64
  | Ii32TruncUf64
  | Ii64ExtendSi32
  | Ii64ExtendUi32
  | Ii64TruncSf32
  | Ii64TruncUf32
  | Ii64TruncSf64
  | Ii64TruncUf64
  | If32ConvertSi32
  | If32ConvertUi32
  | If32ConvertSi64
  | If32ConvertUi64
  | If32Demotef64
  | If64ConvertSi32
  | If64ConvertUi32
  | If64ConvertSi64
  | If64ConvertUi64
  | If64Promotef32
  | Ii32Reinterpretf32
  | Ii64Reinterpretf64
  | If32Reinterpreti32
  | If64Reinterpreti64
  deriving (Eq, Read, Show)
