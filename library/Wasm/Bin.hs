module Wasm.Bin where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Binary.Get as Get
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Int
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Word
import Wasm.Type

uN :: Int -> Parser Word64
uN len | len > 64 = undefined
uN len = do
  n <- fmap fromIntegral P.anyWord8
  let result
        | n < 2 ^ 7 && n < 2 ^ len = pure n
        | n >= 2 ^ 7 && len > 7 = do
          m <- uN (len - 7)
          pure (2 ^ 7 * m + (n - 2 ^ 7))
        | otherwise = empty
  result

u32 :: Parser Word32
u32 = fmap fromIntegral (uN 32)

u64 :: Parser Word64
u64 = fmap fromIntegral (uN 64)

i32 :: Parser Word32
i32 = u32

i64 :: Parser Word64
i64 = u64


sN :: Int -> Parser Int64
sN len | len > 64 = undefined
sN len = do
  n <- fmap fromIntegral P.anyWord8
  let result
        | n < 2 ^ 6 && n < 2 ^ (len - 1) = pure n
        | 2 ^ 6 <= n && n < 2 ^ 7 && n >= (2 ^ 7 - 2 ^ (len - 1)) = pure (n - 2 ^ 7)
        | n >= 2 ^ 7 && len > 7 = do
          m <- sN (len - 7)
          pure (2 ^ 7 * m + (n - 2 ^ 7))
        | otherwise = empty
  result

s32 :: Parser Int32
s32 = fmap fromIntegral (sN 32)

s64:: Parser Int64
s64 = fmap fromIntegral (sN 64)

fN :: Int -> Parser (Either Float Double)
fN 32 = do
  bs <- fmap ByteString.Lazy.fromStrict (P.take 4)
  case Get.runGetOrFail Get.getFloatle bs of
    Left _ -> empty
    Right (_, _, x) -> pure (Left x)
fN 64 = do
  bs <- fmap ByteString.Lazy.fromStrict (P.take 8)
  case Get.runGetOrFail Get.getDoublele bs of
    Left _ -> empty
    Right (_, _, x) -> pure (Right x)
fN _ = empty

f32 :: Parser Float
f32 = do
  Left x <- fN 32
  pure x

f64 :: Parser Double
f64 = do
  Right x <- fN 64
  pure x

vec :: Parser a -> Parser (Vector a)
vec g = do
  len <- fmap fromIntegral u32
  Vector.replicateM len g

bytevec :: Parser ByteString
bytevec = do
  len <- fmap fromIntegral u32
  P.take len

name :: Parser Text
name = do
  raw <- bytevec
  case Text.decodeUtf8' raw of
    Left _ -> empty
    Right str -> pure str

valtypeFromWord8 :: Word8 -> Maybe ValType
valtypeFromWord8 0x7F = Just VTi32
valtypeFromWord8 0x7E = Just VTi64
valtypeFromWord8 0x7D = Just VTf32
valtypeFromWord8 0x7C = Just VTf64
valtypeFromWord8 _ = Nothing

valtype :: Parser ValType
valtype = do
  x <- P.anyWord8
  case valtypeFromWord8 x of
    Just vt -> pure vt
    Nothing -> empty

blocktype :: Parser BlockType
blocktype = do
  x <- P.anyWord8
  case x of
    0x40 -> pure (BlockType Nothing)
    _ ->
      case valtypeFromWord8 x of
        Just vt -> pure (BlockType (Just vt))
        Nothing -> empty

functype :: Parser FuncType
functype = do
  P.skip (== 0x60)
  params <- vec valtype
  results <- vec valtype
  pure (FuncType params results)

limits :: Parser Limits
limits = do
  x <- P.satisfy (\x -> x == 0 || x == 1)
  mn <- u32
  mx <-
    case x of
      0 -> pure Nothing
      1 -> fmap Just u32
      _ -> undefined
  pure (Limits mn mx)

memtype :: Parser Limits
memtype = limits

elemtypeFromWord8 :: Word8 -> Maybe ElemType
elemtypeFromWord8 0x70 = Just ETAnyFunc
elemtypeFromWord8 _ = Nothing

elemtype :: Parser ElemType
elemtype = do
  x <- P.anyWord8
  case elemtypeFromWord8 x of
    Just et -> pure et
    Nothing -> empty

tabletype :: Parser TableType
tabletype = liftA2 TableType elemtype limits

mutabilityFromWord8 :: Word8 -> Maybe Mutability
mutabilityFromWord8 0x00 = Just MConst
mutabilityFromWord8 0x01 = Just MVar
mutabilityFromWord8 _ = Nothing

mut :: Parser Mutability
mut = do
  x <- P.anyWord8
  case mutabilityFromWord8 x of
    Just m -> pure m
    Nothing -> empty

globaltype :: Parser GlobalType
globaltype = liftA2 GlobalType valtype mut

memarg :: Parser MemArg
memarg = liftA2 MemArg u32 u32

instr :: Parser Instr
instr = do
  i <- P.anyWord8
  case i of
    -- Control
    0x00 -> pure IUnreachable
    0x01 -> pure INop
    0x02 -> do
      rt <- blocktype
      ins <- fmap Vector.fromList (many instr)
      P.skip (== 0x0B)
      pure (IBlock rt ins)
    0x03 -> do
      rt <- blocktype
      ins <- fmap Vector.fromList (many instr)
      P.skip (== 0x0B)
      pure (ILoop rt ins)
    0x04 -> do
      rt <- blocktype
      ins <- fmap Vector.fromList (many instr)
      P.skip (== 0x0B)
      pure (IIf rt ins Vector.empty)
    0x05 -> do
      rt <- blocktype
      insT <- fmap Vector.fromList (many instr)
      insF <- 
        (P.skip (== 0x05) *> fmap Vector.fromList (many instr)) <|> pure Vector.empty
      P.skip (== 0x0B)
      pure (IIf rt insT insF)
    0x0C -> fmap IBr labelidx
    0x0D -> fmap IBrIf labelidx
    0x0E -> liftA2 IBrTable (vec labelidx) labelidx
    0x0F -> pure IReturn
    0x10 -> fmap ICall funcidx
    0x11 -> fmap ICallIndirect typeidx <* P.skip (== 0x00)
    -- Parametric
    0x1A -> pure IDrop
    0x1B -> pure ISelect
    -- Variable
    0x20 -> fmap IGetLocal localidx
    0x21 -> fmap ISetLocal localidx
    0x22 -> fmap ITeeLocal localidx
    0x23 -> fmap IGetGlobal globalidx
    0x24 -> fmap ISetGlobal globalidx
    -- Memory
    0x28 -> fmap Ii32Load memarg
    0x29 -> fmap Ii64Load memarg
    0x2A -> fmap If32Load memarg
    0x2B -> fmap If64Load memarg
    0x2C -> fmap Ii32Load8S memarg
    0x2D -> fmap Ii32Load8U memarg
    0x2E -> fmap Ii32Load16S memarg
    0x2F -> fmap Ii32Load16U memarg
    0x30 -> fmap Ii64Load8S memarg
    0x31 -> fmap Ii64Load8U memarg
    0x32 -> fmap Ii64Load16S memarg
    0x33 -> fmap Ii64Load16U memarg
    0x34 -> fmap Ii64Load32S memarg
    0x35 -> fmap Ii64Load32U memarg
    0x36 -> fmap Ii32Store memarg
    0x37 -> fmap Ii64Store memarg
    0x38 -> fmap If32Store memarg
    0x39 -> fmap If64Store memarg
    0x3A -> fmap Ii32Store8 memarg
    0x3B -> fmap Ii32Store16 memarg
    0x3C -> fmap Ii64Store8 memarg
    0x3D -> fmap Ii64Store16 memarg
    0x3E -> fmap Ii64Store32 memarg
    0x3F -> pure IMemorySize
    0x40 -> pure IMemoryGrow
    -- Numeric const
    0x41 -> fmap Ii32Const i32
    0x42 -> fmap Ii64Const i64
    0x43 -> fmap If32Const f32
    0x44 -> fmap If64Const f64
    -- Numeric i32 comparison
    0x45 -> pure Ii32EqZ
    0x46 -> pure Ii32Eq
    0x47 -> pure Ii32Ne
    0x48 -> pure Ii32LtS
    0x49 -> pure Ii32LtU
    0x4A -> pure Ii32GtS
    0x4B -> pure Ii32GtU
    0x4C -> pure Ii32LeS
    0x4D -> pure Ii32LeU
    0x4E -> pure Ii32GeS
    0x4F -> pure Ii32GeU
    -- Numeric i64 comparison
    0x50 -> pure Ii64EqZ
    0x51 -> pure Ii64Eq
    0x52 -> pure Ii64Ne
    0x53 -> pure Ii64LtS
    0x54 -> pure Ii64LtU
    0x55 -> pure Ii64GtS
    0x56 -> pure Ii64GtU
    0x57 -> pure Ii64LeS
    0x58 -> pure Ii64LeU
    0x59 -> pure Ii64GeS
    0x5A -> pure Ii64GeU
    -- Numeric f32 comparison
    0x5B -> pure If32Eq
    0x5C -> pure If32Ne
    0x5D -> pure If32Lt
    0x5E -> pure If32Gt
    0x5F -> pure If32Le
    0x60 -> pure If32Ge
    -- Numeric f64 comparison
    0x61 -> pure If64Eq
    0x62 -> pure If64Ne
    0x63 -> pure If64Lt
    0x64 -> pure If64Gt
    0x65 -> pure If64Le
    0x66 -> pure If64Ge
    -- Numeric i32 ALU
    0x67 -> pure Ii32Clz
    0x68 -> pure Ii32Ctz
    0x69 -> pure Ii32PopCnt
    0x6A -> pure Ii32Add
    0x6B -> pure Ii32Sub
    0x6C -> pure Ii32Mul
    0x6D -> pure Ii32DivS
    0x6E -> pure Ii32DivU
    0x6F -> pure Ii32RemS
    0x70 -> pure Ii32RemU
    0x71 -> pure Ii32And
    0x72 -> pure Ii32Or
    0x73 -> pure Ii32Xor
    0x74 -> pure Ii32Shl
    0x75 -> pure Ii32ShrS
    0x76 -> pure Ii32ShrU
    0x77 -> pure Ii32RotL
    0x78 -> pure Ii32RotR
    -- Numeric i64 ALU
    0x79 -> pure Ii64Clz
    0x7A -> pure Ii64Ctz
    0x7B -> pure Ii64PopCnt
    0x7C -> pure Ii64Add
    0x7D -> pure Ii64Sub
    0x7E -> pure Ii64Mul
    0x7F -> pure Ii64DivS
    0x80 -> pure Ii64DivU
    0x81 -> pure Ii64RemS
    0x82 -> pure Ii64RemU
    0x83 -> pure Ii64And
    0x84 -> pure Ii64Or
    0x85 -> pure Ii64Xor
    0x86 -> pure Ii64Shl
    0x87 -> pure Ii64ShrS
    0x88 -> pure Ii64ShrU
    0x89 -> pure Ii64RotL
    0x8A -> pure Ii64RotR
    -- Numeric f32 FPU
    0x8B -> pure If32Abs
    0x8C -> pure If32Neg
    0x8D -> pure If32Ceil
    0x8E -> pure If32Floor
    0x8F -> pure If32Trunc
    0x90 -> pure If32Nearest
    0x91 -> pure If32Sqrt
    0x92 -> pure If32Add
    0x93 -> pure If32Sub
    0x94 -> pure If32Mul
    0x95 -> pure If32Div
    0x96 -> pure If32Min
    0x97 -> pure If32Max
    0x98 -> pure If32CopySign
    -- Numeric f64 FPU
    0x99 -> pure If64Abs
    0x9A -> pure If64Neg
    0x9B -> pure If64Ceil
    0x9C -> pure If64Floor
    0x9D -> pure If64Trunc
    0x9E -> pure If64Nearest
    0x9F -> pure If64Sqrt
    0xA0 -> pure If64Add
    0xA1 -> pure If64Sub
    0xA2 -> pure If64Mul
    0xA3 -> pure If64Div
    0xA4 -> pure If64Min
    0xA5 -> pure If64Max
    0xA6 -> pure If64CopySign
    -- Numeric Conversions
    0xA7 -> pure Ii32Wrapi64
    0xA8 -> pure Ii32TruncSf32
    0xA9 -> pure Ii32TruncUf32
    0xAA -> pure Ii32TruncSf64
    0xAB -> pure Ii32TruncUf64
    0xAC -> pure Ii64ExtendSi32
    0xAD -> pure Ii64ExtendUi32
    0xAE -> pure Ii64TruncSf32
    0xAF -> pure Ii64TruncUf32
    0xB0 -> pure Ii64TruncSf64
    0xB1 -> pure Ii64TruncUf64
    0xB2 -> pure If32ConvertSi32
    0xB3 -> pure If32ConvertUi32
    0xB4 -> pure If32ConvertSi64
    0xB5 -> pure If32ConvertUi64
    0xB6 -> pure If32Demotef64
    0xB7 -> pure If64ConvertSi32
    0xB8 -> pure If64ConvertUi32
    0xB9 -> pure If64ConvertSi64
    0xBA -> pure If64ConvertUi64
    0xBB -> pure If64Promotef32
    0xBC -> pure Ii32Reinterpretf32
    0xBD -> pure Ii64Reinterpretf64
    0xBE -> pure If32Reinterpreti32
    0xBF -> pure If64Reinterpreti64
    _ -> empty

end :: Parser ()
end = P.skip (== 0x0B)

expr :: Parser (Vector Instr)
expr = fmap Vector.fromList (many instr) <* end

typeidx :: Parser TypeIdx
typeidx = fmap TypeIdx u32

funcidx :: Parser FuncIdx
funcidx = fmap FuncIdx u32

tableidx :: Parser TableIdx
tableidx = fmap TableIdx u32

memidx :: Parser MemIdx
memidx = fmap MemIdx u32

globalidx :: Parser GlobalIdx
globalidx = fmap GlobalIdx u32

localidx :: Parser LocalIdx
localidx = fmap LocalIdx u32

labelidx :: Parser LabelIdx
labelidx = fmap LabelIdx u32
