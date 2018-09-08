{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Wasm.Text.Type where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Char
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import Data.Text (Text)

data Tok
  = TokTkeyword Text
  | TokTu32 Integer
  | TokTu64 Integer
  | TokTs32 Integer
  | TokTs64 Integer
  | TokTf32 Float
  | TokTf64 Double
  | TokTstring ByteString
  | TokTid Text
  | TokTlparen
  | TokTrparen
  | TokTreserved Text
  deriving (Eq, Read, Show)

tok :: Parser Tok
tok =
  tokTkeyword <|> tokTu32 <|> tokTu64 <|> tokTs32 <|> tokTs64 <|> tokTf32 <|>
  tokTf64 <|>
  tokTstring <|>
  tokTid <|>
  tokTlparen <|>
  tokTrparen <|>
  tokTreserved

tokTkeyword :: Parser Tok
tokTkeyword =
  fmap TokTkeyword $ do
    c <- P.peekChar
    case c of
      Just x
        | isAsciiLower x -> P.takeWhile1 isIdChar
      _ -> empty

tokTu32 :: Parser Tok
tokTu32 = fmap TokTu32 (uN 32)

tokTu64 :: Parser Tok
tokTu64 = fmap TokTu64 (uN 64)

tokTs32 :: Parser Tok
tokTs32 = fmap TokTs32 (sN 32)

tokTs64 :: Parser Tok
tokTs64 = fmap TokTs64 (sN 64)

tokTf32 :: Parser Tok
tokTf32 = fmap TokTf32 fN

tokTf64 :: Parser Tok
tokTf64 = fmap TokTf64 fN

tokTstring :: Parser Tok
tokTstring = fmap TokTstring $ do
  P.skip (== '=')
  s <- many stringelem
  P.skip (== '=')
  let bytes =
        ByteString.Lazy.toStrict
          (ByteString.Builder.toLazyByteString (mconcat s))
  guard (ByteString.length bytes < 2 ^ 32)
  pure bytes
  where
    stringelem =
      (do P.skip (== '\\')
          n <- fmap fromInteger hexdigit
          m <- fmap fromInteger hexdigit
          pure (ByteString.Builder.word8 (16 * n + m))) <|>
      (fmap ByteString.Builder.charUtf8 stringchar)
    stringchar =
      (do P.skip (== '\\')
          c <- P.anyChar
          case c of
            't' -> pure '\x09'
            'n' -> pure '\x0A'
            'r' -> pure '\x0D'
            '"' -> pure '\x22'
            '\'' -> pure '\x27'
            '\\' -> pure '\x5C'
            'u' -> do
              P.skip (== '{')
              n <- hexnum
              P.skip (== '}')
              pure (toEnum (fromInteger n))
            _ -> empty) <|>
      (do c <- P.anyChar
          guard (c >= '\x20' && c /= '\x7F' && c /= '"' && c /= '\\')
          pure c)

tokTid :: Parser Tok
tokTid =
  fmap TokTid $ do
    P.skip (== '$')
    P.takeWhile1 isIdChar

tokTlparen :: Parser Tok
tokTlparen = P.skip (== '(') *> pure TokTlparen

tokTrparen :: Parser Tok
tokTrparen = P.skip (== ')') *> pure TokTrparen

tokTreserved :: Parser Tok
tokTreserved = fmap TokTreserved (P.takeWhile1 isIdChar)

isIdChar :: Char -> Bool
isIdChar = P.inClass "0-9a-zA-Z!#$%&'*+./:<=>?@\\^_`|~-"

sign :: Parser Integer
sign = (P.char '+' *> pure 1) <|> (P.char '-' *> pure (-1)) <|> pure 1

digit :: Parser Integer
digit = do
  c <- P.satisfy isDigit
  let n = fromEnum c - fromEnum '0'
  pure (toInteger n)

hexdigit :: Parser Integer
hexdigit =
  digit <|>
  (do c <- P.satisfy (\c -> c >= 'a' && c <= 'f')
      let n = (fromEnum c - fromEnum 'a') + 10
      pure (toInteger n)) <|>
  (do c <- P.satisfy (\c -> c >= 'A' && c <= 'F')
      let n = (fromEnum c - fromEnum 'A') + 10
      pure (toInteger n))

num :: Parser Integer
num =
  (do n <- num
      P.skip (== '_') <|> pure ()
      d <- digit
      pure (n * 10 + d)) <|>
  digit

hexnum :: Parser Integer
hexnum =
  (do n <- hexnum
      P.skip (== '_') <|> pure ()
      d <- hexdigit
      pure (n * 10 + d)) <|>
  hexdigit

uN :: Int -> Parser Integer
uN size = do
  n <- (P.string "0x" *> hexnum) <|> num
  guard (n < 2 ^ size)
  pure n

sN :: Int -> Parser Integer
sN size = do
  s <- sign
  n <- (P.string "0x" *> hexnum) <|> num
  let x = s * n
  guard ((-2) ^ (size - 1) <= x && x < 2 ^ (size - 1))
  pure x

frac :: Parser Rational  
frac =
  (do d <- fmap fromInteger digit
      P.skip (== '_')
      p <- fmap fromInteger digit
      q <- frac
      pure ((d + (p + q) / 10) / 10)) <|>
  (do d <- fmap fromInteger digit
      q <- frac
      pure ((d + q) / 10)) <|>
  pure 0

hexfrac :: Parser Rational
hexfrac =
  (do d <- fmap fromInteger hexdigit
      P.skip (== '_')
      p <- fmap fromInteger hexdigit
      q <- hexfrac
      pure ((d + (p + q) / 10) / 10)) <|>
  (do d <- fmap fromInteger hexdigit
      q <- hexfrac
      pure ((d + q) / 10)) <|>
  pure 0

float :: Parser Rational
float =
  (do p <- fmap fromInteger num
      P.skip (== '.')
      q <- frac
      P.skip (\c -> c == 'E' || c == 'e')
      s <- sign
      e <- num
      pure ((p + q) * 10 ^ (s * e))) <|>
  (do p <- fmap fromInteger num
      P.skip (\c -> c == 'E' || c == 'e')
      s <- sign
      e <- num
      pure (p * 10 ^ (s * e))) <|>
  (do p <- fmap fromInteger num
      P.skip (== '.')
      q <- frac
      pure (p + q))

hexfloat :: Parser Rational
hexfloat =
  P.string "0x" *>
  ((do p <- fmap fromInteger hexnum
       P.skip (== '.')
       q <- hexfrac
       P.skip (\c -> c == 'E' || c == 'e')
       s <- sign
       e <- num
       pure ((p + q) * 2 ^ (s * e))) <|>
   (do p <- fmap fromInteger hexnum
       P.skip (\c -> c == 'E' || c == 'e')
       s <- sign
       e <- num
       pure (p * 2 ^ (s * e))) <|>
   (do p <- fmap fromInteger hexnum
       P.skip (== '.')
       q <- hexfrac
       pure (p + q)))

signif :: Integral a => a -> a
signif 32 = 23
signif 64 = 52
signif _ = undefined

nan :: (Integral a, RealFloat b) => a -> b
nan = undefined

class SizedFloat a where
  floatSizeBits :: Proxy a -> Int

instance SizedFloat Float where
  floatSizeBits _ = 32

instance SizedFloat Double where
  floatSizeBits _ = 64

fNmag ::
     forall a. (RealFloat a, SizedFloat a, Read a)
  => Parser a
fNmag =
  (do _ <- P.string "nan:0x"
      n <- hexnum
      guard (1 <= n && n < 2 ^ signif size)
      pure (nan n)) <|>
  (do _ <- P.string "nan"
      pure (nan (2 ^ (signif size - 1)))) <|>
  (do _ <- P.string "inf"
      pure (read "Infinity")) <|>
  (do z <- hexfloat
      let x = fromRational z
      guard (x /= read "Infinity" && x /= read "-Infinity")
      pure x) <|>
  (do z <- float
      let x = fromRational z
      guard (x /= read "Infinity" && x /= read "-Infinity")
      pure x)
  where
    size = floatSizeBits (Proxy :: Proxy a)

fN ::
     forall a. (RealFloat a, SizedFloat a, Read a)
  => Parser a
fN = do
  s <- sign
  z <- fNmag
  pure (fromInteger s * z)
