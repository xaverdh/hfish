module HFish.Interpreter.Posix.IO.Text where

import System.Posix.Types
import HFish.Interpreter.Posix.IO.ByteString

import qualified Data.Text.ICU.Convert as U
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B


fdGetContentsT :: Fd -> IO T.Text
fdGetContentsT fd =
  toUnicode =<< fdGetContentsB fd

fdGetLineT :: Fd -> IO T.Text
fdGetLineT fd =
  toUnicode =<< fdGetLineB fd

fdPutT :: Fd -> T.Text -> IO ()
fdPutT fd text = do
  bs <- fromUnicode text
  fdPutB fd bs

fdGetContentsLT :: Fd -> IO LT.Text
fdGetContentsLT fd = do
  bs's <- fdGetContentsByteStrings fd
  LT.fromChunks <$> mapM toUnicode bs's

fdGetLineLT :: Fd -> IO LT.Text
fdGetLineLT fd = do
  bs's <- fdGetLineByteStrings fd
  LT.fromChunks <$> mapM toUnicode bs's

fdPutLT :: Fd -> LT.Text -> IO ()
fdPutLT fd bs = do
  text <- fromUnicode $ LT.toStrict bs
  fdPutB fd text


-- Todo: investigate proper use of the "fallback" argument to the ICU open function

-- | Decode 'B.ByteString' using the local(e) encoding.
--   On most modern unix systems this will likely be utf8.
toUnicode :: B.ByteString -> IO T.Text
toUnicode bs = do
  converter <- U.open ""{-Use Default Encoding-} Nothing{-?-}
  return $ U.toUnicode converter bs

-- | Encode 'B.ByteString' using the local(e) encoding.
--   On most modern unix systems this will likely be utf8.
fromUnicode :: T.Text -> IO B.ByteString
fromUnicode text = do
  converter <- U.open ""{-Use Default Encoding-} Nothing{-?-}
  return $ U.fromUnicode converter text



