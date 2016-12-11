module HFish.Interpreter.Posix.IO.Text where

import Data.Functor.Identity
import Control.Monad
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
  text's <- fromUnicodef $ LT.toChunks bs
  -- For performance reasons we use fromUnicodef
  --  instead of reapeated application of fromUnicode.
  forM_ text's $ fdPutB fd


-- Todo: investigate proper use of the "fallback" argument to the ICU open function

-- | Decode 'B.ByteString' using the local(e) encoding.
--   On most modern unix systems this will likely be utf8.
--   (Implememted in terms of toUnicodef).
toUnicode :: B.ByteString -> IO T.Text
toUnicode = 
  fmap runIdentity . toUnicodef . Identity

-- | Encode 'B.ByteString' using the local(e) encoding.
--   On most modern unix systems this will likely be utf8.
--   (Implememted in terms of fromUnicodef).
fromUnicode :: T.Text -> IO B.ByteString
fromUnicode = 
  fmap runIdentity . fromUnicodef . Identity

-- | Functor version of 'toUnicode'.
toUnicodef :: Functor f => f B.ByteString -> IO (f T.Text)
toUnicodef fbs = do
  converter <- U.open ""{-Use Default Encoding-} Nothing{-?-}
  return $ fmap (U.toUnicode converter) fbs

-- | Functor version of fromUnicode'.
fromUnicodef :: Functor f => f T.Text -> IO (f B.ByteString)
fromUnicodef ftext = do
  converter <- U.open ""{-Use Default Encoding-} Nothing{-?-}
  return $ fmap (U.fromUnicode converter) ftext



