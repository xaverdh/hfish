{-# language PackageImports #-}
module HFish.Interpreter.Posix.IO.ByteString where

import System.Posix.Types
import System.IO.Error as IOE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Internal as BI
-- import qualified System.Posix.IO as P
import qualified "unix-bytestring" System.Posix.IO.ByteString as BIO
import qualified Data.Word8 as W
import qualified Foreign as F

fdGetContentsB :: Fd -> IO B.ByteString
fdGetContentsB fd =
  LB.toStrict <$> fdGetContentsLB fd

fdGetLineB :: Fd -> IO B.ByteString
fdGetLineB fd =
  LB.toStrict <$> fdGetLineLB fd

fdPutB :: Fd -> B.ByteString -> IO ()
fdPutB = writeLoop
  where
    writeLoop fd bs
      | bs == B.empty = return ()
      | otherwise = do
          cnt <- BIO.fdWrite fd bs
          writeLoop fd $ B.drop (fromEnum cnt) bs

{-
-- | Outputs a 'B.ByteString' to the specified Fd'.
fdPutB :: Fd -> B.ByteString -> IO ()
fdPutB fd (BI.PS ps s l) =
  F.withForeignPtr ps $ \p ->
    writeLoop (p `F.plusPtr` s) (toEnum l)
  where
    writeLoop p = \case
      0 -> return ()
      len -> do
        bc <- P.fdWriteBuf fd p len
        writeLoop (p `F.plusPtr` fromEnum bc) (len - bc)
-}


fdGetContentsLB :: Fd -> IO LB.ByteString
fdGetContentsLB fd =
  LB.fromChunks
  <$> fdGetContentsByteStrings fd

fdGetLineLB :: Fd -> IO LB.ByteString
fdGetLineLB fd =
  LB.fromChunks
  <$> fdGetLineByteStrings fd

fdPutLB :: Fd -> LB.ByteString -> IO ()
fdPutLB fd =
  mapM_ (fdPutB fd) . LB.toChunks


fdGetContentsByteStrings :: Fd -> IO [B.ByteString]
fdGetContentsByteStrings fd = readLoop fd
  where
    chunkSize :: ByteCount
    chunkSize = toEnum (2^14)

    readLoop :: Fd -> IO [B.ByteString]
    readLoop fd = do
      bs <- mapSingletonList $ BIO.fdRead fd chunkSize
      (bs++) <$> readLoop fd

    mapSingletonList :: IO a -> IO [a]
    mapSingletonList f = IOE.catchIOError (pure <$> f)
      $ \e ->
        if IOE.isEOFError e
        then return []
        else ioError e

fdGetLineByteStrings :: Fd -> IO [B.ByteString]
fdGetLineByteStrings fd = readLoop fd
  where
    chunkSize :: Int
    chunkSize = 2^14

    readLoop :: Fd -> IO [B.ByteString]
    readLoop fd = do
      bs <- BI.createAndTrim (2^14) $ readTillNl fd 0
      if bs == B.empty || B.last bs == W._lf
        then return [bs]
        else (bs:)  <$> readLoop fd

    readTillNl fd pos buf = do
      cnt <- BIO.fdReadBuf fd buf (toEnum 1)
      if cnt == 0
        then return pos
        else do
          c <- F.peek buf
          if c == W._lf
            then return (pos+1)
            else readTillNl fd (pos+1) (buf `F.plusPtr` 1)



