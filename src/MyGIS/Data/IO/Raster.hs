{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MyGIS.Data.IO.Raster
(
    Raster (..)
  , Options (..)

  , defaultOptions

  , pixelGenerator
  , pointGenerator
  , replicateGenerator
  , readerS
  , writerS
  , sink

  , runSession
  , (>->)
  , try
) where
import           Codec.Compression.GZip hiding (compress, decompress,
                                                CompressionLevel)
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Proxy
import           Control.Proxy.Safe
import           Data.Binary
import           Data.Binary.Get (getWord16host, getWord32host)
import           Data.Binary.Put (putWord16host, putWord32host)
import           Data.Int
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import qualified Data.ByteString.Lazy as BS
import           Data.Vector.Binary()
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import           MyGIS.Data.GeoReference

data Raster a = Raster {
    options :: Options
  , georef :: GeoReference
  , path    :: FilePath
}

data Options = Opts {
    compression :: CompressionLevel
  , blockSize   :: BlockSize
} deriving Show

instance Binary Options where
  {-# INLINE put #-}
  put (Opts c (bx,by))
    = put c >> put bx >> put by
  {-# INLINE get #-}
  get
    = do c <- get
         bx <- get
         by <- get
         return (Opts c (bx,by))


defaultOptions :: Options
defaultOptions = Opts 9 (256,256)


type BlockSize = (Int,Int)
type CompressionLevel = Int


data FileHeader = FileHeader {
    fhVersion :: Version
  , fhOptions :: !Options
  , fhGeoReference :: !GeoReference
  , fhOffsets :: !(St.Vector BlockOffset)
} deriving Show

instance NFData FileHeader where
instance Binary FileHeader where
  {-# INLINE put #-}
  put fh
    =  put (vMajor $ fhVersion fh)
    >> put (vMinor $ fhVersion fh)
    >> put (fhOptions fh)
    >> put (fhGeoReference fh)
    >> put (fhOffsets fh)
  {-# INLINE get #-}
  get
    = FileHeader <$> liftA2 Version get get <*> get <*> get <*> get


type BlockOffset = Int64

data Version = Version {
    vMajor :: !Int8
  , vMinor :: !Int8
} deriving Show


version10 :: Version
version10 = Version 1 0

fileHeader10 :: Options -> GeoReference -> St.Vector BlockOffset -> FileHeader
fileHeader10 = FileHeader version10


data Block a = Block !(St.Vector a)
instance NFData (Block a) where

class (Binary a, Stm.Storable a) => BlockData a where

instance BlockData Int8
instance BlockData Int16
instance BlockData Int32
instance BlockData Int

instance Binary (Block Int16) where
  {-# INLINE put #-}
  put (Block a) =  putWord32host ((fromIntegral . St.length) a)
                >> St.forM_ a (putWord16host.fromIntegral)
  {-# INLINE get #-}
  get = do
      n <- liftM fromIntegral getWord32host
      -- new unitinialized array
      mv <- (return . unsafePerformIO) $ Stm.new n
      let fill !i
              | i < n = do
                  x <- liftM fromIntegral getWord16host
                  (unsafePerformIO $ Stm.unsafeWrite mv i x) `seq` return ()
                  fill (i+1)

              | otherwise = return ()
      fill 0
      !data_ <- (return . unsafePerformIO) $ St.unsafeFreeze mv
      return $ Block data_


{-
instance BlockData a => Binary (Block a) where
  {-# INLINE put #-}
  put (Block a) = put (St.length a) >> St.forM_ a put
  {-# INLINE get #-}
  get           = do len <- get
                     vec <- St.replicateM len get
                     return $ Block vec
-}


data BlockIx = BlockIx !Int !Int deriving (Eq,Show)
instance NFData BlockIx where





    


-- | reader is a pipe server that reads blocks from a *seekable* file handle
reader :: (Proxy p, BlockData a, Binary (Block a))
  => Handle
  -> BlockIx
  -> Server p BlockIx (Block a) IO ()
reader h = runIdentityK initialize
  where
    initialize ix = do
        lift $ hSetBinaryMode h True
        contents <- lift $ BS.hGetContents h
        let header = (force $ decode contents)
            decompress = decompressor $ compression $ fhOptions header
        loop header decompress ix
    loop fh c !ix =
        case getOffLen fh ix of
             Just (off,len) -> lift (readBlock h c off len)
                               >>= respond >>= loop fh c
             Nothing        -> error "corrupted file"
{-# SPECIALISE INLINE reader :: (Proxy p) =>
       Handle -> BlockIx -> Server p BlockIx (Block Int16) IO () #-}

-- | writer is a pipe client that writes blocks to a file handle
writer :: (Proxy p, BlockData a, Binary (Block a))
  => Handle
  -> Raster a
  -> ()
  -> Client p BlockIx (Block a) IO ()
writer h raster () = runIdentityP $ do
    -- Create a dummy header with the correct number of offsets to compute
    -- the first block's offset.
    -- FIXME: Calculate first block offset without encoding a dummy header
    let header    = fileHeader10 (options raster) (georef raster) dummyRefs
        dummyRefs = St.replicate nRefs 0
        nRefs     = nx * ny + 1
        fstBlkOff = fromIntegral . BS.length . encode $ header
        (nx,ny)   = numBlocks (georef raster) (options raster)
        compress  = compressor (compression $ options raster)
    
    -- Request all blocks from pipe and write them to file while updating
    -- mutable list of BlockOffsets
    blockRefs <- lift $ Stm.new nRefs
    lift $ hSeek h AbsoluteSeek fstBlkOff
    forM_  (zip [0..] (blockIndexes (nx,ny))) $ \(!i, !ix) -> do
        lift $ liftM fromIntegral (hTell h) >>= Stm.unsafeWrite blockRefs i
        request ix >>= \b -> lift $ writeBlock h compress b

    lift $ liftM fromIntegral (hTell h) >>= Stm.unsafeWrite blockRefs (nRefs-1)
    blockRefs' <- lift $ St.unsafeFreeze blockRefs

    -- Create final header and write it at the beginning of the file
    let finalHeader = fileHeader10 (options raster) (georef raster) blockRefs'
    lift $ hSeek h AbsoluteSeek 0
    lift $ BS.hPut h (encode finalHeader)

{-# SPECIALISE INLINE writer :: (Proxy p) =>
       Handle -> Raster Int16 -> () -> Client p BlockIx (Block Int16) IO () #-}


blockIndexes :: (Int,Int) -> [BlockIx]
blockIndexes (bx,by) = [ BlockIx i j | j <- [0..by-1], i <- [0..bx-1]]
{-# INLINE blockIndexes #-}


getOffLen :: FileHeader -> BlockIx -> Maybe (Integer, Int)
getOffLen h (BlockIx x y)
    = do let nx = fst $ numBlocks (fhGeoReference h) (fhOptions h)
         off  <- fhOffsets h St.!? (nx*y + x)
         off' <- fhOffsets h St.!? (nx*y + x + 1)
         let len = off' - off
         return (fromIntegral off, fromIntegral len)
{-# INLINE getOffLen #-}

numBlocks :: GeoReference -> Options -> (Int, Int)
numBlocks ctx opts = (ceiling (nx/bx), ceiling (ny/by))
  where nx = fromIntegral . width  . shape     $ ctx  :: Double
        ny = fromIntegral . height . shape     $ ctx  :: Double
        bx = fromIntegral . fst    . blockSize $ opts :: Double
        by = fromIntegral . snd    . blockSize $ opts :: Double
{-# INLINE numBlocks #-}


writeBlock :: Binary (Block a) => Handle -> Codec -> Block a -> IO ()
writeBlock h codec = BS.hPut h . codec . encode
{-# SPECIALISE INLINE writeBlock :: Handle -> Codec -> Block Int16 -> IO () #-}

readBlock :: Binary (Block a) => Handle -> Codec -> Integer -> Int -> IO (Block a)
readBlock h codec off len
    = do hSeek h AbsoluteSeek off
         liftM (decode . codec) (BS.hGet h len)
{-# SPECIALISE INLINE readBlock ::
    Handle -> Codec -> Integer -> Int -> IO (Block Int16) #-}


withFileS
     :: (Proxy p)
     => FilePath
     -> IOMode
     -> (Handle -> b' -> ExceptionP p a' a b' b SafeIO r)
     -> b' -> ExceptionP p a' a b' b SafeIO r
withFileS pth mode p b'
  = bracket id (openFile pth mode) hClose (`p` b')



readerS :: (CheckP p, BlockData a, Binary (Block a))
  => Raster a
  -> BlockIx
  -> Server (ExceptionP p) BlockIx (Block a) SafeIO ()
readerS raster
  = withFileS (path raster) ReadMode (\h -> try . reader h)

{-# SPECIALISE readerS :: CheckP p =>
       Raster Int16 -> BlockIx -> 
       Server (ExceptionP p) BlockIx (Block Int16) SafeIO () #-}


writerS :: (CheckP p, BlockData a, Binary (Block a))
  => Raster a
  -> ()
  -> Client (ExceptionP p) BlockIx (Block a) SafeIO ()
writerS raster
  = withFileS (path raster) WriteMode (\h -> try . writer h raster)

{-# SPECIALISE writerS :: CheckP p =>
       Raster Int16 -> () -> 
       Client (ExceptionP p) BlockIx (Block Int16) SafeIO () #-}


-- | pixelGenerator is a pipe server that generates blocks with a function
--   (Pixel -> a)
pixelGenerator :: (Proxy p, Monad m, BlockData a)
  => (Pixel->a)
  -> Raster a
  -> BlockIx
  -> Server p BlockIx (Block a) m ()
pixelGenerator f raster = runIdentityK loop
  where
    (!nx,!ny)             = blockSize . options $ raster
    loop (BlockIx bx by) = respond block >>= loop
      where !block   = Block data_
            data_    = St.generate (nx*ny) genPx
            genPx i  = let (!x,!y) = i `divMod` nx
                       in f $ Pixel (x0+x) (y0+y)
            {-# INLINE [0] genPx #-}
            !x0      = nx * bx
            !y0      = ny * by
{-# SPECIALISE pixelGenerator :: (Proxy p, Monad m)
  => (Pixel->Int16)
  -> Raster Int16
  -> BlockIx
  -> Server p BlockIx (Block Int16) m () #-}

replicateGenerator :: (Proxy p, Monad m, BlockData a)
  => a
  -> Raster a
  -> BlockIx
  -> Server p BlockIx (Block a) m ()
replicateGenerator v raster = runIdentityK loop
  where
    (nx,ny)  = blockSize . options $ raster
    loop _   = respond block >>= loop
      where data_ = St.replicate (nx*ny) v
            block = Block data_


-- | pointGenerator is a pipe server that generates blocks with a function
--   (Point -> a)
pointGenerator :: (Proxy p, Monad m, BlockData a)
  => (Point->a)
  -> Raster a
  -> BlockIx
  -> Server p BlockIx (Block a) m ()
pointGenerator f raster = pixelGenerator f' raster
   where f' = f . backward (georef raster)


sink :: (Proxy p)
  => Raster a
  -> ()
  -> Client p BlockIx (Block a) IO ()
sink raster ()
    = runIdentityP $ mapM_ requestBlock ixs
  where nbs = numBlocks (georef raster) (options raster)
        ixs = blockIndexes nbs
        requestBlock i = request i >>= (\b -> force b `seq` return ())
    




runSession :: forall r a' b.
     (() -> EitherP SomeException ProxyFast a' () () b SafeIO r)
  -> IO r
runSession = runSafeIO . runProxy . runEitherK



type Codec = BS.ByteString -> BS.ByteString

compressor :: CompressionLevel -> Codec
compressor level
    = case level of
           0  -> id         
           l  -> compressWith defaultCompressParams {
                   compressLevel = compressionLevel l
                   -- , compressStrategy = huffmanOnlyStrategy
                   }

decompressor :: CompressionLevel -> Codec
decompressor level
    = case level of
           0  -> id         
           _  -> decompressWith defaultDecompressParams

