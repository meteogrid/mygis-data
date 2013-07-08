{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

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
import           Data.Int
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import qualified Data.ByteString.Lazy as BS
import           Data.Vector.Binary()
import           System.IO

import           MyGIS.Data.Context

data Raster a = Raster {
    options :: Options
  , context :: Context
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
  , fhContext :: !Context
  , fhOffsets :: !(St.Vector BlockOffset)
} deriving Show

instance NFData FileHeader where
instance Binary FileHeader where
  {-# INLINE put #-}
  put fh
    =  put (vMajor $ fhVersion fh)
    >> put (vMinor $ fhVersion fh)
    >> put (fhOptions fh)
    >> put (fhContext fh)
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

fileHeader10 :: Options -> Context -> St.Vector BlockOffset -> FileHeader
fileHeader10 = FileHeader version10


data Block a = Block !(St.Vector a)
instance NFData (Block a) where

class (Binary a, Stm.Storable a) => BlockData a where

instance BlockData Int8
instance BlockData Int16
instance BlockData Int32
instance BlockData Int

instance BlockData a => Binary (Block a) where
  {-# SPECIALISE INLINE put :: Block Int16 -> Put #-}
  put (Block a) = put a
  {-# SPECIALISE INLINE get :: Get (Block Int16) #-}
  get           = liftA Block get


data BlockIx = BlockIx !Int !Int deriving (Eq,Show)
instance NFData BlockIx where





    


-- | reader is a pipe server that reads blocks from a *seekable* file handle
reader :: (Proxy p, BlockData a)
  => Handle
  -> BlockIx
  -> Server p BlockIx (Block a) IO ()
reader h = runIdentityK initialize
  where
    initialize ix = do
        lift $ hSetBinaryMode h True
        contents <- lift $ BS.hGetContents h
        loop (force $ decode contents) ix
    loop !fh ix = do
        let ref = getOffLen fh ix
            decompress = decompressor $ compression $ fhOptions fh
        case ref of
             Just (off,len) -> do
                lift $ hSeek h AbsoluteSeek off
                !block <- lift $
                    liftM (force . decode . decompress) $ BS.hGet h len
                next <- respond block
                loop fh next
             Nothing -> error "corrupted file" --TODO: throw catchable exception
{-# SPECIALISE INLINE reader :: (Proxy p) =>
       Handle -> BlockIx -> Server p BlockIx (Block Int16) IO () #-}


-- | writer is a pipe client that writes blocks to a file handle
writer :: (Proxy p, BlockData a)
  => Handle
  -> Raster a
  -> ()
  -> Client p BlockIx (Block a) IO ()
writer h raster () = runIdentityP $ do
    -- Create a dummy header with the correct number of offsets to compute
    -- the first block's offset.
    -- FIXME: Calculate first block offset without encoding a dummy header
    let header    = fileHeader10 (options raster) (context raster) dummyRefs
        dummyRefs = St.replicate nRefs 0
        nRefs     = nx * ny + 1
        fstBlkOff = fromIntegral . BS.length . encode $ header
        (nx,ny)   = numBlocks (context raster) (options raster)
        compress = compressor (compression $ options raster)
    
    -- Request all blocks from pipe and write them to file while updating
    -- mutable list of BlockOffsets
    blockRefs <- lift $ Stm.new nRefs
    lift $ hSeek h AbsoluteSeek fstBlkOff
    forM_  (zip [0..] (blockIndexes (nx,ny))) $ \(!i, !ix) -> do
        !block <- request ix
        off <- lift $ liftM fromIntegral $ hTell h
        lift $ BS.hPut h $ compress $ encode block
        lift $ Stm.unsafeWrite blockRefs i off

    off <- lift $ liftM fromIntegral $ hTell h
    lift $ Stm.unsafeWrite blockRefs (nRefs-1) off
    blockRefs' <- lift $ St.unsafeFreeze blockRefs

    -- Create final header and write it at the beginning of the file
    let finalHeader = fileHeader10 (options raster) (context raster) blockRefs'
    lift $ hSeek h AbsoluteSeek 0
    lift $ BS.hPut h (encode finalHeader)

{-# SPECIALISE INLINE writer :: (Proxy p) =>
       Handle -> Raster Int16 -> () -> Client p BlockIx (Block Int16) IO () #-}


{-# INLINE blockIndexes #-}
blockIndexes :: (Int,Int) -> [BlockIx]
blockIndexes (bx,by) = [ BlockIx i j | j <- [0..by-1], i <- [0..bx-1]]


{-# INLINE getOffLen #-}
getOffLen :: FileHeader -> BlockIx -> Maybe (Integer, Int)
getOffLen h (BlockIx x y)
    = do let nx = fst $ numBlocks (fhContext h) (fhOptions h)
         off  <- fhOffsets h St.!? (nx*y + x)
         off' <- fhOffsets h St.!? (nx*y + x + 1)
         let len = off' - off
         return (fromIntegral off, fromIntegral len)

{-# INLINE numBlocks #-}
numBlocks :: Context -> Options -> (Int, Int)
numBlocks ctx opts = (ceiling (nx/bx), ceiling (ny/by))
  where nx = fromIntegral . width  . shape     $ ctx  :: Double
        ny = fromIntegral . height . shape     $ ctx  :: Double
        bx = fromIntegral . fst    . blockSize $ opts :: Double
        by = fromIntegral . snd    . blockSize $ opts :: Double



withFileS
     :: (Proxy p)
     => FilePath
     -> IOMode
     -> (Handle -> b' -> ExceptionP p a' a b' b SafeIO r)
     -> b' -> ExceptionP p a' a b' b SafeIO r
withFileS pth mode p b'
  = bracket id (openFile pth mode) hClose (`p` b')



readerS :: (CheckP p, BlockData a)
  => Raster a
  -> BlockIx
  -> Server (ExceptionP p) BlockIx (Block a) SafeIO ()
readerS raster
  = withFileS (path raster) ReadMode (\h -> try . reader h)

{-# SPECIALISE readerS :: CheckP p =>
       Raster Int16 -> BlockIx -> 
       Server (ExceptionP p) BlockIx (Block Int16) SafeIO () #-}


writerS :: (CheckP p, BlockData a)
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
    loop (BlockIx bx by) = do
        let (nx,ny)  = blockSize . options $ raster
            x0       = nx * bx
            y0       = ny * by
            !data_   = St.generate (nx*ny) genPx
            !block   = Block data_
            genPx i = let (!x,!y) = i `divMod` nx
                          !px    = Pixel (x0+x) (y0+y)
                      in f px
        next <- respond block
        loop next

replicateGenerator :: (Proxy p, Monad m, BlockData a)
  => a
  -> Raster a
  -> BlockIx
  -> Server p BlockIx (Block a) m ()
replicateGenerator v raster = runIdentityK loop
  where
    loop _ = do
        let (nx,ny)  = blockSize . options $ raster
            !data_   = St.replicate (nx*ny) v
            !block   = Block data_
        next <- respond block
        loop next


-- | pointGenerator is a pipe server that generates blocks with a function
--   (Point -> a)
pointGenerator :: (Proxy p, Monad m, BlockData a)
  => (Point->a)
  -> Raster a
  -> BlockIx
  -> Server p BlockIx (Block a) m ()
pointGenerator f raster = pixelGenerator f' raster
   where f' = f . backward (context raster)


sink :: (Proxy p)
  => Raster a
  -> ()
  -> Client p BlockIx (Block a) IO ()
sink raster () = runIdentityP $ forM_ (blockIndexes nbs) request
  where nbs = numBlocks (context raster) (options raster)
    




runSession :: forall r a' b.
     (() -> EitherP SomeException ProxyFast a' () () b SafeIO r)
  -> IO r
runSession = runSafeIO . runProxy . runEitherK




compressor :: CompressionLevel -> BS.ByteString -> BS.ByteString
compressor level
    = case level of
           0  -> id         
           l  -> compressWith defaultCompressParams {
                   compressLevel = compressionLevel l
                   -- , compressStrategy = huffmanOnlyStrategy
                   }

decompressor :: CompressionLevel -> BS.ByteString -> BS.ByteString
decompressor level
    = case level of
           0  -> id         
           _  -> decompressWith defaultDecompressParams

