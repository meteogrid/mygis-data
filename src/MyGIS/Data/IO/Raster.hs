{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

module MyGIS.Data.IO.Raster
(
    Raster (..)
  , Options (..)

  , pixelGenerator
  , pointGenerator
  , readerS
  , writerS
  , runSession

  , (>->)
  , try
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (forM_, liftM)
import           Control.Proxy (Proxy(..), Client, Server, runIdentityK,
                                runIdentityP, lift, runProxy, (>->))
import           Control.Proxy.Safe (ExceptionP, CheckP, SafeIO, bracket, try,
                                     runSafeIO, runEitherK)

import           Foreign.Storable (Storable(..))
import           Foreign (castPtr)

import           Data.Binary (Binary(..), decode, encode)
import           Data.Binary.Put (putWord16host)
import           Data.Binary.Get (getWord16host, Get)
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.Int (Int64, Int16, Int32)
import qualified Data.Vector.Storable as St
import qualified Data.ByteString.Lazy as BS
import           Data.Vector.Binary()

import           System.IO

import           MyGIS.Data.Context (Context, Pixel(..), Point, shape, width,
                                     height, backward)

type BlockOffset = Int64
type BlockLen = Int64
type DataType = Int16
data BlockRef = BlockRef !BlockOffset !BlockLen

data Raster = Raster {
    options :: Options
  , context :: Context
  , path    :: FilePath
}

data FileHeader = FileHeader Options Context (St.Vector BlockRef)

data Options = Opts {
    compression :: Maybe Int
  , blockSize   :: (Int,Int)
} deriving (Eq, Show)


data Block = Block !(St.Vector DataType)

newtype BlockIx = BlockIx (Int,Int) deriving (Eq,Show)

numBlocks :: Context -> Options -> (Int, Int)
numBlocks ctx opts = (ceiling (nx/bx), ceiling (ny/by))
  where nx = fromIntegral . width  . shape     $ ctx  :: Double
        ny = fromIntegral . height . shape     $ ctx  :: Double
        bx = fromIntegral . fst    . blockSize $ opts :: Double
        by = fromIntegral . snd    . blockSize $ opts :: Double



instance Binary Options where
  {-# INLINE put #-}
  put (Opts c (bx,by))
    = put (enc c) >> put bx >> put by
    where enc (Just a) = a
          enc Nothing  = -1
  {-# INLINE get #-}
  get
    = do c <- get
         let comp = case c of
                      (-1) -> Nothing
                      v    -> Just v
         bx <- get
         by <- get
         return (Opts comp (bx,by))

instance Binary BlockRef where
  {-# INLINE put #-}
  put (BlockRef a b) = put a >> put b
  {-# INLINE get #-}
  get                = BlockRef <$> get <*> get

    
instance Binary FileHeader where
  {-# INLINE put #-}
  put (FileHeader a b c) = put a >> put b >> put c
  {-# INLINE get #-}
  get                    = FileHeader <$> get <*> get <*> get


{-
Portable entre maquinas con distinto endianness pero 15% mas lento

instance Binary Block where
  {-# INLINE put #-}
  put (Block a) = put a
  {-# INLINE get #-}
  get           = Block <$> get
-}

instance Binary Block where
  {-# INLINE put #-}
  put (Block a)
    = do let len = fromIntegral . St.length $ a :: Int32
         put len
         St.forM_ a (putWord16host . fromIntegral)
  {-# INLINE get #-}
  get
    = do len <- get :: Get Int32
         v <- St.replicateM (fromIntegral len) (liftM fromIntegral getWord16host)
         return $ Block v

instance Storable  BlockRef where
  sizeOf _    = sizeOf (undefined :: BlockOffset)
              + sizeOf (undefined :: BlockLen)

  alignment _ = alignment (undefined :: BlockLen)

  {-# INLINE peek #-}
  peek p = BlockRef <$> peek (castPtr p) <*> peekByteOff (castPtr p) s
    where s = sizeOf (undefined :: BlockOffset)

  {-# INLINE poke #-}
  poke p (BlockRef o l) = poke (castPtr p) o >> pokeByteOff (castPtr p) s l
    where s = sizeOf (undefined :: BlockOffset)



-- | reader is a pipe server that safely reads blocks from an in-file raster
reader :: (Proxy p)
  => Handle
  -> BlockIx
  -> Server p BlockIx Block IO ()

reader h = runIdentityK initialize
  where
    initialize ix = do
        lift $ hSetBinaryMode h True
        contents <- lift $ BS.hGetContents h
        let !fh = decode contents :: FileHeader
        loop fh ix
    loop fh ix = do
        let ref = getRef fh ix
        case ref of
            Just (BlockRef off len) -> do
                lift $ hSeek h AbsoluteSeek (fromIntegral off)
                !block <- lift $ liftM decode $ BS.hGet h (fromIntegral len)
                next <- respond block
                loop fh next
            Nothing -> error "corrupted file" -- TODO: throw catchable exception

{-# INLINE getRef #-}
getRef :: FileHeader -> BlockIx -> Maybe BlockRef
getRef (FileHeader opts ctx refs) (BlockIx (x,y))
    = let nx = fst $ numBlocks ctx opts
      in refs St.!? (nx*y + x)


withFileS
     :: (Proxy p)
     => FilePath
     -> IOMode
     -> (Handle -> b' -> ExceptionP p a' a b' b SafeIO r)
     -> b' -> ExceptionP p a' a b' b SafeIO r
withFileS pth mode p b'
  = bracket id (openFile pth mode) hClose (\h -> p h b')

readerS :: (CheckP p)
  => Raster
  -> BlockIx
  -> Server (ExceptionP p) BlockIx Block SafeIO ()
readerS raster
  = withFileS (path raster) ReadMode (\h -> try . (reader h))


blockIndexes :: (Int,Int) -> [BlockIx]
blockIndexes (bx,by) = [ BlockIx (i,j) | j <- [0..by-1], i <- [0..bx-1]]

writer :: (Proxy p)
  => Handle
  -> Raster
  -> ()
  -> Client p BlockIx Block IO ()
writer h raster () = runIdentityP $ do
    -- Create a dummy healdeocr with the correct number of refs to compute
    -- the first block's offset
    let header    = FileHeader (options raster) (context raster) dummyRefs
        dummyRefs = St.fromList $ replicate (length indexes) (BlockRef 0 0)
        fstBlkOff = fromIntegral . BS.length . encode $ header
        indexes   = blockIndexes $ numBlocks (context raster) (options raster)
    
    lift $ hSeek h AbsoluteSeek fstBlkOff
    blockRefs <- lift $ newIORef []
        
    -- Request all blocks from pipe and write them to file while updating
    -- mutable list of BlockRefs
    forM_ indexes $ \ix -> do
        !block <- request ix
        let !encoded = encode block
            len     = fromIntegral . BS.length $ encoded
        off <- lift $ liftM fromIntegral $ hTell h
        lift $ BS.hPut h encoded
        bs <- lift $ readIORef blockRefs
        lift $ writeIORef blockRefs ((BlockRef off len) : bs)

    blockRefs' <- lift $ liftM (St.fromList . reverse) $ readIORef blockRefs
    -- Create final header and write it at the beginning of the file
    let finalHeader = FileHeader (options raster) (context raster) blockRefs'
    lift $ hSeek h AbsoluteSeek 0
    lift $ BS.hPut h (encode finalHeader)



writerS :: (CheckP p)
  => Raster
  -> ()
  -> Client (ExceptionP p) BlockIx Block SafeIO ()
writerS raster
  = withFileS (path raster) WriteMode (\h -> try . (writer h raster))

-- | pixelGenerator is a pipe server that generates blocks with a function
--   (Pixel -> DataType)
pixelGenerator :: (Proxy p, Monad m)
  => (Pixel->DataType)
  -> Raster
  -> BlockIx
  -> Server p BlockIx Block m ()

pixelGenerator f raster = runIdentityK loop
  where
    loop (BlockIx (bx,by)) = do
        let (nx,ny) = blockSize . options $ raster
            x0      = nx * bx
            y0      = ny * by
            !block   = Block $ St.generate (nx*ny) $ genPx
            genPx i = let (x,y) = i `divMod` nx
                          px    = Pixel (x0+x) (y0+y)
                      in f px
        next <- respond block
        loop next


-- | pointGenerator is a pipe server that generates blocks with a function
--   (Point -> DataType)
pointGenerator :: (Proxy p, Monad m)
  => (Point->DataType)
  -> Raster
  -> BlockIx
  -> Server p BlockIx Block m ()
pointGenerator f raster = pixelGenerator f' raster
   where f' = f . (backward (context raster))

runSession = runSafeIO . runProxy . runEitherK
