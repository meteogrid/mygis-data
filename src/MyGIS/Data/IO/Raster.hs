{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

module MyGIS.Data.IO.Raster
{-
(
    Raster (..)
  , Options (..)

  , generatePixel
  , generatePoint

)
-}
where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (forM_, liftM)
import           Control.Proxy (Proxy(..), Client, Server, runIdentityK,
                                runIdentityP, lift)
import           Control.Proxy.Safe (ExceptionP, CheckP, SafeIO, bracket, try)

import           Data.Binary (Binary(..), decode, encode)
import           Data.Binary.Put (putWord16host)
import           Data.Binary.Get (getWord16host)
import           Data.Int (Int64, Int16)
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import qualified Data.ByteString.Lazy as BS
import           Data.Vector.Binary()

import           System.IO

import           MyGIS.Data.Context (Context, Pixel(..), Point, shape, width,
                                     height, backward)

type BlockOffset = Int64
type DataType = Int16

data Raster = Raster {
    options :: Options
  , context :: Context
  , path    :: FilePath
}

data FileHeader = FileHeader Options Context (St.Vector BlockOffset)

data Options = Opts {
    compression :: Maybe Int
  , blockSize   :: (Int,Int)
} deriving (Eq, Show)


data Block = Block !(St.Vector DataType)

data BlockIx = BlockIx !Int !Int deriving (Eq,Show)

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

    
instance Binary FileHeader where
  {-# INLINE put #-}
  put (FileHeader a b c) = put a >> put b >> put c
  {-# INLINE get #-}
  get                    = FileHeader <$> get <*> get <*> get


instance Binary Block where
  {-# INLINE put #-}
  put (Block a)
    = do let len = fromIntegral $ St.length a
         putWord16host len
         St.forM_ a (putWord16host . fromIntegral)
  {-# INLINE get #-}
  get
    = do len <- getWord16host
         v <- St.replicateM (fromIntegral len) (liftM fromIntegral getWord16host)
         return $ Block v



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
            Just off -> do
                lift $ hSeek h AbsoluteSeek (fromIntegral off)
                !block <- lift $ liftM decode $ BS.hGetContents h
                next <- respond block
                loop fh next
            Nothing -> error "corrupted file" -- TODO: throw catchable exception

{-# INLINE getRef #-}
getRef :: FileHeader -> BlockIx -> Maybe BlockOffset
getRef (FileHeader opts ctx refs) (BlockIx x y)
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
blockIndexes (bx,by) = [ BlockIx i j | j <- [0..by-1], i <- [0..bx-1]]

writer :: (Proxy p)
  => Handle
  -> Raster
  -> ()
  -> Client p BlockIx Block IO ()
writer h raster () = runIdentityP $ do
    -- Create a dummy healdeocr with the correct number of refs to compute
    -- the first block's offset.
    -- FIXME: Calculate offset without encoding a dummy header
    let header    = FileHeader (options raster) (context raster) dummyRefs
        dummyRefs = St.replicate (nx*ny) 0
        fstBlkOff = fromIntegral . BS.length . encode $ header
        (nx,ny)   = numBlocks (context raster) (options raster)
        indexes   = blockIndexes (nx,ny)
    
    lift $ hSeek h AbsoluteSeek fstBlkOff
    blockRefs <- lift $ Stm.new (nx*ny)
        
    -- Request all blocks from pipe and write them to file while updating
    -- mutable list of BlockOffsets
    forM_ (zip [0..] indexes) $ \(i, !ix) -> do
        !block <- request ix
        off <- lift $ liftM fromIntegral $ hTell h
        lift $ BS.hPut h $ encode block
        lift $ Stm.unsafeWrite blockRefs i off

    blockRefs' <- lift $ St.unsafeFreeze blockRefs
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
    loop (BlockIx bx by) = do
        let (nx,ny) = blockSize . options $ raster
            x0      = nx * bx
            y0      = ny * by
            !data_   = St.generate (nx*ny) $ genPx
            !block   = Block data_
            genPx i = let (!x,!y) = i `divMod` nx
                          !px    = Pixel (x0+x) (y0+y)
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
