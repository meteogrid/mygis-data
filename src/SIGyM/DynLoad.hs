{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module SIGyM.DynLoad (
    EvalEnv (..)
  , EitherSymbol
  , defaultEnv
  , loadSymbolFromModule
  , loadSymbolFromBuffer
) where

import           Blaze.ByteString.Builder ( Builder, toByteString
                                          , fromByteString)
import           Control.Exception (Handler(Handler), catches)
import           Data.ByteString as BS (ByteString, length)
import           Data.ByteString.Internal (toForeignPtr)
import           Data.ByteString.Char8 (pack)
import           Data.Dynamic ( fromDynamic, Typeable, dynTypeRep )
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import           Data.Maybe (fromJust)
import           Data.Monoid (mempty, mappend)
import           Data.Time.Clock (UTCTime(UTCTime))
import           Data.Time.Calendar (Day(ModifiedJulianDay))

import           System.FilePath (joinPath)
import           System.IO (withFile, hFlush, IOMode(..))
import           System.IO.Temp (withSystemTempDirectory)

import           GHC hiding (importPaths)
import qualified GHC.Paths as P
import           ErrUtils
import           HscTypes
import           Outputable
import qualified DynFlags as DF
import           StringBuffer


type EitherSymbol = Either ByteString

data EvalEnv = EvalEnv {
    libdir     :: FilePath
  , importPaths :: [FilePath]
} deriving (Show)


defaultEnv :: EvalEnv
defaultEnv = EvalEnv { libdir = P.libdir, importPaths = ["."] }

loadSymbolFromModule ::
    Typeable a =>
    EvalEnv ->
    String ->
    String ->
    IO (EitherSymbol a)
loadSymbolFromModule = loadSymbol Nothing

loadSymbolFromBuffer ::
    Typeable a =>
    EvalEnv ->
    String ->
    String ->
    ByteString ->
    IO (EitherSymbol a)
loadSymbolFromBuffer env modname symbol code =
  withSystemTempDirectory "sigym.dynload." $ \tmpDir -> do
    -- create dummy file since GHC will try to open it even though we pass
    -- a buffer
    withFile (joinPath [tmpDir, modname ++ ".hs"]) WriteMode hFlush
    let env' = env { importPaths = [tmpDir] ++ importPaths env}
    loadSymbol (Just code) env' modname symbol



loadSymbol ::
    Typeable a =>
    Maybe ByteString ->
    EvalEnv ->
    String ->
    String ->
    IO (EitherSymbol a)

loadSymbol code env modname symbol = do
  logRef <- newIORef mempty :: (IO (IORef Builder))
  let compileAndLoad = do
        dflags <- getSessionDynFlags
        let dflags' = dflags {
                mainFunIs = Nothing
              , safeHaskell = Sf_Safe
              , ghcLink = LinkInMemory
              , outputHi = Nothing
              , outputFile = Nothing
              , DF.importPaths = importPaths env
              , log_action = logHandler logRef
              -- , packageFlags = [ DF.TrustPackage "sigym-core"
              --               , DF.TrustPackage "vector"
              --               ]
              , verbosity  = 3
              }
        _ <- setSessionDynFlags (DF.updOptLevel 2 dflags')
        defaultCleanupHandler dflags' $ do
          target <- getTarget modname code
          setTargets [target]
          _ <- load LoadAllTargets
          import_modules [modname]
          obj <- dynCompileExpr (modname ++ "." ++ symbol)
          return (case fromDynamic obj of
                    Just a  -> Right a
                    Nothing -> Left . pack $
                                 "Wrong type '" ++ show (dynTypeRep obj) ++ "'"
                 )

      handler e = do
        msg <- readIORef logRef
        let bsMsg = toByteString msg
        return $ Left (if BS.length bsMsg > 0 then bsMsg else pack e)

  (runGhc (Just (libdir env)) compileAndLoad) `catches` [
        Handler (\(e :: SourceError) -> handler (show e))
      , Handler (\(e :: GhcApiError) -> handler (show e))
    ]

getTarget :: Monad m => String -> Maybe ByteString -> m Target
getTarget modname code = return target
  where target  =
          Target { targetId           = TargetModule $ mkModuleName modname
                 , targetAllowObjCode = True
                 , targetContents     = contents }
        sBuff      = StringBuffer p l o
        (p,o,l)    = toForeignPtr (fromJust code)
        contents   = case code of
                       Nothing -> Nothing
                       Just _  -> Just (sBuff, dummyTime)
        dummyTime  = UTCTime (ModifiedJulianDay 0) (fromRational 0)



import_modules:: [String] -> Ghc ()
import_modules mods =
    GHC.setContext . (map (GHC.IIDecl . import_)) $ ("Data.Dynamic" : mods)
    where 
        import_ name =
            (GHC.simpleImportDecl . GHC.mkModuleName $ name)
            {GHC.ideclQualified=False}

-- from http://parenz.wordpress.com/2013/07/23/on-custom-error-handlers-for-ghc-api/
logHandler :: IORef Builder -> DF.LogAction
logHandler ref dflags severity srcSpan style msg =
  case severity of
     SevError ->  modifyIORef' ref (mappend printDoc)
     SevFatal ->  modifyIORef' ref (mappend printDoc)
     _        ->  return () -- ignore the rest
  where cntx = initSDocContext dflags style
        locMsg = mkLocMessage severity srcSpan msg
        printDoc = fromByteString . pack . show $ runSDoc locMsg cntx
