{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module SIGyM.DynLoad (
    EvalEnv (..)
  , EitherSymbol
  , defaultEnv
  , loadSymbol
) where

import           Blaze.ByteString.Builder ( Builder, toByteString
                                          , fromByteString)
import           Control.Exception (Handler(Handler), catches)
import           Data.Dynamic ( fromDynamic, Typeable, dynTypeRep )
import           Data.ByteString as BS (ByteString, length)
import           Data.ByteString.Char8 (pack)
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import           Data.Monoid (mempty, mappend)

import           GHC
import qualified GHC.Paths as P
import           ErrUtils
import           HscTypes
import           Outputable
import           DynFlags


type EitherSymbol = Either ByteString

data EvalEnv = EvalEnv {
    libdir     :: FilePath
  , importPaths :: [FilePath]
} deriving (Show)


defaultEnv :: EvalEnv
defaultEnv = EvalEnv { libdir = P.libdir, importPaths = ["."] }


loadSymbol ::
    Typeable a =>
    EvalEnv ->
    String ->
    String ->
    IO (EitherSymbol a)

loadSymbol env modname symbol = do
  logRef <- newIORef mempty :: (IO (IORef Builder))
  let compileAndLoad = do
        dflags <- getSessionDynFlags
        let dflags' = dflags {
                mainFunIs = Nothing
              , safeHaskell = Sf_Safe
              , ghcLink = LinkInMemory
              , outputHi = Nothing
              , outputFile = Nothing
              , DynFlags.importPaths = SIGyM.DynLoad.importPaths env
              , log_action = logHandler logRef
              }
        _ <- setSessionDynFlags (updOptLevel 2 dflags')
        defaultCleanupHandler dflags' $ do
          target <- guessTarget modname Nothing
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
        return $ Left (if BS.length bsMsg > 0 then bsMsg else (pack e))

  (runGhc (Just (libdir env)) compileAndLoad) `catches` [
        Handler (\(e :: SourceError) -> handler (show e))
      , Handler (\(e :: GhcApiError) -> handler (show e))
    ]


import_modules:: [String] -> Ghc ()
import_modules mods =
    GHC.setContext . (map (GHC.IIDecl . import_)) $ ("Data.Dynamic" : mods)
    where 
        import_ name =
            (GHC.simpleImportDecl . GHC.mkModuleName $ name)
            {GHC.ideclQualified=False}

-- from http://parenz.wordpress.com/2013/07/23/on-custom-error-handlers-for-ghc-api/
logHandler :: IORef Builder -> LogAction
logHandler ref dflags severity srcSpan style msg =
  case severity of
     SevError ->  modifyIORef' ref (mappend printDoc)
     SevFatal ->  modifyIORef' ref (mappend printDoc)
     _        ->  return () -- ignore the rest
  where cntx = initSDocContext dflags style
        locMsg = mkLocMessage severity srcSpan msg
        printDoc = fromByteString . pack . show $ runSDoc locMsg cntx
