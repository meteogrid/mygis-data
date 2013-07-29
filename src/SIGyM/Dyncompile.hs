{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module SIGyM.Dyncompile (
    EvalEnv (..)
  , EitherSymbol
  , defaultEnv
  , evalWithModules
  , eval
  , loadSymbol
) where

import GHC
import qualified GHC.Paths as P
import DynFlags
import Control.Exception
import Control.Monad ( liftM )
import Data.Dynamic ( fromDynamic, Typeable )

type EitherSymbol = Either String

data EvalEnv = EvalEnv {
    libdir     :: FilePath
  , importPaths :: [FilePath]
} deriving (Show)

defaultEnv :: EvalEnv
defaultEnv = EvalEnv { libdir = P.libdir, importPaths = ["."] }

evalWithModules ::
    Typeable a =>
    EvalEnv ->
    [String] ->
    String ->
    IO (EitherSymbol a)

evalWithModules env modules expr = maybeRunGhc env $ do
    _ <- setFlags env
    import_modules modules
    maybeCompile expr


eval :: Typeable a => EvalEnv -> String -> IO (EitherSymbol a)
eval env = evalWithModules env ["Prelude"]

loadSymbol ::
    Typeable a =>
    EvalEnv ->
    String ->
    String ->
    IO (EitherSymbol a)

loadSymbol env modname symbol = maybeRunGhc env $ do
    _ <- setFlags env
    target <- guessTarget modname Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    import_modules [modname]
    maybeCompile (modname ++ "." ++ symbol)




maybeRunGhc :: EvalEnv -> Ghc (Either String b) -> IO (Either String b)
maybeRunGhc env f =
    (runGhc (Just (libdir env)) f) `catches` [
        Handler (\(ex :: SomeException) -> return (Left (show ex)))
    ]

setFlags :: GhcMonad m => EvalEnv -> m [PackageId]
setFlags env =  do
    dflags <- getSessionDynFlags
    let dflags' = dflags {
            mainFunIs = Nothing
          , ghcLink = LinkInMemory
          , outputHi = Nothing
          , outputFile = Nothing
          , DynFlags.importPaths = SIGyM.Dyncompile.importPaths env
          }
    setSessionDynFlags (updOptLevel 2 dflags')

import_modules:: [String] -> Ghc ()
import_modules mods =
    GHC.setContext . (map (GHC.IIDecl . import_)) $ "Data.Dynamic":mods
    where 
        import_ name =
            (GHC.simpleImportDecl . GHC.mkModuleName $ name)
            {GHC.ideclQualified=False}

maybeCompile:: Typeable a => String -> Ghc (EitherSymbol a)
maybeCompile expr = 
    liftM (maybe_to_maybesym . fromDynamic) (GHC.dynCompileExpr expr)
    where
        maybe_to_maybesym (Just a) = Right a
        maybe_to_maybesym Nothing = Left "Wrong type"
