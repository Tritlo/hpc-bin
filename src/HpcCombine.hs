{-# LANGUAGE CPP #-}
---------------------------------------------------------
-- The main program for the hpc-add tool, part of HPC.
-- Andy Gill, Oct 2006
---------------------------------------------------------

module HpcCombine (sumPlugin, combinePlugin, mapPlugin) where

import Control.DeepSeq
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import HpcFlags
import Trace.Hpc.Tix

------------------------------------------------------------------------------

sumOptions :: FlagOptSeq
sumOptions =
  excludeOpt
    . includeOpt
    . outputOpt
    . unionModuleOpt
    . verbosityOpt

sumPlugin :: Plugin
sumPlugin =
  Plugin
    { name = "sum",
      usage = "[OPTION] .. <TIX_FILE> [<TIX_FILE> [<TIX_FILE> ..]]",
      options = sumOptions,
      summary = "Sum multiple .tix files in a single .tix file",
      implementation = sumMain,
      init_flags = defaultFlags,
      final_flags = defaultFinalFlags
    }

combineOptions :: FlagOptSeq
combineOptions =
  excludeOpt
    . includeOpt
    . outputOpt
    . combineFunOpt
    . combineFunOptInfo
    . unionModuleOpt
    . verbosityOpt

combinePlugin :: Plugin
combinePlugin =
  Plugin
    { name = "combine",
      usage = "[OPTION] .. <TIX_FILE> <TIX_FILE>",
      options = combineOptions,
      summary = "Combine two .tix files in a single .tix file",
      implementation = combineMain,
      init_flags = defaultFlags,
      final_flags = defaultFinalFlags
    }

mapOptions :: FlagOptSeq
mapOptions =
  excludeOpt
    . includeOpt
    . outputOpt
    . mapFunOpt
    . mapFunOptInfo
    . unionModuleOpt
    . verbosityOpt

mapPlugin :: Plugin
mapPlugin =
  Plugin
    { name = "map",
      usage = "[OPTION] .. <TIX_FILE> ",
      options = mapOptions,
      summary = "Map a function over a single .tix file",
      implementation = mapMain,
      init_flags = defaultFlags,
      final_flags = defaultFinalFlags
    }

------------------------------------------------------------------------------

sumMain :: Flags -> [String] -> IO ()
sumMain _ [] = hpcError sumPlugin "no .tix file specified"
sumMain flags (first_file : more_files) = do
  Just tix <- readTix first_file

  tix' <- foldM (mergeTixFile flags (+)) (filterTix flags tix) more_files

  case outputFile flags of
    "-" -> print tix'
    out -> writeTix out tix'

combineMain :: Flags -> [String] -> IO ()
combineMain flags [first_file, second_file] = do
  let f = theCombineFun (combineFun flags)

  Just tix1 <- readTix first_file
  Just tix2 <- readTix second_file

  let tix = mergeTix (mergeModule flags) f (filterTix flags tix1) (filterTix flags tix2)

  case outputFile flags of
    "-" -> print tix
    out -> writeTix out tix
combineMain _ _ = hpcError combinePlugin "need exactly two .tix files to combine"

mapMain :: Flags -> [String] -> IO ()
mapMain flags [first_file] = do
  let f = thePostFun (postFun flags)

  Just tix <- readTix first_file

  let (Tix inside_tix) = filterTix flags tix
#if __GLASGOW_HASKELL__ >= 907
  let tix' = Tix [ TixModule m p i (map f t) n tr r
                 | TixModule m p i t n tr r <- inside_tix]
#else
  let tix' = Tix [ TixModule m p i (map f t)
                 | TixModule m p i t <- inside_tix]
#endif

  case outputFile flags of
    "-" -> print tix'
    out -> writeTix out tix'
mapMain _ [] = hpcError mapPlugin "no .tix file specified"
mapMain _ _ = hpcError mapPlugin "to many .tix files specified"

mergeTixFile :: Flags -> (Integer -> Integer -> Integer) -> Tix -> String -> IO Tix
mergeTixFile flags fn tix file_name = do
  Just new_tix <- readTix file_name
  return $! force $ mergeTix (mergeModule flags) fn tix (filterTix flags new_tix)

-- could allow different numbering on the module info,
-- as long as the total is the same; will require normalization.

mergeTix :: MergeFun -> (Integer -> Integer -> Integer) -> Tix -> Tix -> Tix
mergeTix modComb f (Tix t1) (Tix t2) =
  Tix
    [ case (Map.lookup m fm1, Map.lookup m fm2) of
        -- todo, revisit the semantics of this combination
#if __GLASGOW_HASKELL__ >= 907
        (Just (TixModule _ hash1 len1 tix1 n1 t1 r1), Just (TixModule _ hash2 len2 tix2 n2 t2 r2))
#else
        (Just (TixModule _ hash1 len1 tix1), Just (TixModule _ hash2 len2 tix2))
#endif
          | hash1 /= hash2
              || length tix1 /= length tix2
              || len1 /= length tix1
              || len2 /= length tix2 ->
              error $ "mismatched in module " ++ m
          | otherwise ->
#if __GLASGOW_HASKELL__ >= 907
              TixModule m hash1 len1 (zipWith f tix1 tix2) n2 t2 r2
#else
              TixModule m hash1 len1 (zipWith f tix1 tix2)
#endif
        (Just m1, Nothing) ->
          m1
        (Nothing, Just m2) ->
          m2
        _ -> error "impossible"
      | m <- Set.toList (theMergeFun modComb m1s m2s)
    ]
  where
    m1s = Set.fromList $ map tixModuleName t1
    m2s = Set.fromList $ map tixModuleName t2

    fm1 = Map.fromList [(tixModuleName tix, tix) | tix <- t1]
    fm2 = Map.fromList [(tixModuleName tix, tix) | tix <- t2]
