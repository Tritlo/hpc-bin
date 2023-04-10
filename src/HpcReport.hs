{-# LANGUAGE CPP #-}
---------------------------------------------------------
-- The main program for the hpc-report tool, part of HPC.
-- Colin Runciman and Andy Gill, June 2006
---------------------------------------------------------

module HpcReport (reportPlugin) where

import Control.Monad hiding (guard)
import Data.Function
import Data.List (sort, sortBy, intercalate)
import qualified Data.Set as Set
import HpcFlags
import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Prelude hiding (exp)

------------------------------------------------------------------------------

notExpecting :: String -> a
notExpecting s = error ("not expecting " ++ s)

data BoxTixCounts = BT {boxCount, tixCount :: !Int}

instance Semigroup BoxTixCounts where
  bt1 <> bt2 =
    BT
      { boxCount = ((+) `on` boxCount) bt1 bt2,
        tixCount = ((+) `on` tixCount) bt1 bt2
      }

instance Monoid BoxTixCounts where
  mempty = BT {boxCount = 0, tixCount = 0}

btPercentage :: String -> BoxTixCounts -> String
btPercentage s (BT b t) = showPercentage s t b

showPercentage :: String -> Int -> Int -> String
showPercentage s 0 0 = "100% " ++ s ++ " (0/0)"
showPercentage s n d =
  showWidth 3 p
    ++ "% "
    ++ s
    ++ " ("
    ++ show n
    ++ "/"
    ++ show d
    ++ ")"
  where
    p = (n * 100) `div` d
    showWidth w x0 = replicate (shortOf w (length sx)) ' ' ++ sx
      where
        sx = show x0
        shortOf x y = if y < x then x - y else 0

data BinBoxTixCounts = BBT
  { binBoxCount :: !Int,
    onlyTrueTixCount :: !Int,
    onlyFalseTixCount :: !Int,
    bothTixCount :: !Int
  }

instance Semigroup BinBoxTixCounts where
  bbt1 <> bbt2 =
    BBT
      { binBoxCount = ((+) `on` binBoxCount) bbt1 bbt2,
        onlyTrueTixCount = ((+) `on` onlyTrueTixCount) bbt1 bbt2,
        onlyFalseTixCount = ((+) `on` onlyFalseTixCount) bbt1 bbt2,
        bothTixCount = ((+) `on` bothTixCount) bbt1 bbt2
      }

instance Monoid BinBoxTixCounts where
  mempty =
    BBT
      { binBoxCount = 0,
        onlyTrueTixCount = 0,
        onlyFalseTixCount = 0,
        bothTixCount = 0
      }

bbtPercentage :: String -> Bool -> BinBoxTixCounts -> String
bbtPercentage s withdetail (BBT b tt ft bt) =
  showPercentage s bt b
    ++ if withdetail && bt /= b
      then
        detailFor tt "always True"
          ++ detailFor ft "always False"
          ++ detailFor (b - (tt + ft + bt)) "unevaluated"
      else ""
  where
    detailFor n txt =
      if n > 0
        then ", " ++ show n ++ " " ++ txt
        else ""

data ModInfo = MI
  { exp, alt, top, loc :: !BoxTixCounts,
    guard, cond, qual :: !BinBoxTixCounts,
    decPaths :: [[String]]
  }

instance Semigroup ModInfo where
  mi1 <> mi2 =
    MI
      { exp = ((<>) `on` exp) mi1 mi2,
        alt = ((<>) `on` alt) mi1 mi2,
        top = ((<>) `on` top) mi1 mi2,
        loc = ((<>) `on` loc) mi1 mi2,
        guard = ((<>) `on` guard) mi1 mi2,
        cond = ((<>) `on` cond) mi1 mi2,
        qual = ((<>) `on` qual) mi1 mi2,
        decPaths = ((<>) `on` decPaths) mi1 mi2
      }

instance Monoid ModInfo where
  mempty =
    MI
      { exp = mempty,
        alt = mempty,
        top = mempty,
        loc = mempty,
        guard = mempty,
        cond = mempty,
        qual = mempty,
        decPaths = mempty
      }

allBinCounts :: ModInfo -> BinBoxTixCounts
allBinCounts mi =
  BBT
    { binBoxCount = sumAll binBoxCount,
      onlyTrueTixCount = sumAll onlyTrueTixCount,
      onlyFalseTixCount = sumAll onlyFalseTixCount,
      bothTixCount = sumAll bothTixCount
    }
  where
    sumAll f = f (guard mi) + f (cond mi) + f (qual mi)

accumCounts :: [(BoxLabel, Integer)] -> ModInfo -> ModInfo
accumCounts [] mi = mi
accumCounts ((bl, btc) : etc) mi
  | single bl = accumCounts etc mi'
  where
    mi' = case bl of
      ExpBox False -> mi {exp = inc (exp mi)}
      ExpBox True -> mi {exp = inc (exp mi), alt = inc (alt mi)}
      TopLevelBox dp ->
        mi
          { top = inc (top mi),
            decPaths = upd dp (decPaths mi)
          }
      LocalBox dp ->
        mi
          { loc = inc (loc mi),
            decPaths = upd dp (decPaths mi)
          }
      _other -> notExpecting "BoxLabel in accumcounts"
    inc (BT {boxCount = bc, tixCount = tc}) =
      BT
        { boxCount = bc + 1,
          tixCount = tc + bit (btc > 0)
        }
    upd dp dps =
      if btc > 0 then dps else dp : dps
accumCounts [_] _ = error "accumCounts: Unhandled case: [_] _"
accumCounts ((bl0, btc0) : (bl1, btc1) : etc) mi =
  accumCounts etc mi'
  where
    mi' = case (bl0, bl1) of
      (BinBox GuardBinBox True, BinBox GuardBinBox False) ->
        mi {guard = inc (guard mi)}
      (BinBox CondBinBox True, BinBox CondBinBox False) ->
        mi {cond = inc (cond mi)}
      (BinBox QualBinBox True, BinBox QualBinBox False) ->
        mi {qual = inc (qual mi)}
      _other -> notExpecting "BoxLabel pair in accumcounts"
    inc
      ( BBT
          { binBoxCount = bbc,
            onlyTrueTixCount = ttc,
            onlyFalseTixCount = ftc,
            bothTixCount = btc
          }
        ) =
        BBT
          { binBoxCount = bbc + 1,
            onlyTrueTixCount = ttc + bit (btc0 > 0 && btc1 == 0),
            onlyFalseTixCount = ftc + bit (btc0 == 0 && btc1 > 0),
            bothTixCount = btc + bit (btc0 > 0 && btc1 > 0)
          }

bit :: Bool -> Int
bit True = 1
bit False = 0

single :: BoxLabel -> Bool
single (ExpBox {}) = True
single (TopLevelBox _) = True
single (LocalBox _) = True
single (BinBox {}) = False

modInfo :: Flags -> Bool -> TixModule -> IO ModInfo
#if __GLASGOW_HASKELL__ >= 907
modInfo hpcflags qualDecList tix@(TixModule moduleName _ _ tickCounts _ _) = do
#else
modInfo hpcflags qualDecList tix@(TixModule moduleName _ _ tickCounts) = do
#endif
  Mix _ _ _ _ mes <- readMixWithFlags hpcflags (Right tix)
  return (q (accumCounts (zip (map snd mes) tickCounts) mempty))
  where
    q mi =
      if qualDecList
        then mi {decPaths = map (moduleName :) (decPaths mi)}
        else mi

modReport :: Flags -> TixModule -> IO ()
#if __GLASGOW_HASKELL__ >= 907
modReport hpcflags tix@(TixModule moduleName _ _ _ _ _) = do
#else
modReport hpcflags tix@(TixModule moduleName _ _ _) = do
#endif
  mi <- modInfo hpcflags False tix
  if xmlOutput hpcflags
    then putStrLn $ "  <module name = " ++ show moduleName ++ ">"
    else putStrLn ("-----<module " ++ moduleName ++ ">-----")
  printModInfo hpcflags mi
  when (xmlOutput hpcflags) $ do
    putStrLn "  </module>"

printModInfo :: Flags -> ModInfo -> IO ()
printModInfo hpcflags mi | xmlOutput hpcflags = do
  element "exprs" (xmlBT $ exp mi)
  element "booleans" (xmlBBT $ allBinCounts mi)
  element "guards" (xmlBBT $ guard mi)
  element "conditionals" (xmlBBT $ cond mi)
  element "qualifiers" (xmlBBT $ qual mi)
  element "alts" (xmlBT $ alt mi)
  element "local" (xmlBT $ loc mi)
  element "toplevel" (xmlBT $ top mi)
printModInfo hpcflags mi = do
  putStrLn (btPercentage "expressions used" (exp mi))
  putStrLn (bbtPercentage "boolean coverage" False (allBinCounts mi))
  putStrLn ("     " ++ bbtPercentage "guards" True (guard mi))
  putStrLn ("     " ++ bbtPercentage "'if' conditions" True (cond mi))
  putStrLn ("     " ++ bbtPercentage "qualifiers" True (qual mi))
  putStrLn (btPercentage "alternatives used" (alt mi))
  putStrLn (btPercentage "local declarations used" (loc mi))
  putStrLn (btPercentage "top-level declarations used" (top mi))
  modDecList hpcflags mi

modDecList :: Flags -> ModInfo -> IO ()
modDecList hpcflags mi0 =
  when (decList hpcflags && someDecsUnused mi0) $ do
    putStrLn "unused declarations:"
    mapM_ showDecPath (sort (decPaths mi0))
  where
    someDecsUnused mi =
      tixCount (top mi) < boxCount (top mi)
        || tixCount (loc mi) < boxCount (loc mi)
    showDecPath dp =
      putStrLn ("     " ++ intercalate "." dp)

reportPlugin :: Plugin
reportPlugin =
  Plugin
    { name = "report",
      usage = "[OPTION] .. <TIX_FILE> [<MODULE> [<MODULE> ..]]",
      options = reportFlags,
      summary = "Output textual report about program coverage",
      implementation = reportMain,
      init_flags = defaultFlags,
      final_flags = defaultFinalFlags
    }

reportMain :: Flags -> [String] -> IO ()
reportMain hpcflags (progName : mods) = do
  let hpcflags1 = hpcflags {includeMods = Set.fromList mods `Set.union` includeMods hpcflags}
  let prog = getTixFileName progName
  tix <- readTix prog
  case tix of
    Just (Tix tickCounts) ->
      makeReport hpcflags1 progName $
        sortBy (\mod1 mod2 -> tixModuleName mod1 `compare` tixModuleName mod2) $
#if __GLASGOW_HASKELL__ >= 907
          [tix' | tix'@(TixModule m _ _ _ _ _) <- tickCounts, allowModule hpcflags1 m]
#else
          [tix' | tix'@(TixModule m _ _ _) <- tickCounts, allowModule hpcflags1 m]
#endif
    Nothing -> hpcError reportPlugin $ "unable to find tix file for:" ++ progName
reportMain _ [] =
  hpcError reportPlugin "no .tix file or executable name specified"

makeReport :: Flags -> String -> [TixModule] -> IO ()
makeReport hpcflags progName modTcs | xmlOutput hpcflags = do
  putStrLn "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  putStrLn $ "<coverage name=" ++ show progName ++ ">"
  when (perModule hpcflags) $ do
    mapM_ (modReport hpcflags) modTcs
  mis <- mapM (modInfo hpcflags True) modTcs
  putStrLn "  <summary>"
  printModInfo hpcflags (mconcat mis)
  putStrLn "  </summary>"
  putStrLn "</coverage>"
makeReport hpcflags _ modTcs =
  if perModule hpcflags
    then mapM_ (modReport hpcflags) modTcs
    else do
      mis <- mapM (modInfo hpcflags True) modTcs
      printModInfo hpcflags (mconcat mis)

element :: String -> [(String, String)] -> IO ()
element tag attrs =
  putStrLn $
    "    <"
      ++ tag
      ++ " "
      ++ unwords
        [ x ++ "=" ++ show y
          | (x, y) <- attrs
        ]
      ++ "/>"

xmlBT :: BoxTixCounts -> [(String, String)]
xmlBT (BT b t) =
  [ ("boxes", show b),
    ("count", show t)
  ]

xmlBBT :: BinBoxTixCounts -> [(String, String)]
xmlBBT (BBT b tt tf bt) =
  [ ("boxes", show b),
    ("true", show tt),
    ("false", show tf),
    ("count", show (tt + tf + bt))
  ]

------------------------------------------------------------------------------

reportFlags :: FlagOptSeq
reportFlags =
  perModuleOpt
    . decListOpt
    . excludeOpt
    . includeOpt
    . srcDirOpt
    . hpcDirOpt
    . resetHpcDirsOpt
    . xmlOutputOpt
    . verbosityOpt
