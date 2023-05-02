{-# LANGUAGE CPP #-}
module HpcShowTix (showtixPlugin) where

import qualified Data.Set as Set
import HpcFlags
import Trace.Hpc.Mix
import Trace.Hpc.Tix

------------------------------------------------------------------------------

showtixOptions :: FlagOptSeq
showtixOptions =
  excludeOpt
    . includeOpt
    . srcDirOpt
    . hpcDirOpt
    . resetHpcDirsOpt
    . outputOpt
    . verbosityOpt

showtixPlugin :: Plugin
showtixPlugin =
  Plugin
    { name = "show",
      usage = "[OPTION] .. <TIX_FILE> [<MODULE> [<MODULE> ..]]",
      options = showtixOptions,
      summary = "Show .tix file in readable, verbose format",
      implementation = showtixMain,
      init_flags = defaultFlags,
      final_flags = defaultFinalFlags
    }

showtixMain :: Flags -> [String] -> IO ()
showtixMain _ [] = hpcError showtixPlugin "no .tix file or executable name specified"
showtixMain flags (prog : modNames) = do
  let hpcflags1 = flags {includeMods = Set.fromList modNames `Set.union` includeMods flags}

  optTixs <- readTix (getTixFileName prog)
  case optTixs of
    Nothing ->
      hpcError showtixPlugin $ "could not read .tix file : " ++ prog
    Just (Tix tixs) -> do
      tixs_mixs <-
        sequence
          [ do
              mix <- readMixWithFlags hpcflags1 (Right tix)
              return (tix, mix)
            | tix <- tixs,
              allowModule hpcflags1 (tixModuleName tix)
          ]

      let rjust n str = replicate (n - length str) ' ' ++ str
      let ljust n str = str ++ replicate (n - length str) ' '

      sequence_
        [ sequence_
            [ putStrLn
                ( rjust 5 (show ix)
                    ++ " "
                    ++ rjust 10 (show count)
                    ++ " "
                    ++ ljust 20 modName
                    ++ " "
                    ++ rjust 20 (show pos)
                    ++ " "
                    ++ show lab
                )
              | (count, ix, (pos, lab)) <- zip3 tixs' [(0 :: Int) ..] entries
            ]
#if __GLASGOW_HASKELL__ >= 907
          | ( TixModule modName _hash1 _ tixs' _ _ _,
#else
          | ( TixModule modName _hash1 _ tixs',
#endif
              Mix _file _timestamp _hash2 _tab entries
              ) <-
              tixs_mixs
        ]
