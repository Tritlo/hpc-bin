module HpcShowTix (showtix_plugin) where

import Trace.Hpc.Mix
import Trace.Hpc.Tix

import HpcFlags

import qualified Data.Set as Set

showtix_options = 
  [ excludeOpt,includeOpt,hpcDirOpt
  , outputOpt
  ]

showtix_plugin = Plugin { name = "show"
	      	       , usage = "[OPTION] .. <TIX_FILE> [<MODULE> [<MODULE> ..]]" 
		       , options = showtix_options 
		       , summary = "Show .tix file in readable, verbose format"
		       , implementation = showtix_main
		       , init_flags = default_flags
		       , final_flags = default_final_flags
		       }



showtix_main flags [] = hpcError showtix_plugin $ "no .tix file or executable name specified" 
showtix_main flags (prog:modNames) = do
  let hpcflags1 = flags 
      		{ includeMods = Set.fromList modNames
  	      	     	 	   `Set.union` 
				includeMods flags }

  optTixs <- readTix (getTixFileName prog)
  case optTixs of
    Nothing -> hpcError showtix_plugin $ "could not read .tix file : "  ++ prog
    Just (Tix tixs) -> do
       let modules = map tixModuleName tixs       

       mixs <- sequence
               [ readMix (hpcDirs hpcflags1) modName            -- hard wired to .hpc for now
               | modName <- modules
	       , allowModule hpcflags1 modName
               ]
     
       let rjust n str = take (n - length str) (repeat ' ') ++ str 
       let ljust n str = str ++ take (n - length str) (repeat ' ') 
     
       sequence_ [ sequence_ [ putStrLn (rjust 5 (show ix) ++ " " ++
                                         rjust 10 (show count) ++ " " ++
                                         ljust 20  modName ++ " " ++ rjust 20 (show pos) ++ " " ++ show lab)
                             | (count,ix,(pos,lab)) <- zip3 tixs [(0::Int)..] entries
                             ]
                 | ( TixModule modName hash _ tixs
                   , Mix _file _timestamp _hash _tab entries
                   ) <- zip tixs mixs
                 ]
       
       return ()
     