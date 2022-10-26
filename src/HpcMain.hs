{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module HpcMain ( main ) where
-- (c) 2007 Andy Gill

-- Main driver for Hpc
import Control.Monad (forM, forM_, when)
import Data.Bifunctor (bimap)
import Data.List (intercalate, partition, uncons)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes, isJust)
import Data.Version ( showVersion )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import System.Console.GetOpt ( getOpt, ArgOrder(Permute) )
import System.Directory (doesPathExist)

import HpcFlags
    ( Plugin(..),
      Flags,
      default_flags,
      default_final_flags,
      command_usage )
import HpcReport ( report_plugin )
import HpcMarkup ( markup_plugin )
import HpcCombine ( sum_plugin, combine_plugin, map_plugin )
import HpcShowTix ( showtix_plugin )
import HpcDraft ( draft_plugin )
import HpcOverlay ( overlay_plugin )
import Paths_hpc_bin ( version )

helpList :: IO ()
helpList = do
     putStrLn $
           "Usage: hpc COMMAND ...\n\n" <>
           section "Commands" [help_plugin] <>
           section "Reporting Coverage" [report_plugin, markup_plugin] <>
           section "Processing Coverage files" [sum_plugin, combine_plugin, map_plugin] <>
           section "Coverage Overlays" [overlay_plugin, draft_plugin] <>
           section "Others" [showtix_plugin, version_plugin] <>
           ""
     putStrLn ""
     putStrLn "or: hpc @response_file_1 @response_file_2 ..."
     putStrLn ""
     putStrLn "The contents of a Response File must have this format:"
     putStrLn "COMMAND ..."
     putStrLn ""
     putStrLn "example:"
     putStrLn "report my_library.tix --include=ModuleA \\"
     putStrLn "--include=ModuleB"

-- | Print the summaries of all plugins belonging to a given section.
section :: String -- ^ Name of the section.
        -> [Plugin] -- ^ Plugins belonging to that section.
        -> String
section msg plugins = msg <> ":\n" <> unlines summaries
  where
    summaries = [ take 14 ("  " <> name plugin <> repeat ' ') <> summary plugin | plugin <- plugins ]

main :: IO ()
main = do
 args <- getArgs
 dispatch args

-- | The main dispatch function. It either accepts a valid command followed by a list of its arguments,
-- or a list of response files of the form '@filename'.
dispatch :: [String] -> IO ()
dispatch [] = do
    helpList
    exitWith ExitSuccess
dispatch (txt:args0) = do
    case lookup txt hooks' of
      Just plugin -> dispatchOnPlugin plugin args0
      _ -> case getResponseFileName txt of
        Nothing -> dispatchOnPlugin help_plugin (txt:args0)
        Just firstResponseFileName -> do
          let
            (responseFileNames', nonResponseFileNames) = partitionFileNames args0
          -- if arguments are combination of Response Files and non-Response Files, exit with error
          when (length nonResponseFileNames > 0) $ do
            let
            putStrLn $ "First argument '" <> txt <> "' is a Response File, " <>
              "followed by non-Response File(s): '" <> intercalate "', '" nonResponseFileNames <> "'"
            putStrLn $ "When first argument is a Response File, " <>
              "all arguments should be Response Files."
            exitFailure
          dispatchOnResponseFiles (firstResponseFileName :| responseFileNames')
  where
     getResponseFileName :: String -> Maybe FilePath
     getResponseFileName s = do
       (firstChar, filename) <- uncons s
       if firstChar == '@'
         then pure filename
         else Nothing

     -- first member of tuple is list of Response File names,
     -- second member of tuple is list of all other arguments
     partitionFileNames :: [String] -> ([FilePath], [String])
     partitionFileNames xs = let
       hasFileName :: [(String, Maybe FilePath)]
       hasFileName = fmap (\x -> (x, getResponseFileName x)) xs
       (fileNames, nonFileNames) :: ([Maybe FilePath], [String]) =
         bimap (fmap snd) (fmap fst) $ partition (isJust . snd) hasFileName
       in (catMaybes fileNames, nonFileNames)

-- | Dispatch on a given list of response files.
dispatchOnResponseFiles :: NonEmpty FilePath -> IO ()
dispatchOnResponseFiles fps = do
  forM_ fps $ \responseFileName -> do
    exists <- doesPathExist responseFileName
    when (not exists) $ do
      putStrLn $ "Response File '" <> responseFileName <> "' does not exist"
      exitFailure

  -- read all Response Files
  responseFileNamesAndText :: NonEmpty (FilePath, String) <-
    forM fps $ \responseFileName ->
      fmap (responseFileName, ) (readFile responseFileName)
  forM_ responseFileNamesAndText $ \(responseFileName, responseFileText) ->
    -- parse first word of Response File, which should be a command
    case uncons $ words responseFileText of
      Nothing -> do
        putStrLn $ "Response File '" <> responseFileName <> "' has no command"
        exitFailure
      Just (responseFileCommand, args1) -> case lookup responseFileCommand hooks' of
        -- check command for validity
        -- It is important than a Response File cannot specify another Response File;
        -- this is prevented
        Nothing -> do
          putStrLn $ "Response File '" <> responseFileName <>
            "' command '" <> responseFileCommand <> "' invalid"
          exitFailure
        Just plugin -> do
          putStrLn $ "Response File '" <> responseFileName <> "':"
          dispatchOnPlugin plugin args1

-- | Dispatch on a given plugin and its arguments.
dispatchOnPlugin :: Plugin -> [String] -> IO ()
dispatchOnPlugin plugin args =
  case getOpt Permute (options plugin []) args of
    (_,_,errs) | not (null errs) -> do
      putStrLn "hpc failed:"
      sequence_ [ putStr ("  " <> err) | err <- errs ]
      putStrLn $ "\n"
      command_usage plugin
      exitFailure
    (o,ns,_) -> do
      let flags = final_flags plugin
                . foldr (.) id o
                $ init_flags plugin
      implementation plugin flags ns

------------------------------------------------------------------------------

hooks :: [Plugin]
hooks = [ help_plugin
        , report_plugin
        , markup_plugin
        , sum_plugin
        , combine_plugin
        , map_plugin
        , showtix_plugin
        , overlay_plugin
        , draft_plugin
        , version_plugin
        ]

hooks' :: [(String, Plugin)]
hooks' = [ (name hook,hook) | hook <- hooks ]

------------------------------------------------------------------------------

help_plugin :: Plugin
help_plugin = Plugin { name = "help"
                     , usage = "[<HPC_COMMAND>]"
                     , summary = "Display help for hpc or a single command"
                     , options = id
                     , implementation = help_main
                     , init_flags = default_flags
                     , final_flags = default_final_flags
                     }

help_main :: Flags -> [String] -> IO ()
help_main _ [] = do
            helpList
            exitWith ExitSuccess
help_main _ (sub_txt:_) = do
    case lookup sub_txt hooks' of
      Nothing -> do
          putStrLn $ "no such HPC command: " <> sub_txt
          exitFailure
      Just plugin' -> do
          command_usage plugin'
          exitWith ExitSuccess

------------------------------------------------------------------------------

version_plugin :: Plugin
version_plugin = Plugin { name = "version"
                        , usage = ""
                        , summary = "Display version for hpc"
                        , options = id
                        , implementation = version_main
                        , init_flags = default_flags
                        , final_flags = default_final_flags
                        }

version_main :: Flags -> [String] -> IO ()
version_main _ _ = putStrLn ("hpc tools, version " ++ showVersion version)


------------------------------------------------------------------------------
