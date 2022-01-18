{-# LANGUAGE OverloadedStrings #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.Aeson.Types
import Data.List.Extra
import Data.Maybe
import qualified Data.Text.IO as T
import SimpleCmd
import SimpleCmdArgs
import Web.Fedora.Copr
import Web.Fedora.Copr.API

import qualified Paths_copr_tool

main :: IO ()
main = do
  simpleCmdArgs (Just Paths_copr_tool.version)
    "Query and install Copr builds"
    "see https://github.com/juhp/copr-tool#readme" $
    subcommands
    [
      Subcommand "search"
      "Search Copr projects" $
      searchCmd
      <$> strOptionalWith 'S' "server" "URL" "Koji Hub [default: Fedora]" fedoraCopr
      <*> many (strOptionWith 'a' "arch" "ARCH" "Task arch")
      <*> switchWith 'D' "debug" "Pretty-pretty raw XML result"
      -- FIXME error if integer (eg mistakenly taskid)
    -- , Subcommand "install"
    --   "Install rpm packages directly from a Koji build task" $
    --   installCmd
    --   <$> switchWith 'n' "dry-run" "Don't actually download anything"
    --   <*> switchWith 'D' "debug" "More detailed output"
    --   <*> optional (strOptionWith 'S' "server" "COPRURL"
    --                 ("Copr server url [default: Fedora]"))
    --   <*> switchWith 'l' "list" "List builds"
    --   <*> switchWith 'L' "latest" "Latest build"
      <*> (strArg "PKG|NVR...")
    ]
  where
    -- disttagOpt :: String -> Parser String
    -- disttagOpt disttag = startingDot <$> strOptionalWith 'd' "disttag" "DISTTAG" ("Use a different disttag [default: " ++ disttag ++ "]") disttag

    startingDot cs =
      case cs of
        "" -> error' "empty disttag"
        (c:_) -> if c == '.' then cs else '.' : cs

searchCmd :: String -> [String] -> Bool -> String -> IO ()
searchCmd url _archs _debug project = do
  res <- coprSearchProjects' url project
  let items = lookupKey' "items" res
  mapM_ T.putStrLn $ mapMaybe (lookupKey "name") items

coprSearchProjects' :: String -> String -> IO Object
coprSearchProjects' server query = do
  let path = "project/search"
      params = makeKey "query" query
  queryCopr server path params
