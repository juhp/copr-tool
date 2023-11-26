{-# LANGUAGE CPP, OverloadedStrings #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad (forM_, when)
import Data.Aeson.Types
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Strict as M
#endif
import qualified Data.ByteString.Char8 as B
import Data.List.Extra
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (
#if MIN_VERSION_time(1,9,0)
  defaultTimeLocale, formatTime,
#endif
  getCurrentTimeZone, NominalDiffTime, utcToZonedTime)
import Data.Time.Clock (diffUTCTime, UTCTime)
import Data.Time.Clock.System (SystemTime(MkSystemTime), systemToUTCTime)
import Data.Tuple.Extra
import Data.Yaml (encode)
import Network.HTTP.Directory (httpExists', httpFileSizeTime', trailingSlash,
                               (+/+))
import SimpleCmd
import SimpleCmdArgs
import Web.Fedora.Copr
import Web.Fedora.Copr.API

import qualified Paths_copr_tool

main :: IO ()
main = do
  simpleCmdArgs (Just Paths_copr_tool.version)
    "Query tool for Copr"
    "see https://github.com/juhp/copr-tool#readme" $
    subcommands
    [
      Subcommand "search"
      "Search Copr projects" $
      searchCmd
      <$> coprServerOpt
--      <*> many (strOptionWith 'a' "arch" "ARCH" "Task arch")
      <*> strArg "QUERY"

    , Subcommand "owner"
      "List owner's projects" $
      ownerCmd
      <$> coprServerOpt
      <*> strArg "USER"

    , Subcommand "project"
      "Show project details" $
      projectCmd
      <$> coprServerOpt
      <*> strArg "COPR"

    , Subcommand "monitor"
      "Latest project chroot builds" $
      monitorCmd
      <$> coprServerOpt
      <*> strArg "COPR"
      <*> optional (strOptionWith 'f' "fields" "FIELDS" "Comma separated list of extra fields")

    , Subcommand "packages"
      "List project packages" $
      packagesCmd
      <$> coprServerOpt
      <*> strArg "COPR"

    , Subcommand "package"
      "Show project package details" $
      packageCmd
      <$> coprServerOpt
      <*> strArg "COPR"
      <*> strArg "PKG"

    , Subcommand "progress"
      "Follow current builds build log sizes" $
      coprProgress
      <$> switchWith 'D' "debug" "Debug output"
      <*> coprServerOpt
      <*> strArg "COPR"
      <*> optional (argumentWith auto "BUILD")

    -- , Subcommand "install"
    --   "Install rpm packages directly from a Koji build task" $
    --   installCmd
    --   <$> switchWith 'n' "dry-run" "Don't actually download anything"
    --   <*> switchWith 'D' "debug" "More detailed output"
    --   <*> optional (strOptionWith 'S' "server" "COPRURL"
    --                 ("Copr server url [default: Fedora]"))
    --   <*> switchWith 'l' "list" "List builds"
    --   <*> switchWith 'L' "latest" "Latest build"
    --   <*> (strArg "PKG|NVR...")
    ]
  where
    coprServerOpt = strOptionalWith 'S' "server" "URL" "Copr API server [default: Fedora]" fedoraCopr
    -- disttagOpt :: String -> Parser String
    -- disttagOpt disttag = startingDot <$> strOptionalWith 'd' "disttag" "DISTTAG" ("Use a different disttag [default: " ++ disttag ++ "]") disttag

    -- startingDot cs =
    --   case cs of
    --     "" -> error' "empty disttag"
    --     (c:_) -> if c == '.' then cs else '.' : cs

-- FIXME filter --with-description/instructions etc
searchCmd :: String -> String -> IO ()
searchCmd url name = do
  res <- coprSearchProjects url name
  let items = lookupKey' "items" res
  mapM_ T.putStrLn $ mapMaybe (lookupKey "full_name") items

-- FIXME print copr project url too
projectCmd :: String -> String -> IO ()
projectCmd url copr = do
  let (user,proj) = splitCopr copr
  res <- coprGetProject url user proj
  -- FIXME improve output/order fields
  B.putStrLn $ encode res

ownerCmd :: String -> String -> IO ()
ownerCmd url user = do
  res <- coprGetProjectsList url user
  let items = lookupKey' "items" res
  mapM_ T.putStrLn $ mapMaybe (lookupKey "name") items

-- FIXME print project packages url
packagesCmd :: String -> String -> IO ()
packagesCmd url copr = do
  let (user,proj) = splitCopr copr
  res <- coprGetPackageList url user proj
  let items = lookupKey' "items" res :: [Object]
  -- FIXME improve output/order fields
  mapM_ putStrLn $ mapMaybe (lookupKey "name") items

-- Not that useful - maybe remove?
packageCmd :: String -> String -> String -> IO ()
packageCmd url copr pkg = do
  let (user,proj) = splitCopr copr
  res <- coprGetPackage url user proj pkg
  -- FIXME improve output/order fields
  B.putStrLn $ encode res

--type ChrootResult = (Text

monitorCmd :: String -> String -> Maybe String -> IO ()
monitorCmd url copr mfields = do
  let (user,proj) = splitCopr copr
  res <- coprMonitorProject url user proj $ maybe [] (splitOn ",") mfields
  let pkgs = lookupKey' "packages" res :: [Object]
  -- FIXME improve output/order fields
  (mapM_ printPkgRes . mapMaybe pkgResults) pkgs
  where
    pkgResults :: Object -> Maybe (T.Text,Object)
    pkgResults obj = do
      name <- lookupKey "name" obj
      chroots <- lookupKey "chroots" obj
      return (name,chroots)

    printPkgRes :: (T.Text,Object) -> IO ()
    printPkgRes (name,chroots) = do
      T.putStr $ name <> ": "
      mapM_ printChRes $ map (first toText) $ M.toList chroots
      T.putStrLn ""
#if !MIN_VERSION_aeson(2,0,0)
      where toText = id
#endif

    printChRes :: (T.Text,Value) -> IO ()
    printChRes (chr, val) =
      T.putStr $ chr <>
      case val of
        Object obj -> chrState obj <> " "
        _ -> error' $ "bad chroot result for " <> T.unpack chr
      where
        chrState :: Object -> T.Text
        chrState obj = do
          case lookupKey "state" obj of
            Just st -> "[" <> st <> "]"
            Nothing -> ""

splitCopr :: String -> (String, String)
splitCopr copr =
  case splitOn "/" copr of
    [u,c] | not (null u || null c) -> (u,c)
    _ -> error' $ "bad copr name: should be user/proj"

coprProgress :: Bool -> String -> String -> Maybe Int -> IO ()
coprProgress debug server copr mbuild = do
  items <-
    case mbuild of
      Nothing -> do
        let (user,proj) = splitCopr copr
        res <- coprGetBuildList server user proj [makeItem "status" "running"]
        return $ (lookupKey' "items" res :: [Object])
      Just buildid -> pure <$> coprGetBuild server buildid
  when debug $ print items
  forM_ items $ \build -> do
    when debug $ print build
    let ownername = lookupKey' "ownername" build
        projectname = lookupKey' "projectname" build
        -- copr-be.cloud.fedoraproject.org to avoid cloudflare caching
        repo_url = "https://copr-be.cloud.fedoraproject.org/results" +/+ ownername +/+ projectname
        chroots = lookupKey' "chroots" build :: [String]
        bid = lookupKey' "id" build :: Int
        source_package = lookupKey' "source_package" build
        name = lookupKey' "name" source_package
        -- FIXME can this fail?
        started_on = readTime' $ lookupKey' "started_on" build
    tz <- getCurrentTimeZone
    putStrLn $ show (utcToZonedTime tz started_on) +-+ show bid
    putStrLn $ trailingSlash $ "https://copr.fedorainfracloud.org/coprs" +/+ ownername +/+ projectname +/+ "build" +/+ show bid
    forM_ (sort chroots) $ \chroot -> do
      let results = repo_url +/+ chroot +/+ displayBuild bid ++ "-" ++ name
      -- FIXME maybe get all Headers
      -- FIXME this redirects to builder-live.log.gz
      sizetime <- httpFileSizeTime' $ results +/+ "builder-live.log"
      putStr $ renderBuild chroot tz sizetime started_on
      success <- httpExists' $ results +/+ "success"
      if success
        then putStrLn " done"
        else putStrLn ""
      putStrLn $ trailingSlash results
  where
    displayBuild :: Int -> String
    displayBuild bid =
      (if bid < 10000000 then ('0' :) else id) $
      show bid

    renderBuild chroot tz (msize, mtime) start =
      -- FIXME render in KB
      maybe "" (\t -> show (utcToZonedTime tz t) +-+ renderDuration True (diffUTCTime t start)) mtime +-+ maybe "0" show msize ++ "B" +-+ chroot

-- from koji-tool Time
readTime' :: Double -> UTCTime
readTime' =
  let mkSystemTime t = MkSystemTime t 0
  in systemToUTCTime . mkSystemTime . truncate

-- derived from koji-tool Time
renderDuration :: Bool -> NominalDiffTime -> String
#if MIN_VERSION_time(1,9,0)
renderDuration short dur =
  let fmtstr
        | dur < 60 = "%s" ++ secs
        | dur < 3600 = "%m" ++ mins ++ "%S" ++ secs
        | otherwise = "%h" ++ hrs ++ "%M" ++ mins
  in formatTime defaultTimeLocale fmtstr dur
  where
    secs = if short then "s" else " sec"
    mins = if short then "m" else " min"
    hrs = if short then "h" else " hours"
#else
renderDuration short dur =
  show dur ++ if short then "" else "sec"
#endif
