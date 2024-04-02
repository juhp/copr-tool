{-# LANGUAGE CPP, OverloadedStrings #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra (forM, unless, void, when, whenJust)
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
  getCurrentTimeZone, NominalDiffTime, TimeZone, utcToZonedTime)
import Data.Time.Clock (diffUTCTime, UTCTime)
import Data.Time.Clock.System (SystemTime(MkSystemTime), systemToUTCTime)
import System.Time.Extra (sleep)
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
      <*> switchWith 'q' "quiet" "Quiet output"
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
  let (user,proj) = splitCopr' copr
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
  let (user,proj) = splitCopr' copr
  res <- coprGetPackageList url user proj
  let items = lookupKey' "items" res :: [Object]
  -- FIXME improve output/order fields
  mapM_ putStrLn $ mapMaybe (lookupKey "name") items

-- Not that useful - maybe remove?
packageCmd :: String -> String -> String -> IO ()
packageCmd url copr pkg = do
  let (user,proj) = splitCopr' copr
  res <- coprGetPackage url user proj pkg
  -- FIXME improve output/order fields
  B.putStrLn $ encode res

--type ChrootResult = (Text

monitorCmd :: String -> String -> Maybe String -> IO ()
monitorCmd url copr mfields = do
  let (user,proj) = splitCopr' copr
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

splitCopr :: String -> Maybe (String, String)
splitCopr copr =
  case splitOn "/" copr of
    [u,c] | not (null u || null c) -> Just (u,c)
    _ -> Nothing

splitCopr' :: String -> (String, String)
splitCopr' copr =
  case splitOn "/" copr of
    [u,c] | not (null u || null c) -> (u,c)
    _ -> error' "expected copr project format: user/proj"

-- FIXME repeat until all done
-- FIXME optional arch/chroot
coprProgress :: Bool -> Bool -> String -> String -> Maybe Int -> IO ()
coprProgress debug quiet server copr mbuild = do
  builds <-
    case mbuild of
      Nothing -> do
        (user,proj) <-
          case splitCopr copr of
            Just (u,p) -> return (u,p)
            Nothing -> do
              fasid <- fasIdFromKrb
              return (fasid, copr)
        res <- coprGetBuildList server user proj [makeItem "status" "running"]
        when debug $ print res
        whenJust (lookupKey "error" res) error'
        return $ (lookupKey' "items" res :: [Object])
      Just buildid -> pure <$> coprGetBuild server buildid
  when debug $ print builds
  tz <- getCurrentTimeZone
  mapM_ (runProgress tz) builds
  where
    runProgress :: TimeZone -> Object -> IO ()
    runProgress tz obj =
      case readBuild obj of
        Just bld -> do
          putStrLn $ bld_pkgname bld ++ "-" ++ bld_version bld
          void $ doProgress tz bld
        Nothing -> error' $ "incomplete build:" +-+ show obj

    doProgress :: TimeZone -> Build -> IO Build
    doProgress tz build@(Build project bid chroots pkgname _version start) = do
      -- copr-be.cloud.fedoraproject.org to avoid cloudflare caching        let
      let repo_url = "https://copr-be.cloud.fedoraproject.org/results" +/+ project
      unless quiet $ do
        putStrLn $ show (utcToZonedTime tz start) +-+ show bid
        putStrLn $ trailingSlash $ "https://copr.fedorainfracloud.org/coprs" +/+ project +/+ "build" +/+ show bid
      mrunning <-
        forM chroots $ \(Chroot chroot sizetime) -> do
        let results = repo_url +/+ chroot +/+ displayBuild bid ++ "-" ++ pkgname
        unless quiet $ putStrLn $ trailingSlash results
        -- FIXME maybe get all Headers
        -- FIXME this redirects to builder-live.log.gz
        exists <- httpExists' $ results +/+ "builder-live.log"
        if exists
          then do
          -- FIXME check changed
          sizetime' <- httpFileSizeTime' $ results +/+ "builder-live.log"
          let update = sizetime /= sizetime'
          when update $
            renderBuild chroot tz sizetime' start
          success <- httpExists' $ results +/+ "success"
          if success
            then putStrLn " done" >> return Nothing
            else do
            when update (putStrLn "")
            return (Just (Chroot chroot sizetime'))
          else do
          putStrLn $ chroot ++ ": no builder-live.log yet"
          return $ Just (Chroot chroot sizetime)
      when debug $ print mrunning
      case catMaybes mrunning of
        [] -> return build
        running -> do
          sleep 61
          doProgress tz $ build {bld_chroots = running}

    displayBuild :: Int -> String
    displayBuild bid =
      (if bid < 10000000 then ('0' :) else id) $
      show bid

    renderBuild chroot tz (msize, mtime) start =
      whenJust mtime $ \t ->
      -- FIXME render in KB
      -- FIXME just time no date
      putStr $ show (utcToZonedTime tz t) +-+ '(' : renderDuration True (diffUTCTime t start) ++ "):" +-+ maybe "0" show msize ++ "B" +-+ chroot


data Chroot = Chroot String (Maybe Integer, Maybe UTCTime)
  deriving Show

data Build =
  Build
  { _project :: String -- copr/project
  , _id :: Int
  , bld_chroots :: [Chroot]
  , bld_pkgname :: String
  , bld_version :: String
  , _start :: UTCTime
  }

readBuild :: Object -> Maybe Build
readBuild build = do
  ownername <- lookupKey "ownername" build
  projectname <- lookupKey "projectname" build
  bid <- lookupKey "id" build
  chroots <- lookupKey "chroots" build
  let src_pkg = lookupKey "source_package" build
  name <- src_pkg >>= lookupKey "name"
  version <- src_pkg >>= lookupKey "version"
  started_on <- readTime' <$> lookupKey "started_on" build
  return $ Build (ownername +/+ projectname) bid (zipWith Chroot chroots (repeat (Nothing,Nothing))) name version started_on

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

-- copied from fbrnch Koji
maybeFasIdFromKrb :: IO (Maybe String)
maybeFasIdFromKrb =
  fmap (removeSuffix "@FEDORAPROJECT.ORG") . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) <$> klistEntryFedora

fasIdFromKrb :: IO String
fasIdFromKrb = do
  mfasid <- maybeFasIdFromKrb
  case mfasid of
    Nothing -> error' "Could not determine fasid from klist"
    Just fasid -> return fasid

klistEntryFedora :: IO [String]
klistEntryFedora = do
  mres <- cmdMaybe "klist" ["-l"]
  return $
    maybe []
    (words . fromMaybe "" . find ("@FEDORAPROJECT.ORG" `isInfixOf`) . lines)
    mres
