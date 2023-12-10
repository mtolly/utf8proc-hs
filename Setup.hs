module Main (main) where

import           Data.Functor                       (void)
import           Data.Maybe                         (fromJust)
import           Distribution.PackageDescription    as PD
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PreProcess
import           Distribution.Simple.Setup          (ConfigFlags, CopyFlags)
import           Distribution.Types.HookedBuildInfo (HookedBuildInfo,
                                                     emptyHookedBuildInfo)
import           Distribution.Types.Library         (Library (..))
import           System.Directory                   (copyFile,
                                                     createDirectoryIfMissing,
                                                     getCurrentDirectory)
import           System.Process                     (CreateProcess (..),
                                                     callProcess, proc,
                                                     readCreateProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preConf = myPreConf, confHook = myConfHook, copyHook = myCopyHook }

-- On my Stack system, for an external package such as a git reference, this gives me something like
-- $HOME/.stack/snapshots/x86_64-linux-tinfo6/fccfa9932e1adafc2949e626b62baeceff08655250fc8a4a10c9ee215c88d179/9.6.2/lib
staticLibOutputDir :: PackageDescription -> LocalBuildInfo -> FilePath
staticLibOutputDir packageDescription localBuildInfo
  = flibdir $ absoluteComponentInstallDirs
    packageDescription
    localBuildInfo
    (localUnitId localBuildInfo)
    NoCopyDest

-- Run make in the library folder to produce the static library
myPreConf :: Args -> ConfigFlags -> IO HookedBuildInfo
myPreConf _ _ = do
  void $ readCreateProcess (proc "make" ["libutf8proc.a"]) { cwd = Just "cbits/utf8proc-2.9.0" } ""
  -- Return empty HookedBuildInfo as we are not modifying build info here.
  return emptyHookedBuildInfo

-- Add the directories containing the header file and the static library to the package.
-- Need to jump through hoops because a relative folder is not permitted in extra-lib-dirs
myConfHook
  :: (PD.GenericPackageDescription, PD.HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
myConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
  dir <- getCurrentDirectory
  return localBuildInfo
    { localPkgDescr = packageDescription
      { PD.library = Just library
        { PD.libBuildInfo = libraryBuildInfo
          { PD.includeDirs
            = (dir ++ "/cbits/utf8proc-2.9.0") -- utf8proc.h
            : PD.includeDirs libraryBuildInfo
          , PD.extraLibDirs
            = (dir ++ "/cbits/utf8proc-2.9.0") -- this is needed while building this package
            : staticLibOutputDir packageDescription localBuildInfo -- this is the permanent place we copy to, needed while linking downstream
            : PD.extraLibDirs libraryBuildInfo
          }
        }
      }
    }

myCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
myCopyHook packageDescription localBuildInfo userHooks copyFlags = do
  copyHook simpleUserHooks packageDescription localBuildInfo userHooks copyFlags
  copyFile "cbits/utf8proc-2.9.0/libutf8proc.a"
    (staticLibOutputDir packageDescription localBuildInfo <> "/libutf8proc.a")
