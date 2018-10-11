import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..), InstallDirs(..), absoluteInstallDirs)
import System.FilePath
import System.Directory
import System.Process.Typed
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

windowsKitPath :: FilePath
windowsKitPath = "C:\\Program Files (x86)\\Windows Kits\\10\\lib\\10.0.10240.0"

vslinkPath :: FilePath
vslinkPath =  "C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\bin\\amd64\\link.exe"

vslibPath :: FilePath
vslibPath = "C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\lib\\amd64\\*.lib"

vsenvlibPath :: [FilePath]
vsenvlibPath =
    [ windowsKitPath </> "ucrt" </> "x64"
    , windowsKitPath </> "um" </> "x64"
    ]

getNvapiDir :: IO FilePath
getNvapiDir = (\cd -> cd </> "nvapi" </> "amd64" ) <$> getCurrentDirectory
    
main = defaultMainWithHooks simpleUserHooks
    { confHook = \x fs -> prepareNvapi fs >>= confHook simpleUserHooks x
    , copyHook = \a b c d -> copyHook simpleUserHooks a b c d >> copyNVapiLib a b c d
    }

prepareNvapi :: ConfigFlags -> IO ConfigFlags
prepareNvapi flags = do

    -- get dir
    nvapiDir <- getNvapiDir
    tmpDir <- getTemporaryDirectory

    let nvapi64Lib = nvapiDir </> "nvapi64.lib"
        nvapiLib   = nvapiDir </> "nvapi.lib"
        nvapiDll   = nvapiDir </> "nvapi.dll"
        nvapiExp   = nvapiDir </> "nvapi.exp"

    -- check if we can generate dll
    canGen <- fmap and . sequence $
      [ doesFileExist vslinkPath
      , doesFileExist nvapi64Lib
      , doesDirectoryExist windowsKitPath
      , doesDirectoryExist (takeDirectory vslibPath)
      ] <> map doesDirectoryExist vsenvlibPath

    if not canGen
    then putStrLn "Could not find some of the required components, skipping creation of nvapi.dll"
    else do        

      -- cleanup previous versions if necessary
      doesFileExist nvapiLib >>= flip when (removeFile nvapiLib)
      doesFileExist nvapiDll >>= flip when (removeFile nvapiDll)
      doesFileExist nvapiExp >>= flip when (removeFile nvapiExp)

      -- run VS linker
      runProcess_
        . setEnv [ ("LIB", intercalate ";" vsenvlibPath)
                , ("TEMP", tmpDir)
                , ("TMP", tmpDir)
                ]
        . setWorkingDir nvapiDir
        $ proc vslinkPath [ "/dll", "/out:nvapi.dll", "/machine:X64", "/def:nvapi.def", "nvapi64.lib", vslibPath]

      -- delete byproducts
      doesFileExist nvapiExp >>= flip when (removeFile nvapiExp)
      doesFileExist nvapiLib >>= flip when (removeFile nvapiLib)

      -- Create an import lib
      dlltoolPath <- fromMaybe (error "Could not lookup dlltool") <$> findExecutable "dlltool"
      runProcess_
        . setEnv [ ("TEMP", tmpDir)
                , ("TMP", tmpDir)
                ]
        . setWorkingDir nvapiDir
        $ shell $ dlltoolPath <> " -d nvapi.def -l nvapi.lib"

    return $ flags { configExtraLibDirs = nvapiDir : configExtraLibDirs flags }


copyNVapiLib :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
copyNVapiLib pkg_descr lbi _ flags = do
    let binPref = bindir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    
    nvapiDir <- getNvapiDir
    copyFile (nvapiDir </> "nvapi.dll") (binPref </> "nvapi.dll")
