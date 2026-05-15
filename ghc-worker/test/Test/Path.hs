module Test.Path where

import Control.Monad.Extra (whenM)
import GHC.Data.OsPath (doesFileExist, unsafeDecodeUtf)
import System.Directory.OsPath (removeFile)
import System.OsPath (OsPath, osp, (<.>), (</>))
import System.OsPath.Extra (toOsPath)
import Test.Data.Project (ModuleKey (..), UnitKey (..))

-- * Unit Names and Paths

showUnit :: UnitKey -> String
showUnit (UnitKey key) = show key

unitName :: UnitKey -> String
unitName unit = "unit" ++ showUnit unit

unitDir :: UnitKey -> OsPath
unitDir = toOsPath . unitName

unitOutputDir :: UnitKey -> OsPath
unitOutputDir key = [osp|out|] </> unitDir key

unitTmpDir :: UnitKey -> OsPath
unitTmpDir key = [osp|meta|] </> unitDir key

unitCacheDir :: UnitKey -> OsPath
unitCacheDir unit = [osp|cache|] </> unitDir unit

-- * Module Names and Paths

moduleName :: ModuleKey -> String
moduleName ModuleKey {unit, number} =
  "Unit" ++ showUnit unit ++ "Module" ++ show number

moduleValueName :: ModuleKey -> String
moduleValueName ModuleKey {unit, number} =
  "value_" ++ showUnit unit ++ "_" ++ show number

moduleOutputBase :: ModuleKey -> OsPath
moduleOutputBase key =
  unitOutputDir key.unit </> toOsPath (moduleName key)

moduleSourcePath :: ModuleKey -> OsPath
moduleSourcePath key =
  unitDir key.unit </> toOsPath (moduleName key) <.> [osp|hs|]

compileTmpDir :: ModuleKey -> OsPath
compileTmpDir key =
  unitDir key.unit </> toOsPath (moduleName key)

cachedUnitPath :: UnitKey -> OsPath
cachedUnitPath unit =
  unitCacheDir unit </> [osp|cached_unit.json|]

removeIfExists :: OsPath -> IO ()
removeIfExists path = do
  whenM (doesFileExist path) do
    removeFile path
