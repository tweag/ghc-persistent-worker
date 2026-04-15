-- | Generate a synthetic binary-tree multi-unit Haskell project for ghc-server testing.
--
-- === Deep mode (original)
--
-- The generated project has a binary tree of modules. Two tree levels are grouped into
-- two units, split horizontally (left half of modules in one unit, right half in another).
--
-- At level @l@, there are @2^(l+1)@ modules. Each module at level @l@, index @i@ imports
-- two children at @(l+1, 2*i)@ and @(l+1, 2*i+1)@. Leaf modules have no imports.
--
-- The CLI depth argument @d@ produces @2*d@ levels and @2*d@ units.
-- Module count grows exponentially.
--
-- === Wide mode
--
-- A binary tree of /units/, each with a fixed number of modules.
-- Depth @d@ produces @2^d - 1@ units (e.g. depth 10 = 1023 units).
-- Each non-leaf unit depends on its two children.
-- Module 0 of each unit imports module 0 from both child units.
module GhcServer.GenProject where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import GhcServer.Data.UnitConfig (UnitConfig (..))
import System.Directory (createDirectoryIfMissing)

-- | Configuration for external dependency packages in generated projects.
data ExtDepsConfig =
  ExtDepsConfig {
    -- | Root directory containing prebuilt ext dep packages (e.g. from @test-ext-deps.nix@).
    extDepsDir :: FilePath,
    -- | Which ext dep indexes to use (e.g. @[0..4]@).
    extDepIndexes :: [Int]
  }
  deriving stock (Show)

-- | Naming conventions matching @Test.Path@ and @test-ext-deps.nix@.
extDepName :: Int -> String
extDepName i = "extdep" ++ show i

extDepModuleName :: Int -> String
extDepModuleName i = "Extdep" ++ show i

extDepValueName :: Int -> String
extDepValueName i = "extdep_value_" ++ show i

-- | GHC CLI args to make ext dep packages visible: @-package-db@ and @-package@ per ext dep.
extDepArgs :: ExtDepsConfig -> [String]
extDepArgs cfg =
  concatMap perDep cfg.extDepIndexes
  where
    perDep i = ["-package-db", cfg.extDepsDir ++ "/" ++ extDepName i ++ "/package.conf.d",
                "-package", extDepName i]

-- ---------------------------------------------------------------------------
-- Tree geometry
-- ---------------------------------------------------------------------------

-- | Total number of tree levels.
totalLevels :: Int -> Int
totalLevels depth = 2 * depth

-- | Number of modules at a given level.
levelSize :: Int -> Int
levelSize level = 2 ^ (level + 1)

-- | The group index for a level (two levels per group).
levelGroup :: Int -> Int
levelGroup level = div level 2

-- | Number of groups.
totalGroups :: Int -> Int
totalGroups depth = depth

-- | Whether a level is the top (even) or bottom (odd) within its group.
isTopLevel :: Int -> Bool
isTopLevel level = even level

-- | The unit index for a module at a given level and position.
--
-- Each group has two units: the left unit (even) and the right unit (odd).
-- The left half of modules at each level goes to the left unit; the right half to the right.
moduleUnit :: Int -> Int -> Int
moduleUnit level index =
  2 * levelGroup level + side
  where
    halfSize = div (levelSize level) 2
    side = if index < halfSize then 0 else 1

-- | The module number within its unit.
--
-- Modules from the top level of the group come first, then the bottom level.
moduleNumber :: Int -> Int -> Int
moduleNumber level index =
  offset + localIndex
  where
    group = levelGroup level
    halfSize = div (levelSize level) 2
    localIndex = mod index halfSize
    offset
      | isTopLevel level = 0
      | otherwise = div (levelSize (2 * group)) 2

-- | A module in the tree, identified by its tree coordinates.
data TreeModule =
  TreeModule {
    level :: Int,
    index :: Int,
    unitIndex :: Int,
    modNumber :: Int
  }
  deriving stock (Show)

-- | The name of a unit.
unitName :: Int -> String
unitName i = "unit" ++ show i

-- | The name of a module given its unit and module number.
moduleName :: Int -> Int -> String
moduleName u m = "U" ++ show u ++ "M" ++ show m

-- | Compute all modules in the tree.
allModules :: Int -> [TreeModule]
allModules depth =
  [ TreeModule {level, index, unitIndex, modNumber}
  | level <- [0 .. totalLevels depth - 1]
  , index <- [0 .. levelSize level - 1]
  , let unitIndex = moduleUnit level index
  , let modNumber = moduleNumber level index
  ]

-- | The two children of a module (if it's not a leaf).
children :: Int -> TreeModule -> [(Int, Int)]
children depth m
  | m.level + 1 >= totalLevels depth = []
  | otherwise = [(m.level + 1, 2 * m.index), (m.level + 1, 2 * m.index + 1)]

-- ---------------------------------------------------------------------------
-- Unit structure
-- ---------------------------------------------------------------------------

-- | Compute which units each unit depends on.
--
-- A unit @2g@ (left) depends on unit @2(g+1)@ (left of next group).
-- A unit @2g+1@ (right) depends on unit @2(g+1)+1@ (right of next group).
-- The last group has no deps.
unitDeps :: Int -> Int -> [Int]
unitDeps depth unitIdx
  | group + 1 >= totalGroups depth = []
  | otherwise = [2 * (group + 1) + side]
  where
    group = div unitIdx 2
    side = mod unitIdx 2

-- | Total number of units.
totalUnits :: Int -> Int
totalUnits depth = 2 * depth

-- ---------------------------------------------------------------------------
-- Source generation
-- ---------------------------------------------------------------------------

-- | Build a lookup from @(level, index)@ to @(unitIndex, modNumber)@ for import resolution.
buildModuleMap :: Int -> Map.Map (Int, Int) (Int, Int)
buildModuleMap depth =
  Map.fromList
    [ ((m.level, m.index), (m.unitIndex, m.modNumber))
    | m <- allModules depth
    ]

-- | Generate the Haskell source for a module.
moduleSource :: Int -> Map.Map (Int, Int) (Int, Int) -> Maybe ExtDepsConfig -> TreeModule -> String
moduleSource depth modMap extDeps m =
  unlines $
    ["module " ++ modName ++ " where"]
    ++ importLines
    ++ extDepImports
    ++ [""]
    ++ [valueName ++ " :: Int"]
    ++ [valueName ++ " = " ++ valueExpr]
  where
    modName = moduleName m.unitIndex m.modNumber
    valueName = "value_" ++ show m.unitIndex ++ "_" ++ show m.modNumber

    childMods = [(cu, cm) | (cl, ci) <- children depth m, let (cu, cm) = modMap Map.! (cl, ci)]

    childValue cu cm = "value_" ++ show cu ++ "_" ++ show cm

    importLines =
      ["import " ++ moduleName cu cm ++ " (" ++ childValue cu cm ++ ")" | (cu, cm) <- childMods]

    -- Leaf modules import ext deps so that ext dep packages are actually exercised
    isLeaf = null childMods
    extDepImports = case extDeps of
      Just cfg | isLeaf ->
        ["import " ++ extDepModuleName i ++ " (" ++ extDepValueName i ++ ")" | i <- cfg.extDepIndexes]
      _ -> []

    extDepValues = case extDeps of
      Just cfg | isLeaf -> [extDepValueName i | i <- cfg.extDepIndexes]
      _ -> []

    allValues = [childValue cu cm | (cu, cm) <- childMods] ++ extDepValues

    valueExpr
      | null allValues = "1"
      | otherwise = intercalate " + " allValues ++ " + 1"

-- ---------------------------------------------------------------------------
-- Project writing
-- ---------------------------------------------------------------------------

-- | Write the entire project to disk.
writeProject :: FilePath -> Int -> Maybe ExtDepsConfig -> IO ()
writeProject root depth extDeps = do
  let modMap = buildModuleMap depth
      mods = allModules depth
      numUnits = totalUnits depth
  -- Create unit directories and write unit.json files
  traverse_ (writeUnitDir root depth extDeps) ([0 .. numUnits - 1] :: [Int])
  -- Write module source files
  traverse_ (writeModuleSource root depth modMap extDeps) mods

-- | Create a unit directory with its @unit.json@.
writeUnitDir :: FilePath -> Int -> Maybe ExtDepsConfig -> Int -> IO ()
writeUnitDir root depth extDeps unitIdx = do
  let dir = root ++ "/" ++ unitName unitIdx
  createDirectoryIfMissing True dir
  let deps = unitDeps depth unitIdx
      args = maybe [] extDepArgs extDeps
      config = UnitConfig {deps = map unitName deps, args}
  LBS.writeFile (dir ++ "/unit.json") (encode config)

-- | Write a single module's source file.
writeModuleSource :: FilePath -> Int -> Map.Map (Int, Int) (Int, Int) -> Maybe ExtDepsConfig -> TreeModule -> IO ()
writeModuleSource root depth modMap extDeps m = do
  let uName = unitName m.unitIndex
      mName = moduleName m.unitIndex m.modNumber
      dir = root ++ "/" ++ uName
      source = moduleSource depth modMap extDeps m
  writeFile (dir ++ "/" ++ mName ++ ".hs") source

-- ---------------------------------------------------------------------------
-- Production mode: binary tree of small units + one large downstream unit
-- ---------------------------------------------------------------------------

-- | Write a production-topology project: a binary tree of small units (3 modules each) with one
-- large downstream unit that depends on all other units.
writeProductionProject :: FilePath -> Int -> Int -> Maybe ExtDepsConfig -> IO ()
writeProductionProject root depth bigUnitModsPerLevel extDeps = do
  let treeUnits = wideUnitCount depth
  -- Write binary tree units (1-indexed heap layout, 3 modules each)
  traverse_ (writeWideUnit root treeUnits 3 extDeps) ([1 .. treeUnits] :: [Int])
  -- Write the big downstream unit that depends on the root of the tree (unit1)
  writeProductionBigUnit root treeUnits bigUnitModsPerLevel extDeps

-- | Number of levels in the big unit's dependency chain.
productionLevels :: Int
productionLevels = 20

-- | Write the large downstream unit for production mode.
-- Structure: 20 levels × N modules per level, plus a BigMain module.
-- Each module at level L imports the corresponding module at level L+1.
-- BigMain imports all level-0 modules.
writeProductionBigUnit :: FilePath -> Int -> Int -> Maybe ExtDepsConfig -> IO ()
writeProductionBigUnit root _treeUnits modsPerLevel extDeps = do
  let bigName = "unitbig"
      dir = root ++ "/" ++ bigName
      -- Depend on the tree root
      deps = [unitName 1]
      args = maybe [] extDepArgs extDeps
      config = UnitConfig {deps, args}
  createDirectoryIfMissing True dir
  LBS.writeFile (dir ++ "/unit.json") (encode config)
  -- Write level modules: BigL<level>M<idx>
  traverse_ (\(level, idx) -> writeProductionLevelModule dir modsPerLevel extDeps level idx)
    [(l, m) | l <- [0 .. productionLevels - 1], m <- [0 .. modsPerLevel - 1]]
  -- Write the main module
  writeProductionMainModule dir modsPerLevel extDeps

-- | Module name for a level module: @BigL<level>M<idx>@
productionModName :: Int -> Int -> String
productionModName level idx = "BigL" ++ show level ++ "M" ++ show idx

-- | Write a single level module.
-- Each module at level L imports the module at level L+1 with the same index.
writeProductionLevelModule :: FilePath -> Int -> Maybe ExtDepsConfig -> Int -> Int -> IO ()
writeProductionLevelModule dir modsPerLevel extDeps level idx = do
  let mName = productionModName level idx
      depImport
        | level < productionLevels - 1 =
            let depName = productionModName (level + 1) idx
                depVal = "value_" ++ show (level + 1) ++ "_" ++ show idx
            in ["import " ++ depName ++ " (" ++ depVal ++ ")"]
        | otherwise = []

      extImports = case extDeps of
        Nothing -> []
        Just cfg | level == productionLevels - 1 ->
          ["import " ++ extDepModuleName i ++ " (" ++ extDepValueName i ++ ")"
          | i <- take 1 cfg.extDepIndexes]
        _ -> []

      valName = "value_" ++ show level ++ "_" ++ show idx
      value = "value_" ++ show (level + 1) ++ "_" ++ show idx

      valueExpr
        | level < productionLevels - 1 = value ++ " + " ++ show (level * modsPerLevel + idx)
        | otherwise = case extDeps of
            Just cfg -> intercalate " + " [extDepValueName i | i <- take 1 cfg.extDepIndexes]
                        ++ " + " ++ show (level * modsPerLevel + idx)
            Nothing -> show (level * modsPerLevel + idx)

      source = unlines $
        ["module " ++ mName ++ " where"]
        ++ depImport
        ++ extImports
        ++ [""]
        ++ [valName ++ " :: Int"]
        ++ [valName ++ " = " ++ valueExpr]
  writeFile (dir ++ "/" ++ mName ++ ".hs") source

-- | Write the BigMain module that imports all level-0 modules and the cross-unit dep.
writeProductionMainModule :: FilePath -> Int -> Maybe ExtDepsConfig -> IO ()
writeProductionMainModule dir modsPerLevel extDeps = do
  let mName = "BigMain"
      l0Imports =
        ["import " ++ productionModName 0 idx ++ " (value_0_" ++ show idx ++ ")"
        | idx <- [0 .. modsPerLevel - 1]]
      crossImport = ["import U1M0 (val_0)"]
      extImports = case extDeps of
        Nothing -> []
        Just cfg -> ["import " ++ extDepModuleName i ++ " (" ++ extDepValueName i ++ ")"
                    | i <- take 1 cfg.extDepIndexes]
      localValues = ["value_0_" ++ show idx | idx <- [0 .. modsPerLevel - 1]]
      valueExpr = case extDeps of
        Just cfg -> intercalate " + " (["val_0"] ++ [extDepValueName i | i <- take 1 cfg.extDepIndexes] ++ localValues)
        Nothing -> intercalate " + " ("val_0" : localValues)
      source = unlines $
        ["module " ++ mName ++ " where"]
        ++ crossImport
        ++ l0Imports
        ++ extImports
        ++ [""]
        ++ ["main_value :: Int"]
        ++ ["main_value = " ++ valueExpr]
  writeFile (dir ++ "/" ++ mName ++ ".hs") source

-- | Total unit count for production mode.
productionUnitCount :: Int -> Int
productionUnitCount depth = wideUnitCount depth + 1

-- ---------------------------------------------------------------------------
-- Wide mode: binary tree of units with fixed module count
-- ---------------------------------------------------------------------------

-- | Total number of units in a wide binary tree: @2^depth - 1@.
wideUnitCount :: Int -> Int
wideUnitCount depth = 2 ^ depth - 1

-- | The two child unit indices for a given unit in a 1-indexed binary heap layout.
-- Unit 1 is the root. Children of unit @i@ are @2*i@ and @2*i+1@.
-- Returns empty list for leaf units.
wideUnitChildren :: Int -> Int -> [Int]
wideUnitChildren totalUnits' uid
  | left > totalUnits' = []
  | otherwise = [left, right]
  where
    left = 2 * uid
    right = 2 * uid + 1

-- | Generate the source for a module in wide mode.
--
-- Module 0 of each non-leaf unit imports @val_0@ from both child units' module 0.
-- Leaf unit's module 0 imports ext dep values when ext deps are configured.
-- All other modules are standalone.
wideModuleSource :: Int -> Int -> Int -> Maybe ExtDepsConfig -> [Int] -> String
wideModuleSource uid mid modsPerUnit extDeps childUids =
  unlines $
    ["module " ++ moduleName uid mid ++ " where"]
    ++ importLines
    ++ extDepImports
    ++ [""]
    ++ [valName ++ " :: Int"]
    ++ [valName ++ " = " ++ valueExpr]
  where
    valName = "val_" ++ show mid

    importLines
      | mid == 0 =
          [ "import qualified " ++ moduleName cu 0
          | cu <- childUids
          ]
      | otherwise = []

    isLeaf = null childUids
    extDepImports = case extDeps of
      Just cfg | mid == 0, isLeaf ->
        ["import " ++ extDepModuleName i ++ " (" ++ extDepValueName i ++ ")" | i <- cfg.extDepIndexes]
      _ -> []

    extDepValues = case extDeps of
      Just cfg | mid == 0, isLeaf -> [extDepValueName i | i <- cfg.extDepIndexes]
      _ -> []

    childRef cu = moduleName cu 0 ++ ".val_0"

    childValues = map childRef childUids
    allValues = childValues ++ extDepValues

    valueExpr
      | mid == 0, not (null allValues) =
          intercalate " + " allValues ++ " + 1"
      | otherwise = show (uid * modsPerUnit + mid)

-- | Write a flat single-unit project to disk.
--
-- Generates one unit (@unit1@) with @numModules@ modules.
-- Module 0 imports all other modules; other modules are leaf modules with no imports.
-- Designed for profiling incremental metadata: modifying module 0 triggers a metadata rerun
-- where all other modules can be served from cache.
writeFlatProject :: FilePath -> Int -> Maybe ExtDepsConfig -> IO ()
writeFlatProject root numModules extDeps = do
  let uName = "unit1"
      dir = root ++ "/" ++ uName
      args = maybe [] extDepArgs extDeps
      config = UnitConfig {deps = [], args}
  createDirectoryIfMissing True dir
  LBS.writeFile (dir ++ "/unit.json") (encode config)
  traverse_ (writeFlatModule dir numModules extDeps) ([0 .. numModules - 1] :: [Int])

-- | Write a single module for flat mode.
writeFlatModule :: FilePath -> Int -> Maybe ExtDepsConfig -> Int -> IO ()
writeFlatModule dir numModules extDeps mid = do
  let mName = flatModuleName mid
      path = dir ++ "/" ++ mName ++ ".hs"
      imports
        | mid == 0 = map flatModuleName [1 .. numModules - 1]
        | otherwise = []
      extImports = maybe [] (map extDepModuleName . (.extDepIndexes)) extDeps
      extValues = maybe [] (\cfg -> [extDepModuleName i ++ "." ++ extDepValueName i | i <- cfg.extDepIndexes]) extDeps
      allImports = imports ++ extImports
      importLines = map ("import qualified " ++) allImports
      body
        | mid == 0, not (null allImports) =
          let homeRefs = map (\m -> m ++ ".value") imports
              allRefs = homeRefs ++ extValues
              -- Group refs into chunks of 50 to avoid excessively long lines
              chunks = chunksOf 50 allRefs
              chunkBindings = zipWith chunkBinding [(0 :: Int) ..] chunks
              chunkBinding i cs = "chunk" ++ show i ++ " :: Int\nchunk" ++ show i ++ " = " ++ intercalate " + " cs
              chunkRefs = ["chunk" ++ show i | i <- [0 .. length chunks - 1]]
              topBinding = "m0_value :: Int\nm0_value = " ++ intercalate " + " chunkRefs
          in unlines (chunkBindings ++ [topBinding])
        | otherwise =
          "value :: Int\nvalue = " ++ show mid
  writeFile path $ unlines $
    ["module " ++ mName ++ " where", ""] ++
    importLines ++
    ["", body, ""]

-- | Module name for flat mode.
flatModuleName :: Int -> String
flatModuleName i = "M" ++ show i

-- | Split a list into chunks of at most @n@ elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

writeWideProject :: FilePath -> Int -> Int -> Maybe ExtDepsConfig -> IO ()
writeWideProject root depth modsPerUnit extDeps = do
  let total = wideUnitCount depth
  traverse_ (writeWideUnit root total modsPerUnit extDeps) ([1 .. total] :: [Int])

-- | Write a single unit directory for wide mode.
writeWideUnit :: FilePath -> Int -> Int -> Maybe ExtDepsConfig -> Int -> IO ()
writeWideUnit root totalUnits' modsPerUnit extDeps uid = do
  let uName = unitName uid
      dir = root ++ "/" ++ uName
      children' = wideUnitChildren totalUnits' uid
      deps = map unitName children'
      args = maybe [] extDepArgs extDeps
      config = UnitConfig {deps, args}
  createDirectoryIfMissing True dir
  LBS.writeFile (dir ++ "/unit.json") (encode config)
  traverse_ (writeWideModule dir uid modsPerUnit extDeps children') ([0 .. modsPerUnit - 1] :: [Int])

-- | Write a single module source file for wide mode.
writeWideModule :: FilePath -> Int -> Int -> Maybe ExtDepsConfig -> [Int] -> Int -> IO ()
writeWideModule dir uid modsPerUnit extDeps childUids mid = do
  let mName = moduleName uid mid
      source = wideModuleSource uid mid modsPerUnit extDeps childUids
  writeFile (dir ++ "/" ++ mName ++ ".hs") source
