-- | Tests for the parent-side executor bytecode planning in 'GhcWorker.Executor.Plan'.
module Test.ExecutorPlanTest where

import GHC.Fingerprint (Fingerprint, fingerprintString)
import GHC.Unit.Types (stringToUnitId)
import GhcWorker.Executor.Plan (applyDelta, dependencyOrder, planDelta, showHomeModuleId)
import Types.ExecutorPlan (Payload (..), PlanDelta (..), PlannedModule (..))
import Hedgehog ((===))
import Language.Haskell.Syntax.Module.Name (mkModuleName)
import Test.Run (unitTest)
import Test.Tasty (TestTree, testGroup)
import Types.State.Executor (ExecutorLoaded (..), HomeModuleId, emptyExecutorLoaded)

modId :: String -> HomeModuleId
modId name = (stringToUnitId "unit1", mkModuleName name)

hashOf :: String -> Fingerprint
hashOf = fingerprintString

planned :: String -> String -> [String] -> PlannedModule
planned name content deps =
  PlannedModule {home = modId name, hash = hashOf content, deps = modId <$> deps, payload = PayloadFile (name ++ ".gbc")}

-- | @A <- B <- C@, all at their initial content.
closureAbc :: [PlannedModule]
closureAbc = [planned "A" "a1" [], planned "B" "b1" ["A"], planned "C" "c1" ["B"]]

names :: [String] -> [String]
names = fmap (showHomeModuleId . modId)

sentNames :: PlanDelta -> [String]
sentNames delta = showHomeModuleId . (.home) <$> delta.send

invalidatedNames :: PlanDelta -> [String]
invalidatedNames delta = showHomeModuleId <$> delta.invalidated

test_dependencyOrder :: TestTree
test_dependencyOrder =
  unitTest "dependency order of a diamond with a cycle" do
    let deps = \case
          "D" -> ["B", "C"]
          "B" -> ["A"]
          "C" -> ["A", "D"]
          _ -> []
    dependencyOrder deps ["D"] === ["A", "B", "C", "D" :: String]

test_initialDelta :: TestTree
test_initialDelta =
  unitTest "a fresh executor receives the whole closure" do
    let delta = planDelta emptyExecutorLoaded closureAbc
    sentNames delta === names ["A", "B", "C"]
    invalidatedNames delta === []
    (applyDelta delta emptyExecutorLoaded).generation === 1

test_sharedDelta :: TestTree
test_sharedDelta =
  unitTest "a second execution only receives missing modules" do
    let loaded = applyDelta (planDelta emptyExecutorLoaded (take 2 closureAbc)) emptyExecutorLoaded
        delta = planDelta loaded (closureAbc ++ [planned "E" "e1" ["A"]])
    sentNames delta === names ["C", "E"]
    sentNames (planDelta (applyDelta delta loaded) closureAbc) === []

test_replacedDelta :: TestTree
test_replacedDelta =
  unitTest "a changed module is resent with its loaded dependents" do
    let loaded = applyDelta (planDelta emptyExecutorLoaded closureAbc) emptyExecutorLoaded
        changed = [planned "A" "a2" [], planned "B" "b1" ["A"]]
        delta = planDelta loaded changed
        loaded' = applyDelta delta loaded
    sentNames delta === names ["A", "B"]
    invalidatedNames delta === names ["C"]
    sentNames (planDelta loaded' (changed ++ [planned "C" "c1" ["B"]])) === names ["C"]

test_executorPlan :: TestTree
test_executorPlan =
  testGroup "executor plan" [
    test_dependencyOrder,
    test_initialDelta,
    test_sharedDelta,
    test_replacedDelta
  ]
