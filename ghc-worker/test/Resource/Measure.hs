module Resource.Measure where

import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.List (intercalate)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog.Internal.Property (TestT, failWith)
import System.Environment (lookupEnv)
import Test.Resource.Stats (PhaseReference (..), PhaseResult (..), phaseSummary, rtsStatsAvailable)

-- | Check whether the environment supports running the resource test.
-- Requires both RTS stats (compiled with @-T@) and the @resource_test_ext_deps@ env var
-- (set by the @test-ext-deps@ devshell), which ensures controlled build conditions.
checkEnvironment :: IO (Maybe String)
checkEnvironment =
  rtsStatsAvailable >>= \case
    False -> pure (Just "RTS stats not available (compiled without -T?)")
    True -> lookupEnv "resource_test_ext_deps" <&> \case
      Just _ -> Nothing
      Nothing -> Just "resource_test_ext_deps not set (use the test-ext-deps devshell)"

-- | Pair each measured result with its reference by name.
pairResults :: [PhaseReference] -> [PhaseResult] -> [(PhaseReference, PhaseResult)]
pairResults refs phases =
  [(ref, result) | ref <- refs, Just result <- [lookup ref.name resultMap]]
  where
    resultMap = [(r.name, r) | r <- phases]

-- | Format the failure report with regression details and all phase deviations.
formatReport :: [String] -> [String] -> String
formatReport summaries regressions =
  intercalate "\n" $
  ["Allocation regressions detected:"]
  ++
  indent regressions
  ++
  ["", "All phases:"]
  ++
  indent summaries
  where
    indent = fmap ("  " ++)

assertMeasurements ::
  HasCallStack =>
  [PhaseReference] ->
  [PhaseResult] ->
  TestT IO ()
assertMeasurements refs results =
  withFrozenCallStack do
    unless (null regressions) do
      failWith Nothing (formatReport (fst <$> checks) regressions)
  where
    checks = uncurry phaseSummary <$> pairResults refs results
    regressions = [r | (_, Just r) <- checks]
