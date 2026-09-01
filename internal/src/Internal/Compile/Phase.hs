module Internal.Compile.Phase where

import Control.Exception (finally)
import Data.Foldable (for_)
import GHC (ModSummary (..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Env.Types (Hsc (..))
import GHC.Driver.Hooks (Hooks (..))
import GHC.Driver.Main (hscParse', hscTypecheckRename)
import GHC.Driver.Pipeline (runPhase)
import GHC.Driver.Pipeline.Phases (PhaseHook (..), TPhase (..))
import GHC.Tc.Types (FrontendResult (..))
import Types.Api (Event (..), Events (..), targetFromWorkerSpec)
import Types.Target (TargetSpec (..))

phaseLabel :: TPhase a -> Maybe String
phaseLabel = \case
  T_HsPp {} -> Just "cpp"
  T_Hsc {} -> Just "main"
  T_HscPostTc {} -> Just "simplify"
  T_HscBackend {} -> Just "backend"
  _ -> Nothing

-- | Wrap an action with phase event emissions.
withPhase ::
  Events ->
  TargetSpec ->
  String ->
  IO a ->
  IO a
withPhase events spec phase act = do
  for_ uiTarget \ target ->
    events.publish \ requestId -> PhaseStart {target, phase, requestId}
  startNs <- getMonotonicTimeNSec
  result <- finally act do
    endNs <- getMonotonicTimeNSec
    for_ uiTarget \ target ->
      events.publish \ requestId ->
        PhaseEnd {target, durationMs = fromIntegral ((endNs - startNs) `div` 1_000_000), requestId}
  pure result
  where
    uiTarget = targetFromWorkerSpec spec

-- | Install a 'PhaseHook' on the given 'HscEnv' that send phase events for each phase matched by 'phaseLabel'.
withPhaseHook :: Events -> TargetSpec -> HscEnv -> HscEnv
withPhaseHook events target hsc_env =
  hsc_env {hsc_hooks = (hsc_hooks hsc_env) {runPhaseHook = Just (PhaseHook run)}}
  where
    run :: forall a . TPhase a -> IO a
    run tPhase =
      wrap tPhase (runPhase tPhase)

    wrap :: forall a . TPhase a -> IO a -> IO a
    wrap tPhase =
      maybe id (withPhase events target) (phaseLabel tPhase)

unliftHsc :: (forall a . IO a -> IO a) -> Hsc b -> Hsc b
unliftHsc f (Hsc hsc) =
  Hsc \ e m -> f (hsc e m)

-- | Replacement for GHC's default frontend that emits phase events.
parseAndTypecheck :: Events -> TargetSpec -> ModSummary -> Hsc FrontendResult
parseAndTypecheck events target mod_summary = do
  hpm <- phase "parse" (hscParse' mod_summary)
  FrontendTypecheck <$> phase "typecheck" (typecheck hpm)
  where
    phase :: String -> Hsc a -> Hsc a
    phase name = unliftHsc (withPhase events target name)

    typecheck hpm =
      Hsc \ hsc_env msgs -> do
        (tc_result, _) <- hscTypecheckRename hsc_env mod_summary hpm
        pure (tc_result, msgs)

withFrontendHook :: Events -> TargetSpec -> HscEnv -> HscEnv
withFrontendHook events target hsc_env =
  hsc_env {hsc_hooks = (hsc_hooks hsc_env) {hscFrontendHook = Just (parseAndTypecheck events target)}}

withPhaseEvents :: Events -> TargetSpec -> HscEnv -> HscEnv
withPhaseEvents events target hsc_env =
  withFrontendHook events target (withPhaseHook events target hsc_env)
