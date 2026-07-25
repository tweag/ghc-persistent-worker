{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Internal.Evaluate where

import Control.Concurrent (withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import GHC (
  Ghc,
  getSession,
  getSessionDynFlags,
  runTcInteractive,
  setInteractiveDynFlags,
  setSession,
  )
import GHC.Data.Bag (emptyBag)
import GHC.Driver.Env (hsc_home_unit, hscInterp, runInteractiveHsc, hscSetActiveUnitId)
import GHC.Driver.Env.Types (HscEnv (hsc_IC), hsc_mod_graph)
import GHC.Driver.Errors.Types (hoistTcRnMessage)
import GHC.Driver.Main (hscParseStmtWithLocation, ioMsgMaybe)
import GHC.Iface.Load (loadSrcInterface)
import GHC.Runtime.Context (
  InteractiveContext (..),
  InteractiveImport (..),
  )
import GHC.Runtime.Eval (
  execOptions,
  setContext,
  )
import GHC.Runtime.Eval.Types (
  IcGlobalRdrEnv (..),
  )
import GHC.Tc.Utils.Env (lookupGlobal)
import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Reader (
  GlobalRdrEltX (..),
  GlobalRdrEnvX,
  GREInfo,
  IfGlobalRdrEnv,
  Parent (NoParent),
  hydrateGlobalRdrEnv,
  plusGlobalRdrEnv,
  )
import GHC.Types.Name.Occurrence (OccName, mkOccEnv)
import GHC.Types.PkgQual (PkgQual (NoPkgQual, ThisPkg))
import GHC.Types.TyThing (tyThingGREInfo)
import GHC.Unit (moduleUnitId)
import GHC.Unit.Finder qualified as Finder
import GHC.Unit.Finder.Types (FindResult (..))
import GHC.Unit.Home (homeUnitId)
import GHC.Unit.Module.ModIface (mi_exports)
import GHC.Unit.Types (
  IsBootInterface (..),
  moduleName,
  )
import GHC.Utils.Outputable (ppr, text, (<+>))
import Internal.Cache.Hpt (loadHomeUnit)
import Internal.Log (logDebugD, logTimed)
import Language.Haskell.Syntax.Module.Name (ModuleName (..), mkModuleName)
import System.OsPath.Extra (toOsPath)
import Types.Args (Args (..))
import Types.Env (Env (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))
import Types.Target (ModuleTarget (..))


import GHC.Driver.Config (initEvalOpts)
import GHC.Driver.Env (hsc_interp, mkInteractiveHscEnv)
import GHC.Driver.Main (hscParsedStmt)
import GHC.Driver.Monad (GhcMonad)
import GHC.Hs.Extension (GhcPs)
-- import GHC.Runtime.Eval (updateFixityEnv)
import GHC.Runtime.Eval.Types (ExecOptions (..), isStep)
import GHC.Runtime.Interpreter (evalStmt, wormhole)
import GHCi.Message (EvalResult (..), EvalStatus_ (..))
import GHCi.RemoteTypes (ForeignHValue, HValueRef)
import Language.Haskell.Syntax.Expr (GhciLStmt)
import Unsafe.Coerce (unsafeCoerce)

evaluate :: Env -> Maybe String -> ModuleTarget -> [String] -> String -> Ghc Bool
evaluate env mHomeUnit target@(ModuleTarget modu) imports expr = do
  logTimed env.log "evaluate is called" do
    hsc_env0 <- GHC.getSession
    dflags0 <- GHC.getSessionDynFlags

    case mHomeUnit of
      Nothing -> logDebugD env.log (text "Nothing") >> pure False
      Just homeUnit -> do
        logDebugD env.log (text (show homeUnit))
        hsc_env2 <- liftIO $ withMVar env.state \ state -> do
          (_, hsc_env1) <-
            loadHomeUnit env.log dflags0 env.args.features (moduleUnitId target.mod) (state, hsc_env0) (toOsPath homeUnit)
          pure hsc_env1 {hsc_mod_graph = state.make.moduleGraph}
        let hsc_env = hscSetActiveUnitId (moduleUnitId target.mod) (hsc_env2)
        GHC.setSession hsc_env
        dflags <- GHC.getSessionDynFlags
        GHC.setInteractiveDynFlags dflags
        let home_unit = hsc_home_unit hsc_env
            home_unit_id = homeUnitId home_unit
            uid = moduleUnitId target.mod

        let modname = moduleName modu
            pkgqual = ThisPkg home_unit_id

        result <- liftIO do
          Finder.findImportedModule hsc_env modname pkgqual

        case result of
          Found modLoc modu -> do
            {- let unit = moduleUnit modu
            case unit of
              RealUnit (Definite uid') ->
                logDebugD env.log (text "RealUnit" <+> ppr uid')
              VirtUnit {} -> logDebugD env.log (text "VirtUnit")
              HoleUnit -> logDebugD env.log (text "HoleUnit") -}
            setContext [IIModule modname]

            for_ imports $ \imp -> do
              e <- loadImport env (mkModuleName imp)
              case e of
                Left _ -> pure ()
                Right rdr_env -> updateGlobalRdrEnv env rdr_env

            r <- evalStmtCustom expr execOptions
            case r of
              -- x :: [ForeignHValue]
              EvalComplete _ (EvalSuccess (fhv:_)) -> do
                let Just interp = hsc_interp hsc_env
                logDebugD env.log (text "eval complete")
                hv <- liftIO $ wormhole interp fhv
                logDebugD env.log (text "fhv -> hv")
                let (total, failed) = (unsafeCoerce hv :: {- IO () -} {- IO (Int, Int) -} (Int, Int))
                -- hv'' <- liftIO hv'
                -- let hv'' = hv'
                -- logDebugD env.log (text "hv = " <+> text (show hv'))
                -- let (total, failed) = hv'
                pure (failed == 0)

              _ -> logDebugD env.log (text "eval not complete") >> pure False
            {- case r of
              ExecComplete {execResult, execAllocation} -> do
                case execResult of
                  Left e -> logDebugD env.log (text "complete: left" <+> text (show e))
                  Right [] -> logDebugD env.log (text "finished, but no results?")
                  Right xs@(it : _) -> do
                    logDebugD env.log (text "complete: right:" <+> (foldr (<+>) empty (map pprName xs)))
                    logDebugD env.log (text "execAlocation = " <+> ppr execAllocation)

              ExecBreak {} -> logDebugD env.log (text "break") -}

          NoPackage _ -> logDebugD env.log (text "No Package") >> pure False
          FoundMultiple _ -> logDebugD env.log (text "Found Multiple") >> pure False
          NotFound {} -> logDebugD env.log (text "Not Found") >> pure False

loadImport :: Env -> ModuleName -> Ghc (Either String (GlobalRdrEnvX GREInfo))
loadImport env modname = do
  hsc_env <- getSession
  logDebugD env.log ("try to import" <+> ppr modname)
  result <- liftIO $ Finder.findImportedModule hsc_env modname NoPkgQual
  case result of
    Found modLoc modu -> do
      -- logDebugD env.log (text "found" <+> ppr modu)
      -- setContext [IIModule modname]
      all_env <-
            liftIO
          $ runInteractiveHsc hsc_env
          $ ioMsgMaybe $ hoistTcRnMessage $ GHC.runTcInteractive hsc_env
          $ do
            iface <- loadSrcInterface (text "imported by GHCi") (modname) NotBoot NoPkgQual
            let es :: [AvailInfo]
                es = mi_exports iface

                convert (Avail n) = Just (nameOccName n, [GRE {gre_name = n, gre_par = NoParent, gre_lcl = True, gre_imp = emptyBag, gre_info = ()}])
                convert (AvailTC _ _) = Nothing

                converted :: [(OccName, [GlobalRdrEltX ()])]
                converted = mapMaybe convert es
                exports :: IfGlobalRdrEnv
                exports = mkOccEnv converted

                get_GRE_info nm = tyThingGREInfo <$> lookupGlobal hsc_env nm
                exports_env = hydrateGlobalRdrEnv get_GRE_info exports
            pure exports_env
      pure (Right all_env)
    _ -> do
      logDebugD env.log (text "not found or error")
      pure (Left "error")

updateGlobalRdrEnv :: Env -> GlobalRdrEnvX GREInfo -> Ghc ()
updateGlobalRdrEnv env rdr_env = do
  hsc_env <- getSession
  let old_ic         = hsc_IC hsc_env
      -- this is a redefinition of replaceImportEnv, not overwriting previous context
      extendImportEnv igre import_env = igre { igre_env = new_env }
        where
          new_env = import_env `plusGlobalRdrEnv` igre_env igre
      !final_gre_cache =
        -- ic_gre_cache old_ic `replaceImportEnv` rdr_env
        ic_gre_cache old_ic `extendImportEnv` rdr_env
  setSession
    hsc_env{ hsc_IC = old_ic {ic_gre_cache = final_gre_cache}}

checkGlobalRdrEnv :: Env -> Ghc ()
checkGlobalRdrEnv env = do
  hsc_env <- getSession
  let rdr_env = igre_env (ic_gre_cache (hsc_IC hsc_env))
  logDebugD env.log (text "==== checkGlobalRdrEnv ====")
  logDebugD env.log (ppr rdr_env)

-- | Run a statement in the current interactive context.
evalStmtCustom
  :: GhcMonad m
  => String             -- ^ a statement (bind or expression)
  -> ExecOptions
  -> m (EvalStatus_ [ForeignHValue] [HValueRef]) -- ExecResult
evalStmtCustom input exec_opts@ExecOptions{..} = do
    hsc_env <- getSession

    mb_stmt <-
      liftIO $
      runInteractiveHsc hsc_env $
      hscParseStmtWithLocation execSourceFile execLineNumber input

    case mb_stmt of
      -- empty statement / comment
      -- FOR NOW
      Nothing -> return undefined -- (EvalComplete (Right []) 0)
      Just stmt -> evalStmt' stmt input exec_opts

evalStmt' :: GhcMonad m => GhciLStmt GhcPs -> String -> ExecOptions -> m (EvalStatus_ [ForeignHValue] [HValueRef])-- ExecResult
evalStmt' stmt stmt_text ExecOptions{..} = do
    hsc_env <- getSession
    let interp = hscInterp hsc_env

    -- Turn off -fwarn-unused-local-binds when running a statement, to hide
    -- warnings about the implicit bindings we introduce.
    let ic       = hsc_IC hsc_env -- use the interactive dflags
        -- FOR NOW
        -- idflags' = ic_dflags ic `wopt_unset` Opt_WarnUnusedLocalBinds
        idflags' = ic_dflags ic
        hsc_env' = mkInteractiveHscEnv (hsc_env{ hsc_IC = ic { ic_dflags = idflags' }})

    r <- liftIO $ hscParsedStmt hsc_env' stmt

    case r of
      Nothing ->
        -- empty statement / comment
        -- FOR NOW
        return undefined -- (ExecComplete (Right []) 0)
      Just (ids, hval, fix_env) -> do
        -- FOR NOW
        -- updateFixityEnv fix_env

        status <-
          -- withVirtualCWD $
            liftIO $ do
              let eval_opts = initEvalOpts idflags' (isStep execSingleStep)
              evalStmt interp eval_opts (execWrap hval)
        pure status
