module Test.CompactBytecodeTest where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import GHC (LoadHowMuch (LoadAllTargets), getSession, load, mkModuleName, setTargets)
import GHC.ByteCode.Types (CompiledByteCode (..))
import GHC.Compact (compact, getCompact)
import GHC.Data.FlatBag (elemsFlatBag)
import GHC.Driver.Env (hsc_HPT)
import GHC.Linker.Types (Linkable (..), LinkablePart (..))
import GHC.Unit.Home.ModInfo (HomeModLinkable (..), hm_linkable)
import GHC.Unit.Home.PackageTable (lookupHpt)
import GHC.Unit.Types (stringToUnitId)
import GhcServer.Build.BytecodeMirror (mirrorLinkable, mirrorUnlinkedBCO, rehydrateLinkable)
import Hedgehog (annotate, failure)
import Test.PackageDb (UnitSpec (..), moduleSpec)
import Test.Run (transientSession, unitTest, withTemp)
import Test.Target (fileUnitTargets, ghcOptions)
import Test.Tasty (TestTree)

-- | Compile a single module to bytecode and object code, extract its 'HomeModLinkable' from the finished session's
-- HPT, confirm that 'compact'ing the raw 'CompiledByteCode' still fails (regression guard for the root cause
-- documented above), confirm that the 'Name'-free mirror of each 'UnlinkedBCO' compacts successfully, and confirm
-- that 'rehydrateLinkable' can reconstruct a real 'GHC.Linker.Types.Linkable' from a compacted mirror.
test_compactBytecode :: TestTree
test_compactBytecode =
  withTemp "compact-bytecode" \tmpResource ->
    unitTest "compact CompiledByteCode" do
      tmp <- liftIO tmpResource
      targets <- liftIO (fileUnitTargets tmp unitSpec)
      result <- transientSession options do
        setTargets (toList targets)
        _ <- load LoadAllTargets
        env <- getSession
        mhmi <- liftIO (lookupHpt (hsc_HPT env) (mkModuleName "M1"))
        pure ((env,) . hm_linkable <$> mhmi)
      case result of
        Nothing -> failure
        Just (_, HomeModLinkable {homeMod_bytecode = Nothing}) -> failure
        Just (env, HomeModLinkable {homeMod_bytecode = Just lnk}) -> do
          let cbcs = [cbc | BCOs cbc <- toList lnk.linkableParts]
          case cbcs of
            [] -> failure
            cbc : _ -> do
              rawResult <- liftIO (try @SomeException (compact cbc))
              case rawResult of
                Right _ ->
                  annotate "unexpected: compacting the raw CompiledByteCode succeeded (root cause may have changed)"
                    *> failure
                Left err -> annotate ("compacting raw CompiledByteCode failed as expected: " <> show err)

              mirrorResult <-
                liftIO (try @SomeException (compact (mirrorUnlinkedBCO <$> elemsFlatBag cbc.bc_bcos)))
              case mirrorResult of
                Left err -> annotate ("compacting the Name-free mirror unexpectedly failed: " <> show err) *> failure
                Right _ -> annotate "compacting the Name-free mirror succeeded"

              case mirrorLinkable lnk of
                Nothing -> annotate "unexpected: whole-Linkable mirroring failed" *> failure
                Just mlnk -> do
                  compacted <- liftIO (try @SomeException (compact mlnk))
                  case compacted of
                    Left err -> annotate ("compacting the Name-free Linkable mirror failed: " <> show err) *> failure
                    Right region -> do
                      rehydrated <-
                        liftIO (try @SomeException (rehydrateLinkable env lnk.linkableModule (getCompact region)))
                      case rehydrated of
                        Left (err :: SomeException) ->
                          annotate ("rehydrating the Linkable mirror failed: " <> show err) *> failure
                        Right _ -> annotate "rehydrating the Linkable mirror succeeded"
  where
    unitId = stringToUnitId "unit1"

    unitSpec =
      UnitSpec {
        name = "unit1",
        deps = [],
        modules = moduleSpec "M1" ["module M1 where", "m1 :: Int", "m1 = 1"] :| []
      }

    options = ghcOptions unitId [] ++ ["-fbyte-code-and-object-code", "-fprefer-byte-code"]

