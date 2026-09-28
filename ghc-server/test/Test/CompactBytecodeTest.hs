{-# LANGUAGE CPP #-}

module Test.CompactBytecodeTest where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import GHC (LoadHowMuch (LoadAllTargets), getSession, load, mkModuleName, setTargets)
import GHC.ByteCode.Types (CompiledByteCode (..))
import GHC.Compact (compact, getCompact)
import GHC.Data.FlatBag (elemsFlatBag)
import GHC.Driver.DynFlags (targetProfile)
import GHC.Driver.Env (hsc_HPT, hsc_dflags)
#if defined(GBC)
import GHC.Linker.Types (LinkableWith (..), ModuleByteCode (..))
#else
import GHC.Linker.Types (Linkable (..), LinkablePart (..))
import GHC.Types.Unique.FM (UniqFM, sizeUFM)
#endif
import GHC.Unit.Home.PackageTable (lookupHpt)
import GHC.Unit.Types (stringToUnitId)
import GhcServer.Build.BytecodeMirror (homeModBytecodeLinkable, mirrorLinkable, mirrorSourceFor, mirrorUnlinkedBCO, rehydrateLinkable)
import Hedgehog (annotate, assert, failure, (===))
import Test.PackageDb (UnitSpec (..), moduleSpec)
import Test.Run (transientSession, unitTest, withTemp)
import Test.Target (fileUnitTargets, ghcOptions)
import Test.Tasty (TestTree)
import Types.State.Make (HomeByteCode)

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
        pure ((env,) <$> mhmi)
      case result >>= \ (env, hmi) -> (env, hmi,) <$> homeModBytecodeLinkable hmi of
        Nothing -> failure
        Just (env, hmi, lnk) -> do
          let cbcs = linkableByteCode lnk
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
                liftIO (try @SomeException (compact (mirrorUnlinkedBCO mempty <$> elemsFlatBag cbc.bc_bcos)))
              case mirrorResult of
                Left err -> annotate ("compacting the Name-free mirror unexpectedly failed: " <> show err) *> failure
                Right _ -> annotate "compacting the Name-free mirror succeeded"


              src <- liftIO (mirrorSourceFor (targetProfile (hsc_dflags env)) hmi)
              case mirrorLinkable src lnk of
                Nothing -> annotate "unexpected: whole-Linkable mirroring failed" *> failure
                Just mlnk -> do
                  -- Compact a String-keyed map like 'GhcServer.Build.SharedBytecode.collectBytecode' produces.
                  compacted <- liftIO (try @SomeException (compact (Map.singleton ("unit1" :: String, "M1" :: String) mlnk)))
                  case compacted of
                    Left err -> annotate ("compacting the Name-free Linkable mirror failed: " <> show err) *> failure
                    Right region -> do
                      rehydrated <-
                        liftIO (try @SomeException (traverse (rehydrateLinkable env lnk.linkableModule) (getCompact region)))
                      case rehydrated of
                        Left (err :: SomeException) ->
                          annotate ("rehydrating the Linkable mirror failed: " <> show err) *> failure
                        Right m -> do
                          let rcbcs = foldMap linkableByteCode (Map.elems m)
                              sizes :: (CompiledByteCode -> Int) -> (Int, Int)
                              sizes f = (sum (f <$> cbcs), sum (f <$> rcbcs))
                              strs = sizes (entryCount . bc_strs)
                              itbls = sizes (entryCount . bc_itbls)
                          annotate ("strs (orig, rehydrated): " <> show strs)
                          annotate ("itbls (orig, rehydrated): " <> show itbls)
                          assert (fst strs > 0 && fst itbls > 0)
                          uncurry (===) strs
                          uncurry (===) itbls
  where
    unitId = stringToUnitId "unit1"

    unitSpec =
      UnitSpec {
        name = "unit1",
        deps = [],
        modules = moduleSpec "M1" ["module M1 where", "data T = A Int | B", "m1 :: String", "m1 = \"hello\""] :| []
      }

    options = ghcOptions unitId [] ++ ["-fbyte-code-and-object-code", "-fprefer-byte-code"]


linkableByteCode :: HomeByteCode -> [CompiledByteCode]
#if defined(GBC)
linkableByteCode l = [l.linkableParts.gbc_compiled_byte_code]
#else
linkableByteCode l = [cbc | BCOs cbc <- toList l.linkableParts]
#endif

#if defined(GBC)
entryCount :: [a] -> Int
entryCount = length
#else
entryCount :: UniqFM k a -> Int
entryCount = sizeUFM
#endif
