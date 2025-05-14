# Test that the `ghc-proxy` executable works as a replacement for vanilla GHC.
{...}: {
  build = ''
  step_bb_success "" --config=ghc-worker.enable=false
  '';
}
