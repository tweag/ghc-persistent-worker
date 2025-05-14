# Test that restoring metadata from cache doesn't crash when a TH module imports from a toolchain dep.
{...}: {
  build = ''
  step_bb_success

  echo "" >> $package/Use.hs

  step_bb_success
  '';
}
