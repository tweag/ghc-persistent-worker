# Test that the make state is restored from the Buck cache when recompiling a changed module after that module had been
# built successfully previously, for the specific case of reexporting a class from an external dependency.
{...}: {
  build = ''
  step_bb_success

  echo "" >> $package/M3.hs

  step_bb_success
  '';
}
