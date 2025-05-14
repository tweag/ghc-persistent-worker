# Test that the make state is restored from the Buck cache when recompiling a changed module after that module had been
# built successfully previously.
# This assumes that the worker process was killed by Buck after the first build concluded, causing the worker in the
# second build to have an empty module graph and HPT for all packages that weren't changed.
{...}: {
  build = ''
  step_bb_success

  echo "" >> $package/L3_3.hs

  step_bb_success
  '';
}
