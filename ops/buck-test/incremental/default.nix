# Test that incremental metadata works: after a successful build, modifying a source file
# triggers an incremental metadata computation that only downsweeps changed modules.
{...}: {
  build = ''
  step_bb_success

  echo "-- modified" >> $package/M2.hs

  step_bb_success
  '';
}
