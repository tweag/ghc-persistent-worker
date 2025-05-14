# Test that the make state is restored from the Buck cache when recompiling a failed module, and that the error is the
# same both times, rather than getting a unit state failure in the second attempt.
{...}: {
  build = ''
  failing_build()
  {
    exit_code 3
    output_ignore
    describe 'Run a Buck build compile errors errors'
    error_match 'undefined, called at'
    step_bb
  }
  failing_build
  failing_build
  '';
}
