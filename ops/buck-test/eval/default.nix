# Test running `haskell_eval` targets with the worker's eval mode via `buck test`: `IO (Int, Int)` results with and
# without failure counting, `IO ()` results, and failing evaluations (nonzero failure count, exception).
{...}: {
  build = ''
  passing()
  {
    output_ignore
    error_ignore
    describe "Run Buck test $1 using worker evaluation"
    step buck test -v 2,stderr //''${package}:$1
  }
  failing()
  {
    exit_code 32
    output_ignore
    error_ignore
    describe "Run failing Buck test $1 using worker evaluation"
    step buck test -v 2,stderr //''${package}:$1
  }
  passing eval
  passing eval_unit
  passing eval_plain
  failing eval_fail
  failing eval_throw
  '';
}
