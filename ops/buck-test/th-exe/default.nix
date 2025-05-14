# Test executing an external program from a splice.
{...}: {
  build = ''
  step_bb_success ''${package}:th-exe-run
  step_bb_success ''${package}:th-exe-use
  '';
}
