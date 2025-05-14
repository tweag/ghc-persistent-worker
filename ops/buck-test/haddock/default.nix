{...}: {
  build = ''
  exit_code 3
  output_ignore
  error_ignore
  describe 'Generate haddocks TODO known failure'
  step_bb "''${package}:haddock[haddock]" --config ghc-worker.env=ghc914
  '';
}
