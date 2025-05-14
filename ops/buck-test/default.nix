{hix, util}:
name:
{script ? null, build ? "step_bb_success"}: let

  inherit (util) pkgs;

  default = cmds: ''

  dir=$(mktemp -d --tmpdir=$PWD buck-${name}-XXX)
  package=''${dir##*/}

  cleanup()
  {
    rm -rf $dir
    buck clean &> /dev/null
  }
  trap cleanup EXIT
  trap cleanup INT

  step_bb()
  {
    local target=''${1:-$package/...}
    step buck build -v 2,stderr //$target $*[2,$]
  }

  step_bb_success()
  {
    output_ignore
    describe 'Run a Buck build without errors'
    error_match 'BUILD SUCCEEDED'
    step_bb "$@"
  }

  mkdir -p $dir
  test_base=$dir
  ${pkgs.rsync}/bin/rsync -rlt ops/buck-test/${name}/project/ $dir/
  sed -i "s#ops/buck-test/${name}/project/#$package/#g" $dir/**/BUCK

  source "${hix}/test/internal/step.zsh"

  setopt no_err_exit
  export hix_test_verbose=1
  export hix_test_full_output=1

  buck clean &> /dev/null

  () {
    setopt local_options err_return
    ${cmds}
  }
  exit $?
  '';

  test =
    if script != null
    then script
    else default build
    ;

in util.hixScript "buck-test-${name}" { path = [pkgs.ansifilter pkgs.gnused pkgs.ripgrep]; } test
