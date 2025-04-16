{util, ...}: let

  inherit (util) build;

  serverPkg = build.packages.min.ghc-server.package;

in {

  config = {

    outputs.apps.rebuild-impure-worker = util.app (util.zscript "rebuild-impure-worker" ''
    if [[ -z $1 ]]
    then
      echo "Usage: nix run .#rebuild-impure-worker GHC_BUILD_DIR [DEV_SHELL]"
      exit 1
    fi
    dir=$1
    nix develop .#''${2-cabal-build} -c cabal build -fmwb -funit-index -fdownsweep-cache $@[3,$] -w $dir/stage1/bin/ghc ghc-worker
    print 'Worker executable:'
    cabal -v0 list-bin ghc-worker
    '');

    outputs.apps.test-server = util.app (util.zscript "test-server" ''
    project=/tmp/ghc-server-test-$$
    echo "Creating test project at $project"

    mkdir -p $project/unit0 $project/unit1

    cat > $project/unit0/unit.json <<'EOF'
    {
      "deps": [],
      "args": ["-package", "base"]
    }
    EOF

    cat > $project/unit0/A.hs <<'EOF'
    module A where
    hello :: String
    hello = "Hello from unit0"
    EOF

    cat > $project/unit1/unit.json <<'EOF'
    {
      "deps": ["unit0"],
      "args": ["-package", "base"]
    }
    EOF

    cat > $project/unit1/B.hs <<'EOF'
    module B where
    import A (hello)
    greeting :: String
    greeting = "Greeting: " ++ hello
    EOF

    cleanup() {
      if [[ -n $server_pid ]] && kill -0 $server_pid 2>/dev/null; then
        echo "Stopping server (pid $server_pid)"
        kill $server_pid
        wait $server_pid 2>/dev/null
      fi
      rm -rf $project
    }
    trap cleanup EXIT

    echo "Starting ghc-server..."
    ${serverPkg}/bin/ghc-server --verbose $project &
    server_pid=$!

    echo "Running client..."
    ${serverPkg}/bin/ghc-client $project unit0:metadata
    ${serverPkg}/bin/ghc-client $project unit0:modules
    ${serverPkg}/bin/ghc-client $project unit1:metadata
    ${serverPkg}/bin/ghc-client $project --wait unit1:modules

    echo "Restarting ghc-server..."
    kill $server_pid
    ${serverPkg}/bin/ghc-server --verbose $project &
    server_pid=$!

    echo "Running client..."
    ${serverPkg}/bin/ghc-client $project --wait unit1:modules
    '');

    outputs.apps.profile-test = util.app (util.zscript "profile-test" ''
    nix develop .#test-ext-deps -c cabal test buck-worker-internal \
      --test-option '-p "/profiling/"' \
      $@
    '');

  };

}
