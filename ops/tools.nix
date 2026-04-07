{util, ...}: let

  inherit (util) build;

  serverPkg = build.packages.min.ghc-server.package;
  profiledServerPkg = build.packages.profiled.ghc-server.package;
  profiteur = build.envs.tools.toolchain.packages.profiteur;

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

    outputs.apps.profile-cache-restore = util.app (util.zscript "profile-cache-restore" ''
    depth=''${1-2}
    project=/tmp/ghc-server-profile-$$
    echo "Creating ''$(($depth * 2))-level test project at $project"
    ${serverPkg}/bin/gen-project $project $depth

    cleanup() {
      if [[ -n $server_pid ]] && kill -0 $server_pid 2>/dev/null; then
        kill $server_pid
        wait $server_pid 2>/dev/null
      fi
      if [[ -z $keep_project ]]
      then
        rm -rf $project
      fi
    }
    trap cleanup EXIT

    echo "Starting ghc-server for initial full build..."
    ${serverPkg}/bin/ghc-server --verbose $project &
    server_pid=$!

    echo "Building all units..."
    ${serverPkg}/bin/ghc-client $project --wait

    echo "Stopping server..."
    kill $server_pid
    wait $server_pid || true
    server_pid=

    echo "Deleting output and cache for root units (unit0, unit1)..."
    rm -rf $project/output/unit0 $project/output/unit1
    rm -rf $project/cache/unit0 $project/cache/unit1
    rm -rf $project/socket

    echo "Starting profiled ghc-server..."
    cd $project
    ${profiledServerPkg}/bin/ghc-server --verbose $project +RTS -p &
    server_pid=$!
    cd -

    echo "Building root units..."
    ${serverPkg}/bin/ghc-client $project --wait unit0 unit1

    echo "Stopping profiled server..."
    kill -INT $server_pid
    wait $server_pid || true
    server_pid=

    prof_file=$project/ghc-server.prof
    if [[ ! -f $prof_file ]]; then
      echo "Error: profile output not found at $prof_file"
      exit 1
    fi

    echo "Rendering profile with profiteur..."
    ${profiteur}/bin/profiteur $prof_file
    html_file=''${prof_file%.prof}.prof.html
    cp $prof_file ghc-server.prof
    cp $html_file ghc-server.prof.html
    echo "Profile saved: ghc-server.prof, ghc-server.prof.html"
    '');

    outputs.apps.profile-test = util.app (util.zscript "profile-test" ''
    nix develop .#test-ext-deps -c cabal test buck-worker-internal \
      --test-option '-p "/profiling/"' \
      $@
    '');

  };

}
