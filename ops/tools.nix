{util, ...}: let

  inherit (util) build;

  serverPkg = build.packages.min.ghc-server.package;
  profiledServerPkg = build.packages.profiled.ghc-server.package;
  profiteur = build.envs.tools.toolchain.packages.profiteur;

  setupProject = ''
  project=$(mktemp -d --tmpdir ghc-server-test.XXXXXXXX)
  echo "Creating test project at $project"

  mkdir -p $project/unit0 $project/unit1

  cat > $project/unit0/A.hs <<'EOF'
  module A where
  hello :: String
  hello = "Hello from unit0"
  EOF

  cat > $project/unit1/B.hs <<'EOF'
  module B where
  import A (hello)
  greeting :: String
  greeting = "Greeting: " ++ hello
  EOF
  '';

  cleanup = ''
  cleanup() {
    setopt local_options no_err_return
    if [[ -n $server_pid ]] && kill -0 $server_pid 2>/dev/null; then
      echo "Stopping server (pid $server_pid)"
      kill $server_pid
      wait $server_pid 2>/dev/null || true
    fi
    if [[ -z $keep_project ]]
    then
      echo "Deleting project. Use 'keep_project=1 nix run ...' to keep it."
      rm -rf $project
    fi
  }
  trap cleanup EXIT
  '';

  testServerBuild = args: ''
  ${cleanup}

  server="${serverPkg}/bin/ghc-server"
  client="${serverPkg}/bin/ghc-client"

  echo "Starting ghc-server..."
  $server --verbose ${args} $project &
  server_pid=$!

  echo "Running client..."
  $client $project unit0:metadata
  $client $project unit0:modules
  $client $project unit1:metadata
  $client $project --wait unit1:modules

  echo "Restarting ghc-server..."
  kill $server_pid
  $server --verbose ${args} $project &
  server_pid=$!

  echo "Running client..."
  $client $project --wait unit1:modules
  '';

in {

  config = {

    outputs.apps.rebuild-impure-worker = util.zapp "rebuild-impure-worker" ''
    if [[ -z $1 ]]
    then
      echo "Usage: nix run .#rebuild-impure-worker GHC_BUILD_DIR [DEV_SHELL]"
      exit 1
    fi
    dir=$1
    nix develop .#''${2-cabal-build} -c cabal build -fmwb -funit-index -fdownsweep-cache $@[3,$] -w $dir/stage1/bin/ghc ghc-worker
    print 'Worker executable:'
    cabal -v0 list-bin ghc-worker
    '';

    outputs.apps.test-server = util.zapp "test-server" ''
    ${setupProject}

    cat > $project/unit0/unit.json <<'EOF'
    {
      "deps": [],
      "args": ["-package", "base"]
    }
    EOF

    cat > $project/unit1/unit.json <<'EOF'
    {
      "deps": ["unit0"],
      "args": ["-package", "base"]
    }
    EOF

    ${testServerBuild ""}
    '';

    outputs.apps.test-server-cabal = util.zapp "test-server-cabal" ''
    ${setupProject}

    cat > $project/test-project.cabal <<'EOF'
    cabal-version: 3.0
    name: test-project
    version: 0.1
    build-type: Simple

    library unit0
      hs-source-dirs: unit0
      exposed-modules: A
      build-depends: base
      default-language: GHC2021

    library unit1
      hs-source-dirs: unit1
      exposed-modules: B
      build-depends: base, test-project:unit0
      default-language: GHC2021
    EOF

    ${testServerBuild "--cabal"}
    '';

    outputs.apps.profile-cache-restore = util.zapp "profile-cache-restore" ''
    depth=''${1-2}
    project=$(mktemp -d --tmpdir profile-cache-restore.XXXXXXXX)
    echo "Creating ''$(($depth * 2))-level test project at $project"
    ${serverPkg}/bin/gen-project $project $depth

    ${cleanup}

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
    '';

    outputs.apps.profile-test = util.app (util.zscript "profile-test" ''
    nix develop .#test-ext-deps -c cabal test ghc-worker \
      --test-option '-p "/profiling/"' \
      $@
    '');

  };

}
