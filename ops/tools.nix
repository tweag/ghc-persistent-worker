{config, lib, util, ...}: let

  inherit (util) build;

  serverPkg = build.packages.min.ghc-server.package;

  # Prebuilt ext dep packages for the mwb-26-04-linkables GHC, used by profiling apps.
  fixedGhc = build.envs.mwb-26-04-fixed.toolchain.packages.ghc;
  fixedExtDeps = import ./test-ext-deps.nix {
    inherit (config) pkgs;
    inherit lib;
    ghc = fixedGhc;
  };

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

    outputs.apps.test-incremental = util.zapp "test-incremental" ''
    project=$(mktemp -d --tmpdir ghc-server-incremental.XXXXXXXX)
    echo "Creating incremental test project at $project"

    mkdir -p $project/unit0

    # Generate 20 modules with no dependencies
    for i in $(seq 1 20); do
      cat > $project/unit0/M$i.hs <<EOF
    module M$i where
    value_$i :: Int
    value_$i = $i
    EOF
    done

    cat > $project/unit0/unit.json <<'EOF'
    {
      "deps": [],
      "args": ["-package", "base"]
    }
    EOF

    # Create the source hashes JSON with digests for all source files
    metadata_file=$project/source_hashes.json
    state_file=$project/output/unit0/build-plan.json.incremental-state.json

    compute_metadata() {
      local meta="{\"version\": 1, \"digests\": ["
      local first=true
      for f in $project/unit0/M*.hs; do
        local digest=$(sha1sum $f | cut -d" " -f1)
        if $first; then first=false; else meta="$meta, "; fi
        meta="$meta{\"path\": \"$f\", \"digest\": \"$digest:$(stat -c%s $f)\"}"
      done
      meta="$meta]}"
      echo $meta > $metadata_file
    }

    compute_metadata

    ${cleanup}

    server="${fixedServerPkg}/bin/ghc-server"
    client="${fixedServerPkg}/bin/ghc-client"

    echo "Starting ghc-server with buck_source_hashes..."
    export buck_source_hashes=$metadata_file
    $server --verbose $project &
    server_pid=$!

    echo "First build (full metadata)..."
    $client $project --wait

    echo "Verifying state file was written..."
    if [[ ! -f $state_file ]]; then
      echo "ERROR: Incremental state file not found at $state_file"
      exit 1
    fi
    echo "State file written successfully."

    echo "Modifying M1..M10 to import M11..."
    for i in $(seq 1 10); do
      cat > $project/unit0/M$i.hs <<EOF
    module M$i where
    import M11 (value_11)
    value_$i :: Int
    value_$i = $i + value_11
    EOF
    done

    echo "Updating source hashes..."
    compute_metadata

    echo "Stopping server and clearing for rebuild..."
    kill $server_pid
    wait $server_pid 2>/dev/null || true
    server_pid=
    # Delete compilation artifacts but keep build-plan.json and incremental state
    find $project/output/unit0 \( -name '*.dyn_o' -o -name '*.dyn_hi' \) -delete
    rm -rf $project/socket
    # Delete cached_unit.json so metadata re-runs (not skipped by scheduler)
    rm -f $project/cache/unit0/cached_unit.json

    echo "Second build (incremental metadata after restart)..."
    $server --verbose $project &
    server_pid=$!
    # Only run metadata, don't compile (to avoid scheduler ordering issues)
    $client $project --wait unit0:metadata

    echo "Verifying incremental metadata produced correct graph..."
    # Check that M1 now depends on M11 in the module graph
    m1_deps=$(nix run nixpkgs#jq -- -r '.module_graph.M1[]' $project/output/unit0/build-plan.json 2>/dev/null)
    if [[ $m1_deps != *"M11"* ]]; then
      echo "ERROR: M1 should depend on M11 but module_graph.M1 = $m1_deps"
      exit 1
    fi
    # Check that M11 has no deps (unchanged)
    m11_deps=$(nix run nixpkgs#jq -- -r '.module_graph.M11 | length' $project/output/unit0/build-plan.json 2>/dev/null)
    if [[ $m11_deps != "0" ]]; then
      echo "ERROR: M11 should have no deps but has $m11_deps"
      exit 1
    fi
    echo "Module graph is correct: M1 depends on M11."

    echo "SUCCESS: Incremental metadata test passed."
    '';

    outputs.apps.test-incremental-wide = util.zapp "test-incremental-wide" ''
    depth=''${1-3}
    mods_per_unit=''${2-5}
    project=$(mktemp -d --tmpdir ghc-server-incr-wide.XXXXXXXX)
    echo "Creating wide project: depth=$depth, mods=$mods_per_unit at $project"

    export resource_test_ext_deps=${fixedExtDeps}
    ${serverPkg}/bin/gen-project --wide $project $depth $mods_per_unit

    # Compute buck_source_hashes JSON from unit1 sources
    metadata_file=$project/source_hashes.json
    compute_metadata() {
      local meta="{\"version\": 1, \"digests\": ["
      local first=true
      for f in $project/unit1/*.hs; do
        local digest=$(sha1sum $f | cut -d" " -f1)
        if $first; then first=false; else meta="$meta, "; fi
        meta="$meta{\"path\": \"$f\", \"digest\": \"$digest:$(stat -c%s $f)\"}"
      done
      meta="$meta]}"
      echo $meta > $metadata_file
    }

    compute_metadata

    ${cleanup}

    echo "Starting ghc-server with buck_source_hashes for initial build..."
    export buck_source_hashes=$metadata_file
    ${serverPkg}/bin/ghc-server --verbose $project &
    server_pid=$!

    echo "Full initial build..."
    ${serverPkg}/bin/ghc-client $project --wait

    echo "Stopping server..."
    kill $server_pid
    wait $server_pid || true
    server_pid=

    echo "Modifying unit1/U1M0.hs..."
    echo "-- modified" >> $project/unit1/U1M0.hs
    compute_metadata

    # Keep build-plan.json and incremental state, delete everything else for unit1
    rm -rf $project/socket
    find $project/output/unit1 \( -name '*.dyn_o' -o -name '*.dyn_hi' \) -delete
    rm -f $project/cache/unit1/cached_unit.json

    echo "Starting server for incremental metadata rebuild..."
    ${serverPkg}/bin/ghc-server --verbose $project 2>$project/server_stderr.log &
    server_pid=$!

    echo "Running incremental metadata for unit1..."
    ${serverPkg}/bin/ghc-client $project --wait unit1:metadata

    echo "Server stderr:"
    cat $project/server_stderr.log

    echo "Test completed."
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

    outputs.apps.profile-test = util.app (util.zscript "profile-test" ''
    nix develop .#test-ext-deps -c cabal test ghc-worker \
      --test-option '-p "/profiling/"' \
      $@
    '');

  };

}
