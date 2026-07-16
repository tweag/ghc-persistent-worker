{config, lib, util, ...}: let

  inherit (util) build;

  serverPkg = build.packages.min.ghc-server.package;
  fixedServerPkg = build.packages.mwb-26-04-fixed.ghc-server.package;
  profiledServerPkg = build.packages.profiled.ghc-server.package;
  profiledFixedPkg = build.packages.profiled-fixed.ghc-server.package;
  profiteur = build.envs.tools.toolchain.packages.profiteur;

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

    outputs.apps.profile-cache-restore = util.zapp "profile-cache-restore" ''
    depth=''${1-2}
    project=$(mktemp -d --tmpdir profile-cache-restore.XXXXXXXX)
    echo "Creating ''$(($depth * 2))-level test project at $project"
    ${fixedServerPkg}/bin/gen-project $project $depth

    ${cleanup}

    echo "Starting ghc-server for initial full build..."
    ${fixedServerPkg}/bin/ghc-server --verbose $project &
    server_pid=$!

    echo "Building all units..."
    ${fixedServerPkg}/bin/ghc-client $project --wait

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

    outputs.apps.profile-cache-meta = util.zapp "profile-cache-meta" ''
    depth=''${1-3}
    project=$(mktemp -d --tmpdir profile-cache-meta.XXXXXXXX)
    echo "Creating project with depth=$depth at $project"
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

    # Build target list: only metadata for root units
    targets="unit0:metadata unit1:metadata"

    rts_opts="''${2--p}"
    echo "Starting profiled ghc-server with RTS opts: $rts_opts"
    cd $project
    ${profiledFixedPkg}/bin/ghc-server --verbose $project +RTS $rts_opts &
    server_pid=$!
    cd -

    echo "Running metadata for root units only..."
    ${fixedServerPkg}/bin/ghc-client $project --wait $targets

    echo "Stopping profiled server..."
    kill -INT $server_pid
    wait $server_pid || true
    server_pid=

    # Copy profiling output
    for f in $project/ghc-server.prof $project/ghc-server.eventlog; do
      if [[ -f $f ]]; then
        base=$(basename $f)
        cp $f $base
        echo "Copied: $base"
        if [[ $f == *.prof ]]; then
          ${profiteur}/bin/profiteur $f
          cp ''${f%.prof}.prof.html $base.html
          echo "Rendered: $base.html"
        fi
      fi
    done
    '';

    outputs.apps.profile-test = util.app (util.zscript "profile-test" ''
    nix develop .#test-ext-deps -c cabal test ghc-worker \
      --test-option '-p "/profiling/"' \
      $@
    '');

    outputs.apps.profile-cache-wide = util.zapp "profile-cache-wide" ''
    depth=''${1-10}
    mods_per_unit=''${2-3}
    project=$(mktemp -d --tmpdir profile-cache-wide.XXXXXXXX)

    ${fixedServerPkg}/bin/gen-project --wide $project $depth $mods_per_unit

    ${cleanup}

    echo "Starting ghc-server for initial full build..."
    ${fixedServerPkg}/bin/ghc-server --verbose $project &
    server_pid=$!

    echo "Building all units..."
    ${fixedServerPkg}/bin/ghc-client $project --wait

    echo "Stopping server..."
    kill $server_pid
    wait $server_pid || true
    server_pid=

    echo "Deleting output and cache for root unit (unit1)..."
    rm -rf $project/output/unit1
    rm -rf $project/cache/unit1
    rm -rf $project/socket

    targets="unit1:metadata"

    rts_opts="''${3--p}"
    echo "Starting profiled ghc-server with RTS opts: $rts_opts"
    cd $project
    ${profiledFixedPkg}/bin/ghc-server --verbose $project +RTS $rts_opts &
    server_pid=$!
    cd -

    echo "Running metadata for root unit only..."
    ${fixedServerPkg}/bin/ghc-client $project --wait $targets

    echo "Stopping profiled server..."
    kill -INT $server_pid
    wait $server_pid || true
    server_pid=

    for f in $project/ghc-server.prof $project/ghc-server.eventlog; do
      if [[ -f $f ]]; then
        base=$(basename $f)
        cp $f $base
        echo "Copied: $base"
        if [[ $f == *.prof ]]; then
          ${profiteur}/bin/profiteur $f
          cp ''${f%.prof}.prof.html $base.html
          echo "Rendered: $base.html"
        fi
      fi
    done
    '';

    outputs.apps.profile-incremental = util.zapp "profile-incremental" ''
    num_modules=''${1-1000}
    project=$(mktemp -d --tmpdir profile-incremental.XXXXXXXX)

    export resource_test_ext_deps=${fixedExtDeps}
    ${fixedServerPkg}/bin/gen-project --flat $project $num_modules

    ${cleanup}

    echo "Starting ghc-server for initial full build..."
    ${fixedServerPkg}/bin/ghc-server --verbose $project &
    server_pid=$!

    echo "Building all units..."
    ${fixedServerPkg}/bin/ghc-client $project --wait

    echo "Stopping server..."
    kill $server_pid
    wait $server_pid || true
    server_pid=

    echo "Modifying M0 to trigger incremental metadata..."
    echo "-- modified" >> $project/unit1/M0.hs

    echo "Deleting output and cache for unit1..."
    rm -rf $project/output/unit1
    rm -rf $project/cache/unit1
    rm -rf $project/socket

    rts_opts="''${2--p}"
    echo "Starting profiled ghc-server with RTS opts: $rts_opts"
    cd $project
    ${profiledFixedPkg}/bin/ghc-server --verbose $project +RTS $rts_opts &
    server_pid=$!
    cd -

    echo "Running metadata for unit1..."
    ${fixedServerPkg}/bin/ghc-client $project --wait unit1:metadata

    echo "Stopping profiled server..."
    kill -INT $server_pid
    wait $server_pid || true
    server_pid=

    for f in $project/ghc-server.prof $project/ghc-server.eventlog; do
      if [[ -f $f ]]; then
        base=$(basename $f)
        cp $f $base
        echo "Copied: $base"
        if [[ $f == *.prof ]]; then
          ${profiteur}/bin/profiteur $f
          cp ''${f%.prof}.prof.html $base.html
          echo "Rendered: $base.html"
        fi
      fi
    done
    '';

    outputs.apps.parallel = util.zapp "parallel" ''
    num_modules=''${1-1000}
    project=$(mktemp -d --tmpdir parallel.XXXXXXXX)

    ${fixedServerPkg}/bin/gen-project flat $project $num_modules
    rm $project/unit1/M0.hs

    ${cleanup}

    echo "Starting ghc-server..."
    ${fixedServerPkg}/bin/ghc-server --verbose $project -j 80 &
    server_pid=$!

    echo "Building..."
    time ${fixedServerPkg}/bin/ghc-client $project --wait

    echo "Stopping server..."
    kill $server_pid
    wait $server_pid || true
    server_pid=
    '';

  };

}
