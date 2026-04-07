{ghc-debug, ...}:
{config, lib, build, ...}: let

  sharedExeOverrides = {modify, hsLibC, ...}: {
    ghc-proxy = modify hsLibC.enableSharedExecutables;
    ghc-worker = modify hsLibC.enableSharedExecutables;
  };

  envOverrides = {overrideAttrs, nodoc, ...}: {
    ghc-proxy = nodoc;
    ghc-worker = nodoc;
    buck-worker-grpc = nodoc;
    buck-worker-internal = nodoc;
    buck-worker-proto = nodoc;
    buck-worker-types = nodoc;
  };

  ipeOverrides = {ghcOptions, ...}: let
    opts = ghcOptions ["-finfo-table-map" "-fdistinct-constructor-tables"];
  in {
    ghc-proxy = opts;
    ghc-worker = opts;
    buck-worker-grpc = opts;
    buck-worker-internal = opts;
    buck-worker-proto = opts;
    buck-worker-types = opts;
    debug = opts;
  };

  buckBinOverrides = {overrideAttrs, notest, nodoc, ...}: {
    buck-worker-internal = notest;
  };

  overrides_mwb_flag = extra: {enable, ...}: let

    flags = builtins.foldl' (z: flag: enable flag z) [] extra;

  in {
    buck-worker-types = flags;
    buck-worker-internal = flags;
    ghc-worker = flags;
    ghc-server = flags;
  };

  commonOverrides = flags: [
    sharedExeOverrides
    envOverrides
    (overrides_mwb_flag flags)
  ];

  defaultEnv = extra: {
    hls.enable = lib.mkForce false;
    package-set.extends = "mwb-26-01";
    overrides = commonOverrides ["mwb" "unit-index" "downsweep-cache"] ++ [ipeOverrides] ++ extra;
  };

  mkGithub = {force, source, nodoc, ...}: {owner ? "tek", repo, rev, hash, path ? ""}:
    nodoc (force (source.sub (config.pkgs.fetchFromGitHub { inherit owner repo rev hash; }) path));

in {

  envs.ghc910 = {
    expose.scoped = true;
    overrides = [envOverrides ({notest, ...}: { ghc-worker = notest; })];
  };

  envs.dev = defaultEnv [] // {
    buildInputs = pkgs: [pkgs.zlib pkgs.snappy pkgs.protobuf build.envs.dev.toolchain.packages.proto-lens-protoc];
  };

  envs.min = defaultEnv [];

  envs.mwb-25-10 = {
    expose.scoped = true;
    package-set.extends = "mwb-25-10";
    overrides = commonOverrides ["mwb-25-10"] ++ [buckBinOverrides ipeOverrides];
    packages = [
      "ghc-proxy"
      "ghc-worker"
      "buck-proxy"
      "buck-worker-internal"
      "buck-worker-grpc"
      "buck-worker-proto"
      "buck-worker-types"
    ];
  };

  envs.mwb-26-01 = defaultEnv [buckBinOverrides] // {
    expose.scoped = true;
  };

  envs.profiled = defaultEnv [({notest, ...}: { ghc-worker = notest; ghc-server = notest; })];

  envs.profiled-25-10 = let

    overrides = {notest, ...}: {
      ghc-worker = notest;
      ghc-server = notest;
    };

  in {
    hls.enable = lib.mkForce false;
    overrides = commonOverrides ["mwb-25-10"] ++ [ipeOverrides overrides];
    package-set.extends = "mwb-25-10";
    profiling = true;
  };

  envs.cabal-build = {
    expose.shell = true;
    package-set.compiler.source = "ghc910";
    package-set.overrides = lib.mkForce [];
    package-set.extraOverrides = lib.mkForce [];
    packages = [];
    buildInputs = pkgs: [pkgs.zlib pkgs.snappy pkgs.protobuf];
  };

  envs.test-ext-deps = let

    testExtDeps = import ./test-ext-deps.nix {
      inherit (config) pkgs;
      inherit lib;
      ghc = build.envs.dev.toolchain.packages.ghc;
    };

  in defaultEnv [] // {
    expose.shell = true;
    env.resource_test_ext_deps = "${testExtDeps}";
    buildInputs = pkgs: [pkgs.zlib];
  };

  # Use GHC 9.8 for `cabal-install` and other build tools because:
  # - If we used the same GHC as the build (i.e. MWB branch), any time the GHC changes, Cabal would be rebuilt, which
  #   is time-consuming.
  # - If we used GHC 9.10 (matching the MWB version), the build GHC's libraries would be shadowed by those used to
  #   build Cabal, because the nixpkgs GHC derivation doesn't set the proper hash suffix.
  #   This is fixed in nixpkgs upstream.
  envs.hix-build-tools.package-set.compiler.source = "ghc98";

  package-sets.mwb-25-10 = {
    compiler = "mwb-25-10-ipe";
    overrides = api@{hackage, force, source, notest, ...}: let

      github = mkGithub api;

    in {
      ChasingBottoms = force;
      __all = notest;
      aeson = force (hackage "2.2.3.0" "1a9a0z6ljbck5scwkk9r9p04y9avn9vja3n063lyqgn2v1vjb1sp");
      bitwise = force;
      boring = force;
      cborg = force;
      extra = hackage "1.8.1" "0q3hyffi11dazq9n25r508spvmblx21wipfw10hfkxcazv5l1pg2";
      foldl = force;
      generic-deriving = force;
      ghc-source-gen = github {
        repo = "ghc-source-gen";
        rev = "fd010ca5229a8ff0231a0af36bd17bcf7d0c976f";
        hash = "sha256-I+SeSO/eX/jCUhtmGVljcN+FspkSEWS6WTaU9ktGurg=";
      };
      happy = notest;
      hashable = force (hackage "1.5.0.0" "1hh22f23apsjrn3h36vzw9871jqw6y4r4di1351qs5mqqabhd011");
      hedgehog = force;
      http-types = notest;
      indexed-traversable = force;
      indexed-traversable-instances = force;
      integer-conversion = force;
      integer-logarithms = hackage "1.0.4" "0yyj0g5xkm1pjkkr4smf25lpzc936df0fyc4nsj4bx145ggspx3k";
      invariant = force;
      lens = notest (hackage "5.3.5" "0cbpvsyc9nk0v6n2zcgvcjnzp7pxffnv285jdn6gldrw9pksbkpf");
      lifted-base = force;
      proto-lens-setup = github {
        repo = "proto-lens";
        rev = "901331d19c3ab90ec24e231fa69c9ed81204f73b";
        path = "proto-lens-setup";
        hash = "sha256-st+j4vK4N00xHB//b62/HPLRBUw/PRGL8bP8WECMU5U=";
      };
      scientific = force;
      semialign = force;
      strict = force;
      th-abstraction = notest (hackage "0.7.1.0" "09wr7x9bpzyrys8id1mavk9wvqhh2smxdkfwi82kpcycm7a1z7sx");
      th-compat = force;
      these = force;
      time-compat = force;
      unordered-containers = notest;
      uuid-types = force;
      witherable = force;
      zlib = force;
    };
  };

  overrides = {hackage, force, source, notest, ...}: {
    auto-update = hackage "0.2.6" "0sp25j3fcgmfr2zv1ccg1id1iynj3azinjg23g0vy1m1m7gnmkzi";
    eventlog2html = hackage "0.11.1" "0l4klmfsxmikh8x7rp7l3s5sycwq2xmqz3d1p6078pcygjkzc6fv";
    ghc-debug-brick = source.sub ghc-debug "ghc-debug-brick";
    ghc-debug-client = force (source.sub ghc-debug "client");
    ghc-debug-common = force (source.sub ghc-debug "common");
    ghc-debug-convention = force (source.sub ghc-debug "convention");
    ghc-debug-stub = source.sub ghc-debug "stub";
    grapesy = force (hackage "1.0.1" "0j7w0knclrhxc5h1vlbdpwvvpz6ixjw6flqfhdgk6xw30g7cwf5m");
    grpc-spec = notest (hackage "1.0.0" "0pgq63k6p65c5ffzxwihp8j1p731qrnda5rxrzqsylanmdmnvjb8");
    hinotify = hackage "0.4.2" "072i8d9khxwra5x05bxxm6018ga3sjf7kykxqc6km7vi01wh2h1b";
    hedgehog = hackage "1.7" "04cjnz4i1qs3v9bza8a3ry1czapwqgxazhywkjzq2rg1544gjmby";
    http-semantics = hackage "0.3.0" "0ghj37jr5bsz047p6i66ddkwc9mxkfpbw14nd54slmj1lpwn5z4a";
    http2 = force (hackage "5.3.10" "025l7sxg9jhhkhxzlhylnh2b1phdk3vml3m573lvldcy812hpvjk");
    http2-tls = force;
    network = hackage "3.2.7.0" "08frm9gm422b9aqlmmzflj0yr80ic0ip8s4gsmr0izhizzab5420";
    network-control = hackage "0.1.7" "0p46ymb8565909q2qzig02q91ch8c4zrkminvma1iizb3s2d81m8";
    network-run = hackage "0.4.4" "0c2wpm9bkizaw9sbhy9yi51m04cjlbvzdjw09s5gy74wz2pz4spw";
    proto-lens = force;
    proto-lens-protobuf-types = force;
    proto-lens-protoc = force (hackage "0.9.0.0" "18b0hz5z4cfimnbhjnhdk4lf2r0wy5aardngdhyy8aqvr62v5r62");
    proto-lens-runtime = force;
    proto-lens-setup = force;
    serialise = force;
    snappy-c = force;
    tasty-hedgehog = force;
    time-manager = hackage "0.2.2" "1ja8pimvy07b05ifkrg6q0lzs3kh0k2dmncwjdxl81199r559vf5";
    tls = hackage "2.1.6" "11rxsmwhv6g4298a0355v6flz4n6gw64qw3iha7z0ka3nv7vq4vv";
    uuid = force;
  };

  package-sets.mwb-26-01 = {
    compiler = "mwb-26-01-ipe";
  };

  envs.tools = {
    expose.scoped = true;
    packages = ["ghc-debug-brick" "eventlog2html" "hp2pretty" "ghc-events" "profiteur"];
    overrides = {force, ...}: {
      ghc-prof = force;
      profiteur = force;
    };
  };

  envs.hls-db = {
    package-set.extends = "mwb-26-01";
  };

  commands.hls.env = "hls-db";

  envs.hls = {
    package-set.extends = "mwb-26-01";

    overrides = {hackage, fast, force, unbreak, nobench, notest, source, modify, hsLibC, disable, drv, ghcOption, ...}: let

      reduce = pkg: fast (unbreak (nobench (notest pkg)));

      github = {owner ? "tek", repo, rev, hash, path ? ""}:
        reduce (source.sub (config.pkgs.fetchFromGitHub { inherit owner repo rev hash; }) path);

      rev = "d45b5400b43ec2130ce3197a322891993cb3d73f";
      hash = "sha256-BHBvrVImJAOpm4XY/XeS6Hd2ZzZOcPZuqKRpPKVHGtI=";

      hlsPackage = path:
      ghcOption "-DMWB" (reduce (github {
        repo = "haskell-language-server";
        inherit rev hash;
        inherit path;
      }));

    in {

      binary-instances = force;

      cabal-add = fast (force (hackage "0.2" "0yxh19iqspai0003p83rsnqkhq2dxa3a2vz3qfzg3k4392z1zbvi"));

      haskell-language-server =
        lib.foldl (lib.flip disable) (modify hsLibC.enableSharedExecutables (hlsPackage "")) [
          "stan" "stylishHaskell" "ormolu" "fourmolu" "hlint"
        ];
      ghcide = hlsPackage "ghcide";
      hls-graph = hlsPackage "hls-graph";
      hls-plugin-api = hlsPackage "hls-plugin-api";
      hls-test-utils = hlsPackage "hls-test-utils";

      hie-bios = github {
        repo = "hie-bios";
        rev = "6847c318cb8524f1d46d2bf02b991318253cef9b";
        hash = "sha256-rR8b2g6Req5Ssr4TtfMCNQZFqRrgG0S+pMj06KkE+q4=";
      };

      Diff = hackage "0.5" "13n231179wa9xm2933f328v00jb486w740yahz4qcbza4yv39w1i";
      directory-ospath-streaming = hackage "0.3" "0m0v200mgmkizm3l6pw9x9gvqx9xancgsal4z1pb7hi2pgrj0w0d";
      fourmolu = drv null;
      ghc-lib-parser = hackage "9.12.2.20250421" "0qxi41zr50chrr6isyfpff5kq6kqxhc5iri6a8ixvz27042a0hsq";
      ghc-lib-parser-ex = hackage "9.12.0.0" "1kxdwr1vpjn4rlhbvajdh25zjl3wyl8lli0krmdxlp03jg4p2vlx";
      hiedb = notest (hackage "0.7.0.0" "0i6szmajpg1w2mi29vs2z3brjhznivaq2his6zcz38gpyfr2dlwi");
      hlint = drv null;
      ormolu = drv null;
      stan = drv null;
      stylish-haskell = drv null;

    };

  };

}
