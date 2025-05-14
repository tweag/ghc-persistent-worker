filegroup(
    name = "nix_overlays",
    srcs = glob([
        "flake.lock",
        "**/*.nix",
        "buck-proxy/**",
        "ghc-proxy/**",
        "ghc-worker/**",
        "grpc/**",
        "instrument/**",
        "internal/**",
        "proto/**",
        "types/**",
   ]),
   visibility = ["PUBLIC"],
)
