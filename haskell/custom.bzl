load("@buck2-haskell//:defs.bzl", "haskell_library", "haskell_binary")

def haskell_lib(**kwargs):
    return haskell_library(
        _worker = "toolchains//:persistent_worker",
        **kwargs
    )

def haskell_bin(**kwargs):
    return haskell_binary(
        _worker = "toolchains//:persistent_worker",
        **kwargs
    )
