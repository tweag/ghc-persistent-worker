load("@buck2-haskell//:defs.bzl", "haskell_library", "haskell_binary", "haskell_eval_test")

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

def haskell_eval(**kwargs):
    return haskell_eval_test(
        _worker = "toolchains//:persistent_worker",
        **kwargs
    )
