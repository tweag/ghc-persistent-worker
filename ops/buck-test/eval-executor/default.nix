# Test running `haskell_eval` targets in persistent executor subprocesses via `buck test`: the tests of one unit share an
# executor (`executor` attribute), and each test records the process ID of the process it runs in.
# Since the stdio of the tests isn't forwarded by Buck, they append their reports to a file as well.
{...}: {
  build = ''
  log=/tmp/buck-eval-executor.log
  rm -f $log


  # Field `$3` of the report of test `$2` in unit `$1`.
  field()
  {
    rg "unit=''${1:u} test=$2 " $log | rg -o " $3=[0-9]+" | cut -d= -f2
  }

  check_processes()
  {
    local u
    typeset -A pids
    for u in a b c
    do
      pids[$u]=$(field $u one pid)
      [[ -n $pids[$u] ]] || { echo "No report for unit $u" >&2; return 1 }
      [[ $pids[$u] == $(field $u two pid) ]] || { echo "Unit $u: tests ran in different processes" >&2; return 1 }
      [[ $(( $(field $u one count) + $(field $u two count) )) == 3 ]] || { echo "Unit $u: state wasn't shared" >&2; return 1 }
      [[ $(field $u one ppid) != $pids[$u] ]] || { echo "Unit $u: no parent" >&2; return 1 }
    done
    [[ $(print -l $pids | sort -u | wc -l) == 3 ]] || { echo "Units share processes: $pids" >&2; return 1 }
    [[ $(for u in a b c; do field $u one ppid; done | sort -u | wc -l) == 1 ]] || { echo "Different parents" >&2; return 1 }
    echo "Executor processes: $pids"
  }

  describe "Run all Buck tests in executor subprocesses"
  output_ignore
  error_ignore
  step buck test -v 2,stderr //''${package}/...
  describe 'Tests of a unit share a process, which is neither shared between units nor the worker'
  output_ignore
  error_ignore
  step check_processes
  '';
}
