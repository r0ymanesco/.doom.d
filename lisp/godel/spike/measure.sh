#!/usr/bin/env bash
# Measure Emacs (server godel-doom) CPU, emacsclient latency and timer lag over 8s.
E="emacsclient -s godel-doom --eval"
pid=$($E '(emacs-pid)')
tick=$(getconf CLK_TCK)
cpu() { awk '{print $14+$15}' /proc/$pid/stat; }
$E '(godel-spike-measure-lag 8)' >/dev/null
c0=$(cpu); t0=$(date +%s.%N)
lat=$(python3 -I - <<'PY'
import subprocess, time, statistics
xs = []
for _ in range(15):
    t = time.time(); subprocess.run(["emacsclient", "-s", "godel-doom", "--eval", "(+ 1 1)"], capture_output=True); xs.append((time.time() - t) * 1000); time.sleep(0.45)
print("emacsclient ms: median %.0f max %.0f" % (statistics.median(xs), max(xs)))
PY
)
c1=$(cpu); t1=$(date +%s.%N)
python3 -I -c "print('emacs cpu: %.0f%%' % (100*($c1-$c0)/$tick/($t1-$t0)))"
echo "$lat"
$E '(godel-spike-lag-summary)'
