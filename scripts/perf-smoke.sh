#!/usr/bin/env bash
set -euo pipefail

# Bounded MegaStrike development performance test.
# Usage: bash scripts/perf-smoke.sh [seconds] (default 45; 10..90)
cd "$(dirname "$0")/.."
duration="${1:-45}"
if [[ ! "$duration" =~ ^[0-9]+$ ]] || (( duration < 10 || duration > 90 )); then
  echo 'Duration must be an integer from 10 to 90 seconds.' >&2
  exit 2
fi
export HOST_UID="$(id -u)" HOST_GID="$(id -g)" HOST_HOME="$HOME"
export XAUTHORITY_FILE="${XAUTHORITY:-$HOME/.Xauthority}"
export DISPLAY="${DISPLAY:-:0}"
name="megastrike-perf-$(date +%s)-$$"
logdir="${PERF_LOG_DIR:-./perf-logs}"
mkdir -p "$logdir"
logdir="$(cd "$logdir" && pwd)"
compose=(docker compose -f compose.yaml -f compose.perf.yaml)
launcher_pid=""
cleanup() {
  trap - EXIT INT TERM
  docker stop -t 2 "$name" >/dev/null 2>&1 || true
  if [[ -n "$launcher_pid" ]]; then
    kill "$launcher_pid" >/dev/null 2>&1 || true
    wait "$launcher_pid" >/dev/null 2>&1 || true
  fi
  docker rm -f "$name" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM
printf 'timestamp,cpu,memory,memory_pct,pids\n' > "$logdir/stats.csv"
# Capture stdout/stderr as the process runs so logs survive early container exit.
"${compose[@]}" run --no-deps -T --name "$name" megastrike clojure -M:run > "$logdir/application.log" 2>&1 &
launcher_pid=$!
printf 'Running at most %s seconds; logs in %s\n' "$duration" "$logdir"
printf 'Ctrl+C aborts the test. CPU is limited by compose.perf.yaml.\n'
start="$(date +%s)"
seen=0
while (( $(date +%s) - start < duration )); do
  if ! kill -0 "$launcher_pid" 2>/dev/null; then
    break
  fi
  state="$(docker inspect --format '{{.State.Running}}' "$name" 2>/dev/null || true)"
  if [[ "$state" == true ]]; then
    seen=1
    sample="$(docker stats --no-stream --format '{{.CPUPerc}},{{.MemUsage}},{{.MemPerc}},{{.PIDs}}' "$name" 2>/dev/null || true)"
    [[ -n "$sample" ]] && printf '%s,%s\n' "$(date -Is)" "$sample" >> "$logdir/stats.csv"
  elif (( seen )); then
    break
  fi
  sleep 2
done
printf 'Saved %s/stats.csv and %s/application.log\n' "$logdir" "$logdir"
