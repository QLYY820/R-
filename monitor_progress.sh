#!/usr/bin/env bash
set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
LOG_POINTER="$PROJECT_DIR/logs/LATEST_LOG.txt"
PID_FILE="$PROJECT_DIR/logs/latest.pid"

if [[ ! -f "$LOG_POINTER" ]]; then
  echo "No run log found. Start the analysis with: bash run_bastion_nohup.sh"
  exit 1
fi

LOG_FILE="$(cat "$LOG_POINTER")"
echo "LOG=$LOG_FILE"
if [[ -f "$PID_FILE" ]]; then
  PID="$(cat "$PID_FILE")"
  ps -p "$PID" -o pid,etime,stat,%cpu,%mem,cmd || true
fi
tail -n 100 "$LOG_FILE"
