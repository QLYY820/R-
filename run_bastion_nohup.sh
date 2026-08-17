#!/usr/bin/env bash
set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$PROJECT_DIR"

export MALE_SLEEP_XLSX="/home/sunxuexia/我的数据/data.xlsx"
export MALE_SLEEP_DATA_PATH="/home/sunxuexia/我的数据/data_text.rds"
export MALE_SLEEP_EXPECTED_N="64114"

mkdir -p logs
STAMP="$(date +%Y%m%d_%H%M%S)"
LOG_FILE="$PROJECT_DIR/logs/male_sleep_${STAMP}.log"
PID_FILE="$PROJECT_DIR/logs/latest.pid"
LOG_POINTER="$PROJECT_DIR/logs/LATEST_LOG.txt"

nohup Rscript --vanilla run_all.R >"$LOG_FILE" 2>&1 &
PID=$!
printf '%s\n' "$PID" >"$PID_FILE"
printf '%s\n' "$LOG_FILE" >"$LOG_POINTER"

echo "Background analysis started"
echo "PID=$PID"
echo "LOG=$LOG_FILE"
echo "Monitor with: tail -f \"$LOG_FILE\""
