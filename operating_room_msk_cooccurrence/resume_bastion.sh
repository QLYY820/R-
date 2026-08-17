#!/usr/bin/env bash
set -euo pipefail

if [[ "$#" -ne 1 ]]; then
  printf 'Usage: %s SOURCE_INTERRUPTED_RUN_ROOT\n' "$0" >&2
  exit 64
fi

project_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source_root="$1"
run_stamp="$(date +%Y%m%d_%H%M%S)"
log_dir="${project_dir}/logs"
output_dir="${project_dir}/outputs/resume_${run_stamp}_formal"
mkdir -p "${log_dir}"

log_path="${log_dir}/resume_${run_stamp}.log"
pid_path="${log_dir}/resume_${run_stamp}.pid"
exit_code_path="${log_dir}/resume_${run_stamp}.exit_code"

nohup bash "${project_dir}/resume_worker.sh" \
  "${project_dir}" \
  "${source_root}" \
  "${output_dir}" \
  "${exit_code_path}" \
  >"${log_path}" 2>&1 &

pid=$!
printf '%s\n' "${pid}" >"${pid_path}"
printf 'PID=%s\nLOG=%s\nOUTPUT=%s\nEXIT_CODE=%s\n' \
  "${pid}" "${log_path}" "${output_dir}" "${exit_code_path}"
