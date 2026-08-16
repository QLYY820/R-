#!/usr/bin/env bash
set -euo pipefail

project_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
run_stamp="$(date +%Y%m%d_%H%M%S)"
log_dir="${project_dir}/logs"
output_dir="${project_dir}/outputs/run_${run_stamp}_formal"
mkdir -p "${log_dir}"

log_path="${log_dir}/run_${run_stamp}.log"
pid_path="${log_dir}/run_${run_stamp}.pid"
exit_code_path="${log_dir}/run_${run_stamp}.exit_code"

nohup bash "${project_dir}/run_worker.sh" \
  "${project_dir}" \
  "${output_dir}" \
  "${exit_code_path}" \
  >"${log_path}" 2>&1 &

pid=$!
printf '%s\n' "${pid}" >"${pid_path}"
printf 'PID=%s\nLOG=%s\nOUTPUT=%s\nEXIT_CODE=%s\n' \
  "${pid}" "${log_path}" "${output_dir}" "${exit_code_path}"
