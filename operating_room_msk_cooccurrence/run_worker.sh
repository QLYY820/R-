#!/usr/bin/env bash
set -uo pipefail

if [[ "$#" -ne 3 ]]; then
  printf 'Usage: %s PROJECT_DIR OUTPUT_DIR EXIT_CODE_PATH\n' "$0" >&2
  exit 64
fi

project_dir="$1"
output_dir="$2"
exit_code_path="$3"

Rscript "${project_dir}/run_all.R" \
  --mode=formal \
  --output-root="${output_dir}"
status=$?

status_tmp="${exit_code_path}.tmp.$$"
printf '%s\n' "${status}" >"${status_tmp}"
mv "${status_tmp}" "${exit_code_path}"
exit "${status}"
