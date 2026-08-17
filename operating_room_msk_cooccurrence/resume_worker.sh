#!/usr/bin/env bash
set -uo pipefail

if [[ "$#" -ne 4 ]]; then
  printf 'Usage: %s PROJECT_DIR SOURCE_ROOT OUTPUT_DIR EXIT_CODE_PATH\n' "$0" >&2
  exit 64
fi

project_dir="$1"
source_root="$2"
output_dir="$3"
exit_code_path="$4"

Rscript "${project_dir}/resume_from_models.R" \
  "${source_root}" \
  "${output_dir}" \
  formal
status=$?

status_tmp="${exit_code_path}.tmp.$$"
printf '%s\n' "${status}" >"${status_tmp}"
mv "${status_tmp}" "${exit_code_path}"
exit "${status}"
