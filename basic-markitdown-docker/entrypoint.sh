#!/bin/sh
# Always enable plugins.
# Also pass --llm-client and --llm-model only when both OPENAI_API_KEY and LLM_MODEL are set.

set -- --use-plugins "$@"

if [ -n "${OPENAI_API_KEY}" ] && [ -n "${LLM_MODEL}" ]; then
    set -- --llm-client openai --llm-model "${LLM_MODEL}" "$@"
fi

# Parse args to find the input file and output file (-o).
input_file=""
output_file=""
next_is_output=0
for arg in "$@"; do
    if [ "${next_is_output}" = "1" ]; then
        output_file="${arg}"
        next_is_output=0
        continue
    fi
    case "${arg}" in
        -o) next_is_output=1 ;;
        --use-plugins|--list-plugins|--llm-client|--llm-model) ;;
        -*) ;;
        *)  [ -z "${input_file}" ] && input_file="${arg}" ;;
    esac
done

markitdown "$@"

# After conversion, if an output file was written and the input is a local file,
# match the output's ownership and permissions to the input.
if [ -n "${output_file}" ] && [ -f "${output_file}" ] && [ -f "${input_file}" ]; then
    chown "$(stat -c '%u:%g' "${input_file}")" "${output_file}"
    chmod "$(stat -c '%a' "${input_file}")" "${output_file}"
fi
