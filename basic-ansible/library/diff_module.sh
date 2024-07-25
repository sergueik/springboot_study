#!/bin/bash
# NOTE: unfinished
# WANT_JSON
# Read the variables form the file
source=`jq -r .ANSIBLE_MODULE_ARGS.source < $1`
source_type=`jq -r .ANSIBLE_MODULE_ARGS.source_type < $1`
target=`jq -r .ANSIBLE_MODULE_ARGS.target < $1`
target_type=`jq -r .ANSIBLE_MODULE_ARGS.target_type < $1`
