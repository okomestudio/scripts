#!/bin/bash
#
# BASH helper functions for xsessionrc
#
# Requirement: xinput


############################################
# Extract a device ID from xinput output
# Arguments:
#   pattern1 [pattern2 ...]
# Returns:
#   device_id
############################################
get_device_id() {
  local s=$(xinput | grep "$1")
  for pattern in "${@:1}"; do
    s=$(echo "$s" | grep "$pattern")
  done
  s="${s#*id=}"
  local device_id="${s%%[!0-9]*}"
  echo "$device_id"
}
