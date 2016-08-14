#!/bin/bash
#
# BASH helper functions for xsessionrc
#
# Requirement: xinput


############################################
# Extract a device ID from xinput output
# Arguments:
#   device_name
# Returns:
#   device_id
############################################
get_device_id() {
  local device_name=$1
  local s=$(xinput | grep "$device_name")
  s="${s#*id=}"
  local device_id="${s%%[!0-9]*}"
  echo "$device_id"
}
