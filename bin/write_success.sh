#!/bin/bash
#
# Writes a `_SUCCESS` file to an S3 directory given as a CL argument.
#
# Hadoop creates a `_SUCCESS` file in an output directory to signal
# job completion. This script is for simulating that file creation,
# mainly while debugging. For example
#
#   $ write_success.sh s3://somebucket/somedir
#
# will create an object named `_SUCCESS` at
# `s3://somebucket/somedir/_SUCCESS`
#
# Required:
#
#   $ pip install awscli
#

scriptname=$0

function usage {
  echo ""
  echo "usage: $scriptname s3_uri"
  echo ""
  echo "       s3_uri  S3 directory to which _SUCCESS file is created,"
  echo "               e.g., s3://somebucket/somedir/."
  echo ""
  exit 1
}

opt_pem=""

if [ $# -eq 1 ]; then
  :
else
  usage
fi

dest=$1

if [[ $dest == s3://* ]]; then
  tmpfile=$(mktemp /tmp/write-success.XXXXXX)

  # If the path ends with "/", do not duplicate:
  success=${dest%/}/_SUCCESS

  aws s3 cp ${tmpfile} ${success}
  
  rm $tmpfile
else
  echo "ERROR: invalid S3 path: '${dest}'"
  exit 1
fi
