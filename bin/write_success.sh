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

dest=$1

if [[ $dest == s3://* ]]; then
  tmpfile=$(mktemp /tmp/write-success.XXXXXX)

  # If the path ends with "/", do not duplicate:
  success=${dest%/}/_SUCCESS

  aws s3 cp ${tmpfile} ${success}
  
  rm $tmpfile
else
  echo "ERROR: invalid S3 path: '${dest}'"
fi
