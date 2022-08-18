#!/bin/bash -e

dest=$1
curr=$(readlink -f $(dirname $0))

if [[ ! $dest == */bin* ]]; then
  echo "$dest must ends with /bin"
  exit 1
fi

set -v
mkdir -p $dest
cd $dest
ln -sv $curr/scripts/* ./
