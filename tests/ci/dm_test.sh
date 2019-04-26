#!/bin/bash
set -e

export PATH="$HOME/.cargo/bin:$PATH"

# Set WORKSPACE to the top level directory that contains
# the devicemapper-rs git repo
if [ -z "$WORKSPACE" ]
then
    echo "Required WORKSPACE environment variable not set"
    exit 1
fi

cd $WORKSPACE

rustup default 1.31.0
cargo clean
STRATIS_DESTRUCTIVE_TEST=1 make sudo_test

