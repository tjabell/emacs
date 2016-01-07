#!/usr/bin/env bash

# Get the current directory that this script is executing inq
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
emacs --batch -l $DIR/packages.el
