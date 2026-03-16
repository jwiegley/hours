#!/usr/bin/env bash
set -euo pipefail

find . -name '*.hs' \
    -not -path './dist-*' \
    -not -path './.git/*' \
    -not -path './org2tc/*' \
    -exec fourmolu --mode inplace {} +

echo "All Haskell files formatted."
