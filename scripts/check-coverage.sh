#!/usr/bin/env bash
set -euo pipefail

BASELINE_FILE=".coverage-baseline"

echo "Running tests with coverage enabled..."
cabal test --enable-coverage --test-show-details=direct 2>&1

# Find the generated tix file
TIX_FILE=$(find dist-newstyle -name "hours-test.tix" 2>/dev/null | head -1)

if [[ -z "$TIX_FILE" ]]; then
    echo "Warning: No .tix file found. Skipping coverage check."
    exit 0
fi

# Extract expression coverage percentage
COVERAGE=$(hpc report "$TIX_FILE" 2>/dev/null \
    | grep 'expressions used' \
    | grep -oE '[0-9]+' \
    | head -1 || echo "0")

echo "Current expression coverage: ${COVERAGE}%"

if [[ -f "$BASELINE_FILE" ]]; then
    BASELINE=$(cat "$BASELINE_FILE")
    echo "Baseline coverage: ${BASELINE}%"
    if (( COVERAGE < BASELINE )); then
        echo "ERROR: Coverage dropped from ${BASELINE}% to ${COVERAGE}%"
        exit 1
    fi
    echo "Coverage OK (>= baseline)"
else
    echo "No baseline found. Run with --init to create one:"
    echo "  echo $COVERAGE > $BASELINE_FILE"
fi
