#!/usr/bin/env bash
set -euo pipefail

BASELINE_FILE=".perf-baseline"
ITERATIONS=5

echo "Building with optimizations..."
cabal build all -O2 2>&1

# We need test data to benchmark against
if [[ ! -f "test/data/ideal.json" ]] || [[ ! -f "test/data/real.json" ]]; then
    echo "No test data found in test/data/. Skipping performance check."
    echo "Generate test data to enable:"
    echo "  mkdir -p test/data"
    echo "  cabal run work-periods -- --there > test/data/ideal.json"
    echo "  # create test/data/real.json with sample timeclock data"
    exit 0
fi

# Average over several runs
TOTAL=0
for _ in $(seq 1 $ITERATIONS); do
    START=$(python3 -c 'import time; print(int(time.time() * 1000))')
    cabal run process-hours -- \
        --ideal test/data/ideal.json \
        --real test/data/real.json > /dev/null 2>&1
    END=$(python3 -c 'import time; print(int(time.time() * 1000))')
    DURATION=$((END - START))
    TOTAL=$((TOTAL + DURATION))
done

AVG=$((TOTAL / ITERATIONS))
echo "Average execution time: ${AVG}ms (over ${ITERATIONS} runs)"

if [[ -f "$BASELINE_FILE" ]]; then
    BASELINE=$(cat "$BASELINE_FILE")
    THRESHOLD=$(( BASELINE * 105 / 100 ))
    echo "Baseline: ${BASELINE}ms (5% threshold: ${THRESHOLD}ms)"
    if (( AVG > THRESHOLD )); then
        echo "ERROR: Performance regression! ${AVG}ms > ${THRESHOLD}ms"
        exit 1
    fi
    echo "Performance OK (<= 5% regression)"
else
    echo "No baseline found. Run with --init to create one:"
    echo "  echo $AVG > $BASELINE_FILE"
fi
