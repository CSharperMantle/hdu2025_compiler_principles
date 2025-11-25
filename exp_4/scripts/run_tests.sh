#!/bin/sh

if [ "$#" -ne 1 ]; then
    echo "usage: [EXEC=<executable>] $0 <test_spec.tsv>"
    exit 1
fi

spec_file="$1"
total=0
n_passed=0

EXEC="${EXEC:=dune exec -- exp_4}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
RESET_COLOR='\033[0m'

echo "Reading tests from $spec_file..."
while IFS="$(printf '\t')" read -r mode input_file expected_status expected_msg occurrence || [ -n "$mode" ]; do
    # Skip empty lines or comment
    [ -z "$mode" ] || [ "$mode" = '#' ] && continue

    expected_status=$(echo "$expected_status" | tr -d '\r')
    expected_msg=$(echo "$expected_msg" | tr -d '\r')
    occurrence=$(echo "$occurrence" | tr -d '\r')

    total=$((total + 1))
    
    cmd="$EXEC $mode $input_file"
    output=$( $cmd 2>&1 < /dev/null )
    exit_code=$?
    
    passed=0
    reason=""
    if [ "$expected_status" = "success" ]; then
        if [ $exit_code -eq 0 ]; then
            passed=1
        else
            reason="Expected success (0), got exit code $exit_code"
        fi
    elif [ "$expected_status" = "failure" ]; then
        if [ $exit_code -ne 0 ]; then
            if [ -n "$expected_msg" ]; then
                if [ -n "$occurrence" ]; then
                    count=$(echo "$output" | grep -c "$expected_msg")
                    if [ "$count" -eq "$occurrence" ]; then
                        passed=1
                    else
                        reason="Expected error message '$expected_msg' to occur $occurrence times, found $count"
                    fi
                elif echo "$output" | grep -q "$expected_msg"; then
                    passed=1
                else
                    reason="Expected error message containing '$expected_msg', got: $output"
                fi
            else
                passed=1
            fi
        else
            reason="Expected failure (non-zero), got exit code 0"
        fi
    else
        reason="Unknown expected status: $expected_status"
    fi

    if [ $passed = 1 ]; then
        n_passed=$((n_passed + 1))
        printf "${GREEN}[PASS]${RESET_COLOR} %s - %s\n" "$mode" "$input_file"
    else
        printf "${RED}[FAIL]${RESET_COLOR} %s - %s\n" "$mode" "$input_file"
        echo "  Reason: $reason"
        if [ "$expected_status" = "success" ]; then
             echo "  Output: $output"
        fi
    fi
done < "$spec_file"

echo ""
echo "Summary: $n_passed / $total tests passed"

if [ $n_passed -eq $total ]; then
    exit 0
else
    exit 1
fi
