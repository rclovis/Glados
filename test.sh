#!/bin/bash

stack build --silent


passed=0;
failed=0;

# Test scenarios
function test_execution() {
    input=$1
    expected_output=$2

    # Run the Haskell program with input and redirect stdout to a file
    stack run $1 > output.txt

    # Compare the output file with the expected result
    if diff -q output.txt <(echo "$expected_output"); then
        echo "Test passed: Input $input => Output $expected_output"
    else
        echo "Test failed: Input $input => Expected $expected_output, Got $(cat output.txt)"
    fi

    # Clean up
    rm output.txt
}
 
function test_compil() {
    input=$1
    expected_output=$2

    # Run the Haskell program with input and redirect stdout to a file
    stack run $1

    # Compare the output file with the expected result
    if diff -q out.bin new.bin ; then
        echo "Test passed: Input $input"
    else
        echo "Test failed: Input $input => Binary files differ"
    fi

    # Clean up
    rm out.bin
}

function test_all() {
    file=$1
    stack run $file > /dev/null
    stack run vm-exe out.bin > output.txt

    if diff -q output.txt $2 ; then
        echo "Test passed: $3"
        ((passed++))
    else
        echo "Test failed:
        expected:
            $(cat $2)
        got:
            $(cat output.txt)"
        ((failed++))
    fi
    
    rm out.bin
    rm output.txt
    echo "----------------------------------------"
}

# Test cases
test_all "Ftests/Input1" "Ftests/Output1" "Simple putstr"

echo "Tested: $((passed + failed)) tests"
echo "Passed: $passed"
echo "Failed: $failed"