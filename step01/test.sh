#!/bin/bash
# step01/test.sh

# Get the directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Build inside the step directory
pushd "$DIR" > /dev/null
dune build src/main.exe || exit 1
popd > /dev/null

assert() {
  expected="$1"
  input="$2"

  # Run dune exec with the correct root
  echo "$input" | dune exec --root "$DIR" src/main.exe > tmp.s || exit 1
  cc -o tmp tmp.s || exit 1
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 0
assert 42 42

echo OK
rm -f tmp tmp.s
